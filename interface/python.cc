/*
 * Copyright 2011,2015 Sven Verdoolaege. All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 * 
 *    2. Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY SVEN VERDOOLAEGE ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SVEN VERDOOLAEGE OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * The views and conclusions contained in the software and documentation
 * are those of the authors and should not be interpreted as
 * representing official policies, either expressed or implied, of
 * Sven Verdoolaege.
 */ 

#include "isl_config.h"

#include <stdio.h>
#include <stdarg.h>
#include <iostream>
#include <map>
#include <vector>
#include "python.h"

/* Drop the "isl_" initial part of the type name "name".
 */
static string type2python(string name)
{
	return name.substr(4);
}

/* Print the header of the method "name" with "n_arg" arguments.
 * If "is_static" is set, then mark the python method as static.
 *
 * If the method is called "from", then rename it to "convert_from"
 * because "from" is a python keyword.
 */
static void print_method_header(bool is_static, const string &name, int n_arg)
{
	const char *s;

	if (is_static)
		printf("    @staticmethod\n");

	s = name.c_str();
	if (name == "from")
		s = "convert_from";

	printf("    def %s(", s);
	for (int i = 0; i < n_arg; ++i) {
		if (i)
			printf(", ");
		printf("arg%d", i);
	}
	printf("):\n");
}

/* Reserved words we should not use.
 */
static const char *keywords[] = {"and", "for", "if", "in", "or", "print", 0};

static string handlereservedname(string pname) {
	for (const char **p = keywords; *p; ++p) {
		if (pname.compare(*p) == 0) {
			pname += "_";
			break;
		}
	}
	return pname;
}

/* Print python class for an isl enum type.
 */
void python_generator::print_enum(const isl_enum &enu)
{
	string enum_name_str = type2python(enu.name);
	enum_name_str = handlereservedname(enum_name_str);
	const char *enum_name = enum_name_str.c_str();

	printf("class %s:\n", enum_name);
	printf("    def __init__(self,name,value):\n");
	printf("        self.name  = name\n");
	printf("        self.value = value\n");
	printf("    def __str__(self):\n");
	printf("        return self.name\n");
	printf("    def __repr__(self):\n");
	printf("        return \"<isl.%s.%%s: %%d>\" %% "
	       "(self.name, self.value)\n", enum_name);
	printf("\n");

	map<string,int>::const_iterator it, e = enu.values.end();
	for (it	= enu.values.begin(); it != e; ++it) {
		string name_str = enu.name_without_enum(it->first);
		name_str = handlereservedname(name_str);
		const char *name = name_str.c_str();
		printf("%s.%s = %s(\"%s\",%d)\n", enum_name, name,
		       enum_name, name, it->second);
	}
	printf("\n");
}

/* Print a constructor with a named static method.
 */
bool python_generator::constructorShouldBeNamed(const isl_class &clazz,
						FunctionDecl *cons)
{
	const string fullname = cons->getName();
	return fullname.compare("isl_equality_alloc") == 0 ||
		fullname.compare("isl_inequality_alloc") == 0;
}

/* Construct a wrapper for a callback argument (at position "arg").
 * Assign the wrapper to "cb".  We assume here that a function call
 * has at most one callback argument.
 *
 * The wrapper converts the arguments of the callback to python types.
 * If any exception is thrown, the wrapper keeps track of it in exc_info[0]
 * and returns -1.  Otherwise the wrapper returns 0.
 */
void python_generator::print_callback(QualType type, int arg)
{
	const FunctionProtoType *fn = type->getAs<FunctionProtoType>();
	unsigned n_arg = fn->getNumArgs();

	printf("        exc_info = [None]\n");
	printf("        fn = CFUNCTYPE(c_int");
	for (int i = 0; i < n_arg - 1; ++i) {
		QualType arg_type = fn->getArgType(i);
		assert(is_isl_type(arg_type));
		printf(", c_void_p");
	}
	printf(", c_void_p)\n");
	printf("        def cb_func(");
	for (int i = 0; i < n_arg; ++i) {
		if (i)
			printf(", ");
		printf("cb_arg%d", i);
	}
	printf("):\n");
	for (int i = 0; i < n_arg - 1; ++i) {
		string arg_type;
		arg_type = type2python(extract_type(fn->getArgType(i)));
		printf("            cb_arg%d = %s(ctx=arg0.ctx, "
			"ptr=cb_arg%d)\n", i, arg_type.c_str(), i);
	}
	printf("            try:\n");
	printf("                arg%d(", arg);
	for (int i = 0; i < n_arg - 1; ++i) {
		if (i)
			printf(", ");
		printf("cb_arg%d", i);
	}
	printf(")\n");
	printf("            except:\n");
	printf("                import sys\n");
	printf("                exc_info[0] = sys.exc_info()\n");
	printf("                return -1\n");
	printf("            return 0\n");
	printf("        cb = fn(cb_func)\n");
}

/* Print the argument at position "arg" in call to "fd".
 * "skip" is the number of initial arguments of "fd" that are
 * skipped in the Python method.
 *
 * If the argument is a callback, then print a reference to
 * the callback wrapper "cb".
 * Otherwise, if the argument is marked as consuming a reference,
 * then pass a copy of the the pointer stored in the corresponding
 * argument passed to the Python method.
 * Otherwise, if the argument is a pointer, then pass this pointer itself.
 * Otherwise, pass the argument directly.
 */
void python_generator::print_arg_in_call(const isl_class &clazz,
	FunctionDecl *fd, int arg, int skip)
{
	ParmVarDecl *param = fd->getParamDecl(arg);
	QualType type = param->getOriginalType();
	int idx = arg - skip;

	if (is_isl_enum(type))
		printf(", arg%d.value", idx);
	else if (is_callback(type)) {
		printf("cb");
	} else if (is_isl_class(type)) {
		if (takes(param)) {
			string type_s = extract_type(type);
			if (clazz.name.compare("isl_printer") == 0)
				printf("arg0.makePtr0()");
			else
				printf(", isl.%s_copy(arg%d.ptr)",
				       type_s.c_str(), idx);
		} else
			printf(", arg%d.ptr", idx);
	} else
		printf(", arg%d", idx);
}

static string methodname2python(const isl_class &clazz,
	const string &fullname)
{
	string pname = clazz.name_without_class(fullname);
	return handlereservedname(pname);
}

/* Print a python method corresponding to the C function "method".
 * "super" contains the superclasses of the class to which the method belongs.
 *
 * If the first argument of "method" is something other than an instance
 * of the class, then mark the python method as static.
 * If, moreover, this first argument is an isl_ctx, then remove
 * it from the arguments of the Python method.
 *
 * If the function has a callback argument, then it also has a "user"
 * argument.  Since Python has closures, there is no need for such
 * a user argument in the Python interface, so we simply drop it.
 * We also create a wrapper ("cb") for the callback.
 *
 * For each argument of the function that refers to an isl structure,
 * including the object on which the method is called,
 * we check if the corresponding actual argument is of the right type.
 * If not, we try to convert it to the right type.
 * It that doesn't work and if subclass is set, we try to convert self
 * to the type of the first superclass in "super" and
 * call the corresponding method.
 *
 * If the function consumes a reference, then we pass it a copy of
 * the actual argument.
 *
 * If the return type is isl_bool, then convert the result to
 * a Python boolean, raising an error on isl_bool_error.
 */
void python_generator::print_method(const isl_class &clazz,
	FunctionDecl *method, vector<string> super)
{
	string fullname = method->getName();
	string cname = methodname2python(clazz, fullname);
	int num_params = method->getNumParams();
	int drop_user = has_user_pointer(method) ? 1 : 0;;
	int drop_ctx = first_arg_is_isl_ctx(method);


	print_method_header(is_static(clazz, method), cname,
			    num_params - drop_ctx - drop_user);

	for (int i = drop_ctx; i < num_params; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		string type;
		if (!is_isl_type(param->getOriginalType()))
			continue;
		type = type2python(extract_type(param->getOriginalType()));
		printf("        try:\n");
		printf("            if not arg%d.__class__ is %s:\n",
			i - drop_ctx, type.c_str());
		printf("                arg%d = %s(arg%d)\n",
			i - drop_ctx, type.c_str(), i - drop_ctx);
		printf("        except:\n");
		if (!drop_ctx && i > 0 && super.size() > 0) {
			printf("            return %s(arg0).%s(",
				type2python(super[0]).c_str(), cname.c_str());
			for (int i = 1; i < num_params - drop_user; ++i) {
				if (i != 1)
					printf(", ");
				printf("arg%d", i);
			}
			printf(")\n");
		} else
			printf("            raise\n");
	}
	for (int i = 1; i < num_params; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		QualType type = param->getOriginalType();
		if (!is_callback(type))
			continue;
		print_callback(type->getPointeeType(), i - drop_ctx);
	}
	if (drop_ctx)
		printf("        ctx = Context.getDefaultInstance()\n");
	else
		printf("        ctx = arg0.ctx\n");
	printf("        res = isl.%s(", fullname.c_str());
	if (drop_ctx)
		printf("ctx");
	else
		print_arg_in_call(clazz, method, 0, 0);
	for (int i = 1; i < num_params - drop_user; ++i) {
		printf(", ");
		print_arg_in_call(clazz, method, i, drop_ctx);
	}
	if (drop_user)
		printf(", None");
	printf(")\n");

	QualType rettype = method->getReturnType();
	if (is_isl_class(rettype)) {
		string type;
		type = type2python(extract_type(method->getReturnType()));
		printf("        return %s(ctx=ctx, ptr=res)\n",
			type.c_str());
	} else if (is_string(rettype)) {
		printf("        strres = str(cast(res, c_char_p).value)\n");
		if (gives(method))
			printf("        libc.free(res)\n");
		printf("        return strres\n");
	} else {
		if (drop_user) {
			printf("        if exc_info[0] != None:\n");
			printf("            raise exc_info[0][0], "
				"exc_info[0][1], exc_info[0][2]\n");
		}
		if (is_isl_bool(method->getReturnType())) {
			printf("        if res < 0:\n");
			printf("            raise\n");
			printf("        return bool(res)\n");
		} else {
			printf("        return res\n");
		}
	}
}

/* Print part of an overloaded python method corresponding to the C function
 * "method".
 * "super" contains the superclasses of the class to which the method belongs.
 *
 * In particular, print code to test whether the arguments passed to
 * the python method correspond to the arguments expected by "method"
 * and to call "method" if they do.
 */
void python_generator::print_method_overload(const isl_class &clazz,
	FunctionDecl *method, vector<string> super)
{
	string fullname = method->getName();
	int num_params = method->getNumParams();
	int first;
	string type;

	first = is_static(clazz, method) ? 0 : 1;

	printf("        if ");
	for (int i = first; i < num_params; ++i) {
		if (i > first)
			printf(" and ");
		ParmVarDecl *param = method->getParamDecl(i);
		if (is_isl_type(param->getOriginalType())) {
			string type;
			type = extract_type(param->getOriginalType());
			type = type2python(type);
			printf("arg%d.__class__ is %s", i, type.c_str());
		} else
			printf("type(arg%d) == str", i);
	}
	printf(":\n");
	printf("            res = isl.%s(", fullname.c_str());
	print_arg_in_call(clazz, method, 0, 0);
	for (int i = 1; i < num_params; ++i) {
		printf(", ");
		print_arg_in_call(clazz, method, i, 0);
	}
	printf(")\n");
	type = type2python(extract_type(method->getReturnType()));
	printf("            return %s(ctx=arg0.ctx, ptr=res)\n", type.c_str());
}

/* Print a python method with a name derived from "fullname"
 * corresponding to the C functions "methods".
 * "super" contains the superclasses of the class to which the method belongs.
 *
 * If "methods" consists of a single element that is not marked overloaded,
 * the use print_method to print the method.
 * Otherwise, print an overloaded method with pieces corresponding
 * to each function in "methods".
 */
void python_generator::print_method(const isl_class &clazz,
	const string &fullname, const set<FunctionDecl *> &methods,
	vector<string> super)
{
	string cname;
	set<FunctionDecl *>::const_iterator it;
	int num_params;
	FunctionDecl *any_method;

	any_method = *methods.begin();
	if (methods.size() == 1 && !is_overload(any_method)) {
		print_method(clazz, any_method, super);
		return;
	}

	cname = fullname.substr(clazz.name.length() + 1);
	num_params = any_method->getNumParams();

	print_method_header(is_static(clazz, any_method), cname, num_params);

	for (it = methods.begin(); it != methods.end(); ++it)
		print_method_overload(clazz, *it, super);
}


/* Print the call to a constructor method.
 *
 * The printed code uses "ctxVar" to refer to the isl context
 * and the result of the contructor invocation is stored in
 * the variable named by "resultVar".
 */
void python_generator::print_constructor_call(const isl_class &clazz,
	FunctionDecl *cons, const string &ctxVar, const string &resultVar)
{
	string fullname = cons->getName();
	int num_params = cons->getNumParams();
	int drop_ctx = first_arg_is_isl_ctx(cons);
	int drop_user = has_user_pointer(cons) ? 1 : 0;

	printf("        if len(args) == %d", num_params-drop_ctx-drop_user);
	for (int i = drop_ctx; i < num_params - drop_user; ++i) {
		ParmVarDecl *param = cons->getParamDecl(i);
		QualType type = param->getOriginalType();
		if (is_isl_type(type)) {
			string s;
			s = type2python(extract_type(type));
			printf(" and args[%d].__class__ is %s",
				i - drop_ctx, s.c_str());
		} else if (type->isPointerType()) {
			printf(" and type(args[%d]) == str", i - drop_ctx);
		} else {
			printf(" and type(args[%d]) == int", i - drop_ctx);
		}
	}
	printf(":\n");
	printf("            %s = isl.%s(", resultVar.c_str(), fullname.c_str());
	if (drop_ctx)
		printf("%s", ctxVar.c_str());
	for (int i = drop_ctx; i < num_params - drop_user; ++i) {
		ParmVarDecl *param = cons->getParamDecl(i);
		if (i)
			printf(", ");
		QualType ty = param->getOriginalType();
		if (is_isl_class(ty)) {
			if (takes(param)) {
				string type;
				type = extract_type(ty);
				printf("isl.%s_copy(args[%d].ptr)",
					type.c_str(), i - drop_ctx);
			} else
				printf("args[%d].ptr", i - drop_ctx);
		} else if (is_isl_enum(ty))
			printf("args[%d].value", i - drop_ctx);
		else
			printf("args[%d]", i - drop_ctx);
	}
	if (drop_user > 0)
		printf(", None");
	printf(")\n");
}

/* Print part of the constructor for this isl_class.
 *
 * In particular, check if the actual arguments correspond to the
 * formal arguments of "cons" and if so call "cons" and put the
 * result in self.ptr and a reference to the default context in self.ctx.
 *
 * If the function consumes a reference, then we pass it a copy of
 * the actual argument.
 */
void python_generator::print_constructor(const isl_class &clazz,
					 FunctionDecl *cons)
{
	printf("        self.ctx = Context.getDefaultInstance()\n");
	print_constructor_call(clazz, cons, "self.ctx", "self.ptr");
	printf("            return\n");
}

/* Print the header of the class "name" with superclasses "super".
 */
static void print_class_header(const string &name, const vector<string> &super)
{
	printf("class %s", name.c_str());
	if (super.size() > 0) {
		printf("(");
		for (int i = 0; i < super.size(); ++i) {
			if (i > 0)
				printf(", ");
			printf("%s", type2python(super[i]).c_str());
		}
		printf(")");
	}
	printf(":\n");
}

/* Tell ctypes about the return type of "fd".
 * In particular, if "fd" returns a pointer to an isl object,
 * then tell ctypes it returns a "c_void_p".
 * Similarly, if "fd" returns an isl_bool,
 * then tell ctypes it returns a "c_bool".
 */
void python_generator::print_restype(FunctionDecl *fd)
{
	string fullname = fd->getName();
	QualType type = fd->getReturnType();
	if (is_isl_type(type))
		printf("isl.%s.restype = c_void_p\n", fullname.c_str());
	else if (is_isl_bool(type))
		printf("isl.%s.restype = c_bool\n", fullname.c_str());
	else if (is_string(type))
		printf("isl.%s.restype = POINTER(c_char)\n",
		       fullname.c_str());
}

/* Tell ctypes about the types of the arguments of the function "fd".
 */
void python_generator::print_argtypes(FunctionDecl *fd)
{
	string fullname = fd->getName();
	int n = fd->getNumParams();
	int drop_user = 0;

	printf("isl.%s.argtypes = [", fullname.c_str());
	for (int i = 0; i < n - drop_user; ++i) {
		ParmVarDecl *param = fd->getParamDecl(i);
		QualType type = param->getOriginalType();
		if (is_callback(type))
			drop_user = 1;
		if (i)
			printf(", ");
		if (is_isl_ctx(type))
			printf("Context");
		else if (is_isl_type(type) || is_callback(type))
			printf("c_void_p");
		else if (is_string(type))
			printf("c_char_p");
		else if (type->isPointerType() &&
		         type->getPointeeType()->isVoidType())
			printf("c_void_p");
		else
			printf("c_int");
	}
	if (drop_user)
		printf(", c_void_p");
	printf("]\n");
}

/* Print a constructor as a named static method. */
void python_generator::print_named_constructor(const isl_class &clazz,
					       FunctionDecl *cons)
{
	const string fullname = cons->getName();
	const string cname = clazz.name_without_class(fullname);
	const string p_name = type2python(clazz.name);

	printf("    @staticmethod\n");
	printf("    def %s(*args):\n", cname.c_str());
	printf("        ctx = Context.getDefaultInstance()\n");
	print_constructor_call(clazz, cons, "ctx", "ptr");
	printf("            return %s(ctx=ctx,ptr=ptr)\n", p_name.c_str());
	printf("        raise Error\n");
}

/* Print out the definition of this isl_class.
 *
 * We first check if this isl_class is a subclass of one or more other classes.
 * If it is, we make sure those superclasses are printed out first.
 *
 * Then we print a constructor with several cases, one for constructing
 * a Python object from a return value and one for each function that
 * was marked as a constructor.
 *
 * Next, we print out some common methods and the methods corresponding
 * to functions that are not marked as constructors.
 *
 * Finally, we tell ctypes about the types of the arguments of the
 * constructor functions and the return types of those function returning
 * an isl object.
 */
void python_generator::print(const isl_class &clazz)
{
	// We do not generate the isl_ctx class here (yet).
	if (clazz.is_ctx())
		return;

	const string &name = clazz.name;
	string p_name = type2python(name);
	set<FunctionDecl *>::iterator in;
	map<string, set<FunctionDecl *> >::iterator it;
	vector<string> super = find_superclasses(clazz.type);

	for (int i = 0; i < super.size(); ++i)
		if (done.find(super[i]) == done.end())
			print(classes[super[i]]);
	done.insert(name);

	printf("\n");
	print_class_header(p_name, super);
	printf("    def __init__(self, *args, **keywords):\n");

	printf("        if \"ptr\" in keywords:\n");
	printf("            self.ctx = keywords[\"ctx\"]\n");
	printf("            self.ptr = keywords[\"ptr\"]\n");
	printf("            return\n");

	for (in = clazz.constructors.begin(); in != clazz.constructors.end();
	     ++in) {
		if (!constructorShouldBeNamed(clazz, *in))
			print_constructor(clazz, *in);
	}
	printf("        raise Error\n");
	if (!is_inplace(clazz)) {
		printf("    def __del__(self):\n");
		printf("        if hasattr(self, 'ptr'):\n");
		printf("            isl.%s_free(self.ptr)\n", name.c_str());
	}
	if (can_be_printed(clazz)) {
		printf("    def __str__(self):\n");
		printf("        ptr = isl.%s_to_str(self.ptr)\n", name.c_str());
		printf("        res = str(cast(ptr, c_char_p).value)\n");
		printf("        libc.free(ptr)\n");
		printf("        return res\n");
		printf("    def __repr__(self):\n");
		printf("        s = str(self)\n");
		printf("        if '\"' in s:\n");
		printf("            return 'isl.%s(\"\"\"%%s\"\"\")' %% s\n",
		       p_name.c_str());
		printf("        else:\n");
		printf("            return 'isl.%s(\"%%s\")' %% s\n",
		       p_name.c_str());
		printf("    def makePtr0(self):\n");
		printf("        ptr = self.ptr\n");
		printf("        delattr(self, 'ptr')\n");
		printf("        return ptr;\n");
	}

	for (auto &MethodKV : clazz.methods)
		print_method(clazz, MethodKV.first, MethodKV.second, super);
	for (in = clazz.constructors.begin(); in != clazz.constructors.end();
	     ++in) {
		if (constructorShouldBeNamed(clazz, *in))
			print_named_constructor(clazz, *in);
	}

	printf("\n");
	for (in = clazz.constructors.begin(); in != clazz.constructors.end(); ++in) {
		print_restype(*in);
		print_argtypes(*in);
	}
	for (auto &MethodKV : clazz.methods) {
		for (auto &method : MethodKV.second) {
			print_restype(method);
			print_argtypes(method);
		}
	}
	printf("isl.%s_free.argtypes = [c_void_p]\n", name.c_str());
	printf("isl.%s_to_str.argtypes = [c_void_p]\n", name.c_str());
	printf("isl.%s_to_str.restype = POINTER(c_char)\n", name.c_str());
}

python_generator::python_generator(set<RecordDecl *> &types,
				   set<FunctionDecl *> &functions,
				   set<EnumDecl *> &enums)
    : generator(types, functions, enums), os(outputfile("isl.py"))
{
}

/* Generate a python interface based on the extracted types and functions.
 * We first collect all functions that belong to a certain type,
 * separating constructors from regular methods.  If there are any
 * overloaded functions, then they are grouped based on their name
 * after removing the argument type suffix.
 *
 * Then we print out each class in turn.  If one of these is a subclass
 * of some other class, it will make sure the superclass is printed out first.
 */
void python_generator::generate()
{
	map<string, isl_class> classes;
	map<string, isl_enum>::const_iterator ei;
	map<string, isl_class>::iterator ci;
	done.clear();

	for (ei = enums.begin(); ei != enums.end(); ++ei)
		print_enum(ei->second);

	for (ci = classes.begin(); ci != classes.end(); ++ci) {
		if (done.find(ci->first) == done.end())
			print(ci->second);
	}
}

/* The print* methods call "printf". Intercept these
 * calls here and forward the strings printed to
 * the output stream for the generated file.
 */
void python_generator::printf(const char *fmt, ...)
{
	va_list ap;
	char *ptr;
	size_t size;
	FILE *f = open_memstream(&ptr, &size);

	if (f == NULL) {
		cerr << "Oops, could not open memstream." << endl;
		exit(1);
	}
	va_start(ap, fmt);
	vfprintf(f, fmt, ap);
	fclose(f);
	os << ptr;
}
