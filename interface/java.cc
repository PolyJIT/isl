#include "isl_config.h"

#include <cctype>
#include <cstdio>
#include <sstream>

#include "generator.h"
#include "java.h"

static const string packagePath = "isl/";
static const string commonHeader =
		"package isl;\n"
		"import isl.*;\n"
		"import java.lang.reflect.Constructor;\n"
		"import com.sun.jna.*;\n";

// The pointer type (Ptr) must be public for JNA to be able
// to instantiate the class.
static const string getPtrTypeDecl(const string &superPtrTy) {
	return "    public static class Ptr extends " +
		superPtrTy + " { public Ptr() { super(); } }";
}

static string name2camelcase(const string &name, bool startUpper) {
	bool mkUpper = startUpper;
	string javaname;
	for (string::const_iterator it=name.begin(); it!=name.end(); ++it) {
		char c = *it;
		if (c == '_')
			mkUpper = true;
		else if (mkUpper) {
			javaname += toupper(c);
			mkUpper = false;
		} else
			javaname += c;
	}
	return javaname;
}

static string enumval2java(const string &valname) {
	size_t n = valname.find("_");
	assert(n != string::npos);
	n = valname.find("_", n+1);
	assert(n != string::npos);
	return name2camelcase(valname.substr(n+1), true);
}

/* Drop the "isl_" initial part of the type name "name".
 */
static string type2java(const string &name)
{
	assert(name.length() >= 4);
	return name2camelcase(name.substr(4), true);
}

/* Get the parameter type of an argument of type "ty" of a Java function
 * representing a given isl function in the JNA interface.
 * When isBool is true, then a pointer to int argument is assumed to
 * denote a boolean output parameter.
 * When wrapperTypes is true, a wrapper class (e.g. "Integer") is returned
 * instead of the underlying primitive type (e.g. "int").
 */
string java_generator::paramtype2jna(QualType ty, bool wrapperTypes, bool isBool) {
	string type;
	if (is_isl_ctx(ty))
		type = "Ctx.Ptr";
	else if (is_isl_result_argument(ty)) {
		type = javaTypeName(ty->getPointeeType()) + ".Ptr[]";
	} else if (is_isl_class(ty)) {
		type = type2java(extract_type(ty)) + ".Ptr";
	} else if (is_isl_enum(ty)) {
		type = "int";
	} else if (is_string(ty))
		type = "String";
	else if (ty->isFunctionPointerType())
		type = "Callback";
	else if (ty->isPointerType()) {
		if (ty->getPointeeType().getAsString().compare("int") == 0)
			type = isBool ? "boolean[]" : "int[]";
		else
			type = "Pointer";
	} else if (ty->isVoidType())
		type = "void";
	else if (ty->isIntegerType()) {
		if (ty.getAsString().compare("long") == 0)
			type = wrapperTypes ? "Long" : "long";
		else
			type = wrapperTypes ? "Integer" : "int";
	} else if (ty.getAsString().compare("double") == 0)
		type = wrapperTypes ? "Double" : "double";
	else {
		cerr << "Unsupported argument type: " << ty.getAsString() << endl;
		exit(1);
	}

	return type;
}

string java_generator::paramtype2jna(const ParmVarDecl *decl) {
	return paramtype2jna(decl->getOriginalType(), false, is_bool(decl));
}

string java_generator::paramtype2java(const ParmVarDecl *decl) {
	return paramtype2java(decl->getOriginalType(), false, is_bool(decl));
}

/* Get the return type of the Java function representing a given isl
 * function in the JNA interface.
 */
string java_generator::rettype2jna(const FunctionDecl *method) {
	return paramtype2jna(method->getReturnType());
}

/* Get the Java type corresponding to a given parameter type
 * of an isl function.
 * When wrapperTypes is true, a wrapper class (e.g. "Integer") is
 * returned instead of the underlying primitive type (e.g. "int").
 */
string java_generator::paramtype2java(QualType type, bool wrapperTypes, bool isBool) {
	if (is_isl_ctx(type)) {
		return "isl.Ctx";
	} else if (is_isl_type(type)) {
		return javaTypeName(type);
	} else if (is_isl_result_argument(type)) {
		return javaTypeName(type->getPointeeType()) + "[]";
	} else if (type->isPointerType()) {
		QualType ptype = type->getPointeeType();
		if (ptype->isFunctionType()) {
			const FunctionProtoType *ft = ptype->getAs<FunctionProtoType>();
			unsigned nArgs = ft->getNumArgs() - 1;  // drop "void *user" argument
			ostringstream os;
			bool nonVoidCB = ! ft->getReturnType()->isIntegerType();
			os << (nonVoidCB ? "Callback" : "VoidCallback") << dec << nArgs << "<";
			for (unsigned i=0; i<nArgs; ++i)
				os << (i > 0 ? "," : "") << paramtype2java(ft->getArgType(i), true);
			if (nonVoidCB)
				os << "," << paramtype2java(ft->getReturnType(), true);
			os << ">";
			return os.str();
		}
	}

	return paramtype2jna(type, wrapperTypes, isBool);
}

/* Get the return type of the Java method corresponding
 * to the given isl function.
 */
string java_generator::rettype2java(const FunctionDecl *method) {
	if (is_bool(method)) {
		return "boolean";
	}
	return paramtype2java(method->getReturnType());
}

static const char *keywords[] = { "void", 0 };

string java_generator::methodname2java(const isl_class &clazz, const string &methodname) {
	const string cname = clazz.name_without_class(methodname);
	string jname = name2camelcase(cname, false);
	for (const char **p = keywords; *p; ++p) {
		if (jname == *p) {
			jname += "_";
			break;
		}
	}
	return jname;
}

string java_generator::isl_ptr(const string &classname, const string &expression, bool is_takes) {
	ostringstream os;

	if (is_takes) {
		if (classname.compare("isl_printer") == 0)
			os << "(" << expression << ").makePtr0()";
		else {
			os << "Impl.isl."
			   << classname << "_copy((" << expression << ").getPtr())";
		}
	} else {
		os << "(" << expression << ").getPtr()";
	}

	return os.str();
}

string java_generator::javaTypeName(QualType ty) {
	return type2java(extract_type(ty));
}


void java_generator::print_additional_val_methods(ostream &os) {
	os << "    // Additional convenience methods" << endl
	   << "    public static Val fromBigInteger(isl.Ctx ctx, java.math.BigInteger i) { return readFromStr(ctx, i.toString()); }" << endl
	   << "    public static Val fromLong(isl.Ctx ctx, long l) { return readFromStr(ctx, Long.toString(l)); }" << endl
	   << "    public static Val fromInt(isl.Ctx ctx, int i) { return readFromStr(ctx, Integer.toString(i)); }" << endl
	   << "    public java.math.BigInteger getNum() {" << endl
	   << "        return new java.math.BigInteger(toString().split(\"/\")[0]);" << endl
	   << "    }" << endl
	   << "    public java.math.BigInteger getDen() {" << endl
	   << "       String[] s = toString().split(\"/\");" << endl
	   << "       return new java.math.BigInteger(s.length == 2 ? s[1] : \"1\");" << endl
	   << "    }" << endl;
}

/* Construct a wrapper for a callback argument (at position "arg").
 * Assign the wrapper to "cb".  We assume here that a function call
 * has at most one callback argument.
 */
void java_generator::print_callback(ostream &os, QualType type, const string &arg)
{
	const FunctionProtoType *fn = type->getAs<FunctionProtoType>();
	unsigned n_arg = fn->getNumArgs();

	bool has_result;
	string res_ty, err_res, ok_res;

	QualType t = fn->getReturnType();
	if (is_isl_class(t)) {
		has_result = true;
		res_ty = paramtype2jna(t, false);
		err_res = "null";
		ok_res = "SHOULD_NEVER_BE_USED";
	} else if (t->isIntegerType()) {
		has_result = false;
		res_ty = "int";
		err_res = "-1";
		ok_res = "0";
	} else {
		cerr << "Error: unhandled result type for callback: " << t.getAsString() << endl;
		exit(1);
	}

	os << "            final Ctx _ctx = this.ctx;" << endl
	   << "            Callback cb = new Callback() {" << endl
	   << "                public " << res_ty << " callback(";
	for (unsigned i = 0; i < n_arg - 1; ++i) {
		QualType arg_type = fn->getArgType(i);
		assert(is_isl_type(arg_type));
		os << paramtype2jna(arg_type)
		   << " cb_arg" << dec << i << ", ";
	}
	os << "Pointer _user) {" << endl;
	os << "                    " << res_ty << " res = " << err_res << ";" << endl;
	os << "                    try {" << endl;
	os << "                        "
	   << (has_result ? "res = " : "") << arg << ".apply(";
	for (unsigned i = 0; i < n_arg - 1; ++i) {
		if (i > 0)
			os << ", ";
		os << "new " << type2java(extract_type(fn->getArgType(i)))
		   << "(_ctx, cb_arg" << dec << i << ")";
	}
	os << ")" << (is_isl_class(t) ? ".getPtr()" : "") << ";" << endl;
	if (! has_result)
		os << "                        res = " << ok_res << ";" << endl;
	os << "                    } catch (RuntimeException e) {" << endl
	   << "                        _ctx.setException(e);" << endl
	   << "                    }" << endl
	   << "                    return res;" << endl
	   << "                }" << endl
	   << "            };" << endl;
}

void java_generator::prepare_argument(ostream &os, const ParmVarDecl *param) {
	QualType type = param->getOriginalType();
	const string &name = param->getNameAsString();
	if (is_callback(type)) {
		print_callback(os, type->getPointeeType(), name);
	} else if (is_isl_result_argument(type)) {
		type = type->getPointeeType();
		string javaTyName = javaTypeName(type);
		os << "            assert " << name << " == null || "
		   << name << ".length == 1;" << endl
		   << "        " << javaTyName << ".Ptr[] _" << name
		   << " = " << name << " != null ? new " << javaTyName << ".Ptr[1] : null;" << endl;
	} else if (is_unsigned(type)) {
		os << "            assert " << name << " >= 0;" << endl;
	} else if (is_isl_class(type)) {
		// Make sure the isl object is of the right type,
		// i.e., it matches the compile time type of the
		// parameter (an actual argument for, e.g., isl_union_set
		// could be an isl_set at runtime).
		os << "            " << name << " = " << name << ".as"
		   << javaTypeName(type) << "();" << endl;
	}
}

void java_generator::print_argument(ostream &os, ParmVarDecl *param) {
	const string &name = param->getNameAsString();
	QualType type = param->getOriginalType();
	if (is_callback(type)) {
		os << "cb";
	} else if (is_isl_result_argument(type)) {
		os << "_" << name;
	} else if (is_isl_enum(type))
		os << name  << ".value";
	else if (is_isl_class(type)) {
		string classname = extract_type(type);
		os << isl_ptr(classname, name, takes(param));
	} else if (is_string(type)) {
		os << name;
	} else {
		os << name;
	}
}

void java_generator::handle_result_argument(ostream &os, const string &ctx,
					    const ParmVarDecl *param) {
	const string &name = param->getNameAsString();
	QualType type = param->getOriginalType();
	if (is_isl_result_argument(type)) {
		const string javaTyName = javaTypeName(type->getPointeeType());
		os << "        if (" << name << " != null)" << endl
		   << "            " << name << "[0] = new "
		   << javaTyName << "(" << ctx << ", _" << name << "[0]);" << endl;
	}
}

void java_generator::handle_enum_return(ostream &os, const string &res,
					const isl_enum &enu) {
	os << "        switch(" << res << ") {" << endl;
	map<string,int>::const_iterator it;
	for (it=enu.values.begin(); it!=enu.values.end(); ++it) {
		os << "        case " << it->second << ": return "
		   << type2java(enu.name) << "." << enumval2java(it->first)
		   << ";" << endl;
	}
	os << "        default: throw new IllegalStateException"
	   << "(\"No enum constant in " << type2java(enu.name)
	   << " for value \" + (" << res << ") + \"?\");" << endl
	   << "        }" << endl;
}

void java_generator::handle_return(ostream &os, const FunctionDecl *method,
				   const string &resVar) {
	QualType rettype = method->getReturnType();
	string fullname = method->getName();
	if (is_isl_class(rettype)) {
		string type;
		type = type2java(extract_type(method->getReturnType()));
		os << "        if (" << resVar << " == null)" << endl
		   << "            throw new IslException(\""
		   << fullname << " returned a NULL pointer.\");" << endl
		   << "        return new " << type << "(this.ctx, "
		   << resVar << ");" << endl;
	} else if (is_isl_enum(rettype)) {
		handle_enum_return(os, resVar, find_enum(rettype));
	} else if (is_bool(method)) {
		os << "        if (" << resVar << " == -1)" << endl
		   << "            throw new IslException(\""
		   << fullname << " signaled an error.\");" << endl
		   << "        return " << resVar << " != 0;" << endl;
	} else {
		os << "        return " << resVar << ";" << endl;
	}

}

/* Print a java method corresponding to the C function "method".
 * "subclass" is set if the method belongs to a class that is a subclass
 * of some other class ("super").
 *
 * If the function has a callback argument, then it also has a "user"
 * argument.  Since Java has closures, there is no need for such
 * a user argument in the Java interface, so we simply drop it.
 * We also create a wrapper ("cb") for the callback.
 *
 * For methods with callbacks (which we assume to return 0 or -1) we
 * set the return type of the Java method and the return type of the
 * callback to "void" since errors should be signaled through exceptions.
 *
 * If the function consumes a reference, then we pass it a copy of
 * the actual argument.
 */
void java_generator::print_method(ostream &os, isl_class &clazz, FunctionDecl *method, bool subclass, string super)
{
	string p_name = type2java(clazz.name);
	string fullname = method->getName();
	string cname = methodname2java(clazz, fullname);
	int num_params = method->getNumParams();
	int drop_user = 0;

	for (int i = 1; i < num_params; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		QualType type = param->getOriginalType();
		if (is_callback(type)) {
			if (true /* is_callback_with_user(type) */)
				drop_user = 1;
		}
	}

	os << "    public "
	   << rettype2java(method)
	   << " " << cname << "(";
	for (int i = 1; i < num_params - drop_user; ++i) {
		if (i > 1)
			os << ", ";
		ParmVarDecl *param = method->getParamDecl(i);
		os << (is_callback(param->getOriginalType()) ? "final " : "")
		   << paramtype2java(param)
		   << " " << param->getNameAsString();
	}
	os << ") {" << endl;

	os << "        " << rettype2jna(method) << " res;" << endl;

	os << "        synchronized(this.ctx) {" << endl;

	// Declare variable 'self' which represents 'this' but
	// with the required isl type (i.e., possibly a super type
	// of the actual class).
	os << "            " << p_name << " self = this.as" << p_name
	   << "();" << endl;

	for (int i = 1; i < num_params - drop_user; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		prepare_argument(os, param);
	}
	os << "            res = Impl.isl." << fullname << "("
	   << isl_ptr(clazz.name, "self", takes(method->getParamDecl(0)));
	for (int i = 1; i < num_params - drop_user; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		os << ", ";
		print_argument(os, param);
	}
	if (drop_user)
		os << ", Pointer.NULL";
	os << ");" << endl;

	for (int i = 1; i < num_params - drop_user; ++i) {
		const ParmVarDecl *param = method->getParamDecl(i);
		handle_result_argument(os, "this.ctx", param);
	}
	os << "            this.ctx.checkError();" << endl
	   << "        }" << endl;

	handle_return(os, method, "res");

	os << "    }" << endl;
}

/* Print part of the constructor for this isl_class.
 *
 * When 'asNamedConstructor' is true, generate a static
 * method with a name matching the isl function name
 * (e.g., Set.readFromStr(...)); otherwise, generate
 * a constructor (e.g., Set.Set(...)).
 * To avoid ambiguties, only a few constructor functions
 * can be represented by constructors of the class (because
 * several constructors can have the same argument list, e.g.
 * isl_set_universe and isl_set_empty).
 */
void java_generator::print_constructor(ostream &os, isl_class &clazz, FunctionDecl *cons,
				       bool asNamedConstructor)
{
	const string fullname = cons->getName();
	const string cname = methodname2java(clazz, fullname);
	const string jclass = type2java(clazz.name);
	int ctxArg = -1, ctxSrc = -1;
	int num_params = cons->getNumParams();

	if (!asNamedConstructor)
		os << "    // " << fullname << endl;
	os << "    public ";
	if (asNamedConstructor)
		os << "static ";
	os << type2java(clazz.name);
	if (asNamedConstructor)
		os << " " << cname;
	os << "(";

	// Check if there is an argument which provides us with
	// the isl context.
	for (int i = 0; i < num_params; ++i) {
		QualType ty = cons->getParamDecl(i)->getOriginalType();
		if (is_isl_ctx(ty))
			ctxArg = i;
		else if (is_isl_class(ty))
			ctxSrc = i;
	}

	// When there is no isl class argument that can give us the
	// isl context, there must be an explicit isl context argument.
	if (ctxSrc == -1 && ctxArg == -1) {
		cerr << "Cannot generate binding for '" << fullname << "':" << endl
		     << "  no context argument and no argument to take the context from." << endl;
		exit(1);
	}

	bool firstArg = true;
	for (int i = 0; i < num_params; ++i) {
		if (i == ctxArg && ctxSrc >= 0)  // drop context argument
			continue;

		ParmVarDecl *param = cons->getParamDecl(i);
		if (!firstArg)
			os << ", ";
		else
			firstArg = false;
		const string &pname = param->getNameAsString();
		os <<  paramtype2java(param) << " " << pname;
	}

	os << ") {" << endl;

	const string obj = asNamedConstructor ? "that" : "this";
	if (asNamedConstructor) {
		os << "        " << jclass << " " << obj << " = new "
		   << jclass << "();" << endl;
	}

	if (ctxSrc >= 0) {
		os << "        " << obj << ".ctx = "
		   << cons->getParamDecl(ctxSrc)->getNameAsString()
		   << ".ctx;" << endl;
	} else {
		os << "        " << obj << ".ctx = "
		   << cons->getParamDecl(ctxArg)->getNameAsString()
		   << ";" << endl;
	}

	os << "        synchronized(" << obj << ".ctx) {" << endl;
	for (int i = 0; i < num_params; ++i) {
		if (i == ctxArg && ctxSrc >= 0)
			continue;

		ParmVarDecl *param = cons->getParamDecl(i);
		prepare_argument(os, param);
	}

	os << "            " << obj << ".ptr = Impl.isl." << fullname << "(";
	for (int i = 0; i < num_params; ++i) {
		if (i > 0)
			os << ", ";

		if (i == ctxArg) {
			os << obj << ".ctx.getPtr()";
		} else {
			ParmVarDecl *param = cons->getParamDecl(i);
			print_argument(os, param);
		}
	}
	os << ");" << endl;

	os << "            if (" << obj << ".ptr == null)" << endl
	   << "                throw new IslException(\""
	   << fullname << " returned a NULL pointer.\");" << endl;

	for (int i = 0; i < num_params; ++i) {
		const ParmVarDecl *param = cons->getParamDecl(i);
		handle_result_argument(os, obj + ".ctx", param);
	}

	os << "            " << obj << ".ctx.checkError();" << endl
	   << "        }" << endl;

	if (asNamedConstructor)
		os << "        return " << obj << ";" << endl;
	os << "    }" << endl;
}

/* Print out the definition of this isl_class.
 *
 * We first check if this isl_class is a subclass of some other class.
 * If it is, we make sure the superclass is printed out first.
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
void java_generator::print_class(isl_class &clazz)
{
	const string &name = clazz.name;
	string super;
	string p_name = type2java(name);
	set<FunctionDecl *>::iterator in;
	bool subclass = is_subclass(clazz.type, super);

	ostream &os = outputfile(packagePath + p_name + ".java");
	os << commonHeader;
	os << "public class " << p_name;
	if (subclass)
		os << " extends " << type2java(super);
	os << " {" << endl;
	string superPtr = subclass ? (type2java(super) + ".Ptr")
				   : "PointerType";
	os << getPtrTypeDecl(superPtr) << endl;
	if (!subclass) {
		os << "    protected Ctx ctx;" << endl;
		os << "    protected PointerType ptr;" << endl;
	}

	os << "    " << p_name << "() {}" << endl;

	os << "    " << p_name << "(Ctx ctx, PointerType ptr) {" << endl
	   << "        assert !ptr.getPointer().equals(Pointer.NULL);" << endl
	   << "        this.ctx = ctx;" << endl
	   << "        this.ptr = ptr;" << endl
	   << "    }" << endl;

	os << "   public Ctx getCtx() { return ctx; }" << endl;

	os << "    " << p_name << ".Ptr getPtr() { return (" << p_name
	   << ".Ptr)this.ptr; }" << endl;

	os << "    " << p_name << ".Ptr makePtr0() { " << endl
	   << "        " << p_name << ".Ptr p = ("
	   << p_name << ".Ptr)this.ptr;" << endl
	   << "        this.ptr = new " << p_name << ".Ptr();" << endl
	   << "        return p;" << endl
	   << "    }" << endl;


	for (in = clazz.constructors.begin(); in != clazz.constructors.end(); ++in) {
		// We always print a "named" constructor (i.e., a static function
		// to construct an object).
		// Some isl constructors could be made available as
		// constructors of the generated class but for
		// simplicity and consistency we only generate static
		// member functions.
		print_constructor(os, clazz, *in, true);
	}

	// We do not free objects of classes that have in-place update
	// (e.g., isl_band). These values exist only in dependence of
	// parent objects and are freed when the parent object goes away.
	if (!is_inplace(clazz)) {
		os << "    protected void finalize() {" << endl
		   << "        synchronized(this.ctx) {" << endl
		   << "            Impl.isl." << name << "_free(getPtr());" << endl
		   << "        }" << endl
		   << "    }" << endl;
	}

	if (can_be_printed(clazz)) {
		os << "    public String toString() {" << endl
		   << "        Printer p = Printer.toStr(this.ctx);" << endl
		   << "        p = p.print" << p_name << "(this);" << endl
		   << "        return p.getStr();" << endl
		   << "    }" << endl;
	}

	// Check if runtime type matches compile time type
	// and convert to required type (i.e., change to different
	// isl representation, e.g., from isl_set to isl_union_set).
	//
	os << "    " << p_name << " as" << p_name << "() {" << endl
	   << "        Class clazz = this.getClass();" << endl
	   << "        if (clazz.equals(" << p_name << ".class))" << endl
	   << "            return this;" << endl
	   << "        try {" << endl
	   << "            Constructor<" << p_name << "> c = " << p_name << ".class.getConstructor(clazz);" << endl
	   << "            return c.newInstance(this);" << endl
	   << "        } catch (Exception e) {" << endl
	   << "           throw new IllegalStateException(\"Cannot construct " << p_name << " from \" +" << endl
	   << "               clazz.getName() + \" ?\", e);" << endl
	   << "        }" << endl
	   << "    }" << endl;

	for (in = clazz.methods.begin(); in != clazz.methods.end(); ++in)
		print_method(os, clazz, *in, subclass, super);

	os << "\n";
	if (name.compare("isl_val") == 0)
		print_additional_val_methods(os);

	os << "}" << endl;

	// Add isl_* functions to Impl interface.
	ostream &impl_os = outputfile(packagePath + "Impl.java");
	impl_os << "    void " << name << "_free(" << p_name << ".Ptr islobject);" << endl
		<< "    " << p_name << ".Ptr " << name << "_copy(" << p_name << ".Ptr islobject);" << endl;

	for (in = clazz.constructors.begin(); in != clazz.constructors.end(); ++in) {
		const FunctionDecl *ctor = *in;
		string fullname = ctor->getName();

		impl_os << "    " << p_name << ".Ptr " << fullname << "(";
		for (unsigned i = 0; i < ctor->getNumParams(); ++i) {
			const ParmVarDecl *param = ctor->getParamDecl(i);
			QualType type = param->getOriginalType();
			if (i > 0)
				impl_os << ", ";
			impl_os << paramtype2jna(type) << " "
				<< param->getNameAsString();
		}
		impl_os << ");" << endl;
	}
	for (in = clazz.methods.begin(); in != clazz.methods.end(); ++in) {
		const FunctionDecl *method = *in;
		string fullname = method->getName();

		impl_os << "    " << rettype2jna(*in)
			<< " " << fullname << "(";
		for (unsigned i = 0; i < method->getNumParams(); ++i) {
			const ParmVarDecl *param = method->getParamDecl(i);
			if (i > 0)
				impl_os << ", ";
			impl_os <<  paramtype2jna(param) << " "
				 << param->getNameAsString();
		}
		impl_os << ");" << endl;
	}
}

void java_generator::generate() {
	generateClasses();
	generateEnums();
}


void java_generator::generateClasses() {
	for (unsigned nArgs=1; nArgs<=3; ++nArgs) {
		ostringstream intfname_oss;
		intfname_oss << "Callback" << dec << nArgs;
		const string intfname = intfname_oss.str();
		ostringstream args_oss, tys_oss;
		for (unsigned i=1; i<=nArgs; ++i) {
			if (i > 1) {
				args_oss << ", ";
				tys_oss << ",";
			}
			args_oss << "ArgTy" << dec << i << " arg" << dec << i;
			tys_oss << "ArgTy" << dec << i;
		}
		const string args = args_oss.str();
		const string tys  = tys_oss.str();

		{
			ostream &os = outputfile(packagePath + string("Void") + intfname + string(".java"));
			os << commonHeader
			   << "public interface Void" << intfname << "<" << tys << "> {" << endl
			   << "    void apply(" << args << ");" << endl
			   << "}" << endl;
		}

		{
			ostream &os = outputfile(packagePath /* + string("Ex") */ + intfname + string(".java"));
			os << commonHeader
			   << "public interface " << intfname << "<" << tys
			   << ",RetTy> {" << endl
			   << "    RetTy apply(" << args << ");" << endl
			   << "}" << endl;
		}
	}

	ostream &os_impl = outputfile(packagePath + "Impl.java");
	os_impl << commonHeader
		<< "interface Impl extends Library {" << endl
		<< "    static Impl isl = (Impl)Native.loadLibrary(\"isl\", Impl.class);" << endl
		<< "    Ctx.Ptr isl_ctx_alloc();" << endl
		<< "    int isl_ctx_last_error(Ctx.Ptr ctx);" << endl
		<< "    void isl_ctx_reset_error(Ctx.Ptr ctx);" << endl;

	{
		ostream &os = outputfile(packagePath + "IslException.java");
		os << commonHeader
		   << "public class IslException extends RuntimeException {" << endl
		   << "    public IslException(String msg) { super(msg); }" << endl
		   << "    public IslException(String msg, Throwable cause) { super(msg, cause); }" << endl
		   << "}" << endl;
	}

	{
		ostream &os = outputfile(packagePath + "Ctx.java");
		os << commonHeader
		   << "public class Ctx {" << endl
		   << getPtrTypeDecl("PointerType") << endl
		   << "    private Ptr ctx;" << endl
		   << "    private RuntimeException lastException;" << endl
		   << "    private Ctx() { ctx = Impl.isl.isl_ctx_alloc(); lastException = null; }" << endl
		   << "    public static Ctx alloc() {" << endl
		   << "        return new Ctx();" << endl
		   << "    }" << endl
		   << "    Ptr getPtr() { return ctx; }" << endl
		   << "    void setException(RuntimeException e) {" << endl
		   << "        if (lastException != null)" << endl
		   << "            System.err.println(\"isl bindings warning: two exceptions in a row; exception not handled:\\n\" +" << endl
		   << "                lastException.getMessage());" << endl
		   << "	       lastException = e;" << endl
		   << "	   }" << endl
		   << "    void checkError() {" << endl
		   << "	       RuntimeException e = lastException;" << endl
		   << "        lastException = null;" << endl
		   << "        if (Impl.isl.isl_ctx_last_error(ctx) != 0) { // 0 == isl_error_none" << endl
		   << "            Impl.isl.isl_ctx_reset_error(ctx);" << endl
		   << "            e = new IslException(\"isl error\", e);" << endl
		   << "        }" << endl
		   << "        if (e != null)" << endl
		   << "	           throw e;" <<endl
		   << "     }" << endl
		   << "}" << endl;
	}

	map<string,isl_class>::iterator ci;
	for (ci = classes.begin(); ci != classes.end(); ++ci) {
		print_class(ci->second);
	}

	os_impl << "}" << endl;
}

void java_generator::print_enum(const isl_enum &enu) {
	const string e_name = type2java(enu.name);
	ostream &os = outputfile(packagePath + e_name + ".java");
	os << commonHeader;
	os << "public enum " << e_name << " {" << endl;
	map<string,int>::const_iterator it;
	for (it=enu.values.begin(); it!=enu.values.end(); ++it) {
		os << "    " << enumval2java(it->first) << "("
		   << it->second << ")," << endl;
	}
	os << "    ;" << endl
	   << "    int value;" << endl
	   << "    " << e_name << "(int value) { this.value = value; }" << endl
	   << "}" << endl;
}

void java_generator::generateEnums() {
	map<string,isl_enum>::iterator ei;
	for (ei = enums.begin(); ei != enums.end(); ++ei)
		print_enum(ei->second);
}

java_generator::java_generator(set<RecordDecl *> &types, set<FunctionDecl *> &functions, set<EnumDecl *> &enums)
	: generator(types, functions, enums)
{
}
