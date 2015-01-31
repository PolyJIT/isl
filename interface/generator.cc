/*
 * Copyright 2011 Sven Verdoolaege. All rights reserved.
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

#include <clang/AST/Attr.h>

#include "extract_interface.h"
#include "generator.h"

/* Collect all functions that belong to a certain type,
 * separating constructors from regular methods and collect all enums.
 */
generator::generator(set<RecordDecl *> &types, set<FunctionDecl *> &functions,
		     set<EnumDecl *> &enums)
{
	set<RecordDecl *>::iterator it;
	for (it = types.begin(); it != types.end(); ++it) {
		RecordDecl *decl = *it;
		string name = decl->getName();
		classes[name].name = name;
		classes[name].type = decl;
	}

	set<FunctionDecl *>::iterator in;
	for (in = functions.begin(); in != functions.end(); ++in) {
		isl_class *c = &method2class(classes, *in);
		if (!c)
			continue;
		if (is_constructor(*in)) {
			c->constructors.insert(*in);
		} else {
			FunctionDecl *method = *in;
			string fullname = method->getName();
			fullname = drop_type_suffix(fullname, method);
			c->methods[fullname].insert(method);
		}
	}

	set<EnumDecl *>::const_iterator ie;
	for (ie = enums.begin(); ie != enums.end(); ++ie) {
		const EnumDecl *edecl = *ie;
		const string name = edecl->getName();
		this->enums[name].name = name;
		EnumDecl::enumerator_iterator vi;

		for (vi = edecl->enumerator_begin();
		     vi != edecl->enumerator_end(); ++vi) {
			const EnumConstantDecl *ecd = *vi;
			this->enums[name].values[ecd->getNameAsString()] =
			    ecd->getInitVal().getSExtValue();
		}
	}
}

/* Return a sequence of the types of which the given type declaration is
 * marked as being a subtype.
 */
vector<string> generator::find_superclasses(RecordDecl *decl)
{
	vector<string> super;

	if (!decl->hasAttrs())
		return super;

	string sub = "isl_subclass";
	size_t len = sub.length();
	AttrVec attrs = decl->getAttrs();
	for (AttrVec::const_iterator i = attrs.begin() ; i != attrs.end(); ++i) {
		const AnnotateAttr *ann = dyn_cast<AnnotateAttr>(*i);
		if (!ann)
			continue;
		string s = ann->getAnnotation().str();
		if (s.substr(0, len) == sub) {
			s = s.substr(len + 1, s.length() - len  - 2);
			super.push_back(s);
		}
	}

	return super;
}

/* If "method" is overloaded, then drop the suffix of "name"
 * corresponding to the type of the final argument and
 * return the modified name (or the original name if
 * no modifications were made).
 */
string generator::drop_type_suffix(string name, FunctionDecl *method)
{
	int num_params;
	ParmVarDecl *param;
	string type;
	size_t name_len, type_len;

	if (!is_overload(method))
		return name;

	num_params = method->getNumParams();
	param = method->getParamDecl(num_params - 1);
	type = extract_type(param->getOriginalType());
	type = type.substr(4);
	name_len = name.length();
	type_len = type.length();

	if (name_len > type_len && name.substr(name_len - type_len) == type)
		name = name.substr(0, name_len - type_len - 1);

	return name;
}

/* Is decl marked as being part of an overloaded method?
 */
bool generator::is_overload(Decl *decl)
{
	return has_annotation(decl, "isl_overload");
}

/* Is "type" the type isl_bool?
 */
bool generator::is_isl_bool(QualType type)
{
	string s;

	if (type->isPointerType())
		return false;

	s = type.getAsString();
	return s == "isl_bool";
}

/* Should "method" be considered to be a static method?
 * That is, is the first argument something other than
 * an instance of the class?
 */
bool generator::is_static(const isl_class &clazz, FunctionDecl *method)
{
	ParmVarDecl *param = method->getParamDecl(0);
	QualType type = param->getOriginalType();

	if (!is_isl_type(type))
		return true;
	return extract_type(type) != clazz.name;
}


generator::~generator()
{
	map<string,ostringstream*>::iterator it;
	for (it = files.begin(); it != files.end(); ++it)
		delete it->second;
}

/* Get or create a stream with the given name.
 */
ostream &generator::outputfile(const string &name)
{
	ostringstream *&os = files[name];
	if (!os)
		os = new ostringstream();
	return *os;
}

/* Create all the directories in the given file name
 * if they do not exist.
 */
static void mk_parent_dirs(const string &name) {
	string::size_type off;
	for (off = name.find('/'); off != name.npos;
	     off = name.find('/', off + 1)) {
		string dirname = name.substr(0, off);

		if (mkdir(dirname.c_str(), 0777) != 0 && errno != EEXIST) {
			cerr << "Error creating directory '"
			     << dirname << "'." << endl;
			exit(1);
		}
	}
}

/* Write out all streams generated by calling outputfile() to the given
 * directory.
 */
void generator::write_generated_files(const string &directory) {
	map<string,ostringstream*>::const_iterator it;
	for (it = files.begin(); it != files.end(); ++it) {
		string filename = directory + "/" + it->first;
		ostringstream *oss = it->second;
		mk_parent_dirs(filename);
		ofstream of(filename.c_str());
		of << oss->str();
		of.close();
		delete oss;
	}
	files.clear();
}


/* Is the given type declaration marked as being a subtype of some other
 * type?  If so, return that other type in "super".
 */
bool generator::is_subclass(RecordDecl *decl, string &super)
{
	if (!decl->hasAttrs())
		return false;

	string sub = "isl_subclass";
	size_t len = sub.length();
	AttrVec attrs = decl->getAttrs();
	for (AttrVec::const_iterator i = attrs.begin(); i != attrs.end(); ++i) {
		const AnnotateAttr *ann = dyn_cast<AnnotateAttr>(*i);
		if (!ann)
			continue;
		string s = ann->getAnnotation().str();
		if (s.substr(0, len) == sub) {
			super = s.substr(len + 1, s.length() - len  - 2);
			return true;
		}
	}

	return false;
}

/* Is decl marked as a constructor?
 */
bool generator::is_constructor(Decl *decl)
{
	return has_annotation(decl, "isl_constructor");
}

/* Is decl marked as consuming a reference?
 */
bool generator::takes(Decl *decl)
{
	return has_annotation(decl, "isl_take");
}

/* Is decl marked as producing a reference?
 */
bool generator::gives(Decl *decl)
{
	return has_annotation(decl, "isl_give");
}


/* Return the class that has a name that matches the initial part
 * of the namd of function "fd".
 */
isl_class &generator::method2class(map<string, isl_class> &classes,
	FunctionDecl *fd)
{
	string best;
	map<string, isl_class>::iterator ci;
	string name = fd->getNameAsString();

	for (ci = classes.begin(); ci != classes.end(); ++ci) {
		if (name.substr(0, ci->first.length()) == ci->first)
			best = ci->first;
	}

	if (best.length() == 0)
		cerr << "Cannot find class for method '" << name << "'."
		     << endl;
	assert(best.length() > 0);

	return classes[best];
}

/* Is "type" the type "isl_ctx *"?
 */
bool generator::is_isl_ctx(QualType type)
{
	if (!type->isPointerType())
		return 0;
	type = type->getPointeeType();
	if (type.getAsString() != "isl_ctx")
		return false;

	return true;
}

/* Is the first argument of "fd" of type "isl_ctx *"?
 */
bool generator::first_arg_is_isl_ctx(FunctionDecl *fd)
{
	ParmVarDecl *param;

	if (fd->getNumParams() < 1)
		return false;

	param = fd->getParamDecl(0);
	return is_isl_ctx(param->getOriginalType());
}

/* Is "type" that of a pointer to a function?
 */
bool generator::is_callback(QualType type)
{
	if (!type->isPointerType())
		return false;
	type = type->getPointeeType();
	return type->isFunctionType();
}

/* Is "type" that of "char *" of "const char *"?
 */
bool generator::is_string(QualType type)
{
	if (type->isPointerType()) {
		string s = type->getPointeeType().getAsString();
		return s == "const char" || s == "char";
	}

	return false;
}

/* Return the name of the type that "type" points to.
 */
string generator::extract_type(QualType type)
{
	if (is_isl_class(type)) {
		const RecordType *rt = dyn_cast<RecordType>(
		    type->getPointeeType().getCanonicalType());
		return rt ? rt->getDecl()->getNameAsString()
			  : type->getPointeeType().getAsString();
	}
	if (is_isl_enum(type)) {
		const EnumType *et =
		    dyn_cast<EnumType>(type.getCanonicalType());
		return et ? et->getDecl()->getNameAsString()
			  : type.getAsString();
	}
	std::cerr << "Cannot extract type for " << type.getAsString()
		  << std::endl;
	assert(0);
}

/* Is "type" that of a pointer to an isl_* structure?
 */
bool generator::is_isl_class(QualType type)
{
	if (!type->isPointerType())
		return false;
	type = type->getPointeeType();
	bool isClass = classes.find(type.getAsString()) != classes.end();
	if (!isClass) {
		const RecordType *rt =
		    dyn_cast<RecordType>(type.getCanonicalType());
		isClass = rt &&
			  classes.find(rt->getDecl()->getNameAsString()) !=
			      classes.end();
	}
	return isClass;
}

/* Is "type" an isl type, i.e., an isl_class or an isl_enum.
*/
bool generator::is_isl_type(QualType type) {
	return is_isl_class(type) || is_isl_enum(type);
}

/* check if it is an enum but not a typedef (i.e., of the form
 * "enum isl_xxx")
 */
static bool is_canonical_enum(map<string, isl_enum> &enums, QualType type)
{
	const EnumType *et = dyn_cast<EnumType>(type.getCanonicalType());
	return et &&
	       enums.find(et->getDecl()->getNameAsString()) != enums.end();
}

/* Extract enum types of the form "isl_xxx" (when it's a typedef)
 * and also of the form "enum isl_xxx".
 */
bool generator::is_isl_enum(QualType type)
{
	bool isEnum = enums.find(type.getAsString()) != enums.end();
	if (!isEnum)
		isEnum = is_canonical_enum(enums, type);
	return isEnum;
}

/* Get the isl_enum that is associated to the given type.
 */
const isl_enum &generator::find_enum(QualType type)
{
	assert(is_isl_enum(type));
	return enums.at(extract_type(type));
}

/* Strip the common prefix of the enum type name and the name of
 * the given value name from the value name.
 * Example: for enum "isl_dim_type" with values
 * "isl_dim_all", "isl_dim_cst", etc. we strip "isl_dim_" from the
 * value names.
 */
string isl_enum::name_without_enum(const string &valname) const
{
	/* Append an underscore to the name of the enum type for
	 * the cases where the whole name is a prefix of the
	 * value names (e.g., "isl_fold_min" as a value of "isl_fold").
	 */
	string name_ = name + "_";
	size_t len = std::min(name_.length(), valname.length());
	size_t pos = 0;
	size_t last;
	while (pos < len && name_[pos] == valname[pos]) {
		if (name_[pos] == '_')
			last = pos;
		++pos;
	}
	return valname.substr(last + 1);
}
