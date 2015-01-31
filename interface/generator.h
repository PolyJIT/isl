#ifndef ISL_INTERFACE_GENERATOR_H
#define ISL_INTERFACE_GENERATOR_H

#include <clang/AST/Decl.h>
#include <map>
#include <set>
#include <string>

using namespace clang;
using namespace std;

/* isl_class collects all constructors and methods for an isl "class".
 * "name" is the name of the class.
 * "type" is the declaration that introduces the type.
 * "methods" contains the set of methods, grouped by method name.
 */
struct isl_class {
	string name;
	RecordDecl *type;
	set<FunctionDecl *> constructors;
	map<string, set<FunctionDecl *> > methods;

	bool is_static(FunctionDecl *method);

	void print(map<string, isl_class> &classes, set<string> &done);
	void print_constructor(FunctionDecl *method);
	void print_method(FunctionDecl *method, vector<string> super);
	void print_method_overload(FunctionDecl *method, vector<string> super);
	void print_method(const string &fullname,
		const set<FunctionDecl *> &methods, vector<string> super);
};

vector<string> find_superclasses(RecordDecl *decl);
string drop_type_suffix(string name, FunctionDecl *method);
bool is_overload(Decl *decl);
bool is_isl_bool(QualType type);
bool is_subclass(RecordDecl *decl, string &super);
bool is_constructor(Decl *decl);
bool is_static(const isl_class &clazz, FunctionDecl *method);
bool takes(Decl *decl);
isl_class &method2class(map<string, isl_class> &classes, FunctionDecl *fd);
bool is_isl_ctx(QualType type);
bool first_arg_is_isl_ctx(FunctionDecl *fd);
bool is_isl_type(QualType type);
bool is_callback(QualType type);
bool is_string(QualType type);
string extract_type(QualType type);

#endif
