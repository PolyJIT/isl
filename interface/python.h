#include "generator.h"

using namespace std;
using namespace clang;

class python_generator : public generator {
private:
	set<string> done;

	/* The output stream the python generator writes the
	 * generated code to.
	 */
	ostream &os;

public:
	python_generator(set<RecordDecl *> &types,
			 set<FunctionDecl *> &functions,
			 set<EnumDecl *> &enums);

	virtual void generate() override;

private:
	void print_callback(QualType type, int arg);
	void print(const isl_class &clazz);
	void print_constructor(const isl_class &clazz, FunctionDecl *method);
	void print_method(const isl_class &clazz, FunctionDecl *method,
			  vector<string> super);
	void print_method(const isl_class &clazz, const string &fullname,
			  const set<FunctionDecl *> &methods,
			  vector<string> super);
	void print_method_overload(const isl_class &clazz, FunctionDecl *method,
				   vector<string> super);
	string drop_type_suffix(string name, FunctionDecl *method);
	void print_arg_in_call(FunctionDecl *fd, int arg, int skip);
	void print_restype(FunctionDecl *fd);
	void print_argtypes(FunctionDecl *fd);
	void print_enum(const isl_enum &e);

	/* Redirect calls to "printf" in the print* methods to the
	 * output stream "os"
	 */
	void printf(const char *fmt, ...) __attribute__((format(printf, 2, 3)));
};
