#include "generator.h"

using namespace std;
using namespace clang;

class python_generator : public generator {
private:
	set<string> done;

public:
	python_generator(set<RecordDecl *> &types,
			 set<FunctionDecl *> &functions);

	virtual void generate() override;

private:
	void print_callback(QualType type, int arg);
	void print(const isl_class &clazz);
	void print_constructor(const isl_class &clazz, FunctionDecl *method);
	void print_method(const isl_class &clazz, FunctionDecl *method,
			  bool subclass, string super);
};
