#ifndef ISL_INTERFACE_CPP_H
#define ISL_INTERFACE_CPP_H

#include "generator.h"

class cpp_generator : public generator {
public:
  cpp_generator(set<RecordDecl *> &types, set<FunctionDecl *> &functions,
                set<EnumDecl *> &enums);

  virtual void generate(); // overrides method in base class

private:
  set<string> sources;
  set<string> headers;

  void generateClasses();
  void generateEnums();
  void generateMakefile();
  void generateConfigure();

  set<pair<string, bool> > getDependencies(isl_class &clazz);
  const string getIncludes(isl_class &clazz, bool impl);

  void print_isl_obj_class();
  void print_isl_exception_class();
  void print_function_ptr_helper();

  bool can_copy(isl_class &clazz);

  void insertIfDependency(QualType &argTy);
  void insertIfDependency(string p_name, set<pair<string, bool> > &Deps,
                          QualType &Ty);

  void prepare_argument(ostream &os, const ParmVarDecl *param);
  void print_argument(ostream &os, ParmVarDecl *param);
  void handle_result_argument(ostream &os, const string &ctx,
                              const ParmVarDecl *param);
  void handle_enum_return(ostream &os, const string &res, const isl_enum &enu);
  void handle_return(ostream &os, FunctionDecl *method,
                     const string &resVar);
  void print_method(ostream &os, isl_class &clazz, FunctionDecl *method,
                    bool subclass, string super);
  void print_method_impl(ostream &os, isl_class &clazz, FunctionDecl *method,
                         bool subclass, string super);
  void print_constructor(ostream &os, isl_class &clazz, FunctionDecl *cons);
  void print_constructor_impl(ostream &os, isl_class &clazz,
			      FunctionDecl *cons);
  void print_class(isl_class &clazz);
  void print_class_impl(isl_class &clazz);

  void print_enum(const isl_enum &enu);

  string get_argument_decl_list(FunctionDecl *method, int offset = 0);
  string get_argument_list(FunctionDecl *method, int offset = 0);

  string paramtype2jna(QualType ty, bool wrapperTypes = false,
                       bool isBool = false);
  string paramtype2jna(const ParmVarDecl *decl);
  string paramtype2cpp(QualType ty, bool wrapperTypes = false,
                        bool isBool = false);
  string paramtype2cpp(const ParmVarDecl *decl);
  string rettype2jna(const FunctionDecl *method);
  string rettype2cpp(const FunctionDecl *method);
  string methodname2cpp(const isl_class &clazz, const string &methodname);
  string isl_ptr(const string &classname, const string &expression,
                 bool is_takes);

  string cppTypeName(QualType ty);

  bool constructorShouldBeNamed(const isl_class &clazz,
                                const FunctionDecl *cons);
};

#endif // CPP_H
