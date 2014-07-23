#include "isl_config.h"

#include <cctype>
#include <cstdio>
#include <sstream>

#include "generator.h"
#include "cpp.h"

#define FMT_HEADER_ONLY
#include "cppformat/format.h"

using namespace fmt;

static const string srcPath = "";
static const string includePath = "";
typedef std::map<std::string, std::vector<std::string>> IslDepMap;
static IslDepMap IslExtraDeps = {
    {"isl_union_map", {"isl_aff", "isl_union_map", "isl_flow"}},
    {"isl_qpolynomial", {"isl_polynomial"}},
    {"isl_qpolynomial_fold", {"isl_polynomial"}},
    {"isl_pw_qpolynomial", {"isl_polynomial"}},
    {"isl_pw_qpolynomial_fold", {"isl_polynomial"}},
    {"isl_union_pw_qpolynomial", {"isl_polynomial"}},
    {"isl_union_pw_qpolynomial_fold", {"isl_polynomial"}},
    {"isl_aff_list", {"isl_aff"}},
    {"isl_band_list", {"isl_band"}},
    {"isl_pw_aff_list", {"isl_aff"}},
    {"isl_term", {"isl_polynomial"}},
    {"isl_ast_expr", {"isl_ast"}},
    {"isl_ast_expr_list", {"isl_ast"}},
    {"isl_ast_node", {"isl_ast"}},
    {"isl_ast_node_list", {"isl_ast"}},
    {"isl_ast_print_options", {"isl_ast"}},
    {"isl_basic_set_list", {"isl_set"}},
    {"isl_set_list", {"isl_set"}},
    {"isl_constraint_list", {"isl_constraint"}},
    {"isl_id_list", {"isl_id"}},
    {"isl_val_list", {"isl_val"}},
};

/**
 * @brief Get the necessary include for an isl object
 *
 * @param islName the isl-name of the object
 *
 * @return the include name (without .h suffix).
 */
static const string getIncludeForIslObj(const string &islName) {
  assert(islName.length() >= 4);
  string sName = islName.substr(4);
  string lName;
  for (string::const_iterator it = sName.begin(), ie = sName.end(); it != ie;
       ++it) {
    lName += tolower(*it);
  }

  size_t pos;
  pos = lName.find("basic_");
  if (pos != string::npos)
    return lName.substr(pos + 6);

  if (lName.find("_aff") != string::npos)
    return "aff";
  return lName;
}

/**
 * @brief Get the includes for a given set of dependencies.
 *
 * This function is used to create the include preamble of a .cpp or .h
 * file of the isl bindings.
 *
 * @param deps the header dependencies. The set contains pairs of string and
 *        bool. If the bool is True, we require the full header.
 * @param impl True, if we are a .cpp file, not a header.
 *
 * @return a string containing the include preamble for a .cpp or .h file
 */
static const string getIncludes(set<pair<string, bool>> deps,
                                bool impl = false) {
  string includes;
  set<pair<string, bool>>::iterator i;
  set<pair<string, bool>>::iterator ie;

  for (auto DepPair : deps) {
    if (DepPair.second)
      includes += format("#include \"isl/{0}.h\"\n", DepPair.first);
    else if (impl)
      includes += format("#include \"isl/{0}.hpp\"\n", DepPair.first);
  }

  includes += "#include <string>\n";
  includes += "#include <ostream>\n";

  return includes;
}

/**
 * @brief Get the forward declarations for a given set of dependencies.
 *
 * This function is used to create the forward declarations required in the
 * .cpp/.h file of the isl bindings.
 *
 * @param deps the header dependencies. The set contains pairs of string and
 *        bool. If the bool is True, we require the full header.
 *
 * @return a string containing the forward declarations for a .cpp or .h file
 */
static const string getForwardDecls(set<pair<string, bool>> deps) {
  string forwards;
  set<pair<string, bool>>::iterator i;
  set<pair<string, bool>>::iterator ie;

  for (i = deps.begin(), ie = deps.end(); i != ie; ++i) {
    const pair<string, bool> dep = *i;
    if (!dep.second)
      forwards += "class " + dep.first + ";\n";
  }

  return forwards;
}

/**
 * @brief Indent the outstream by a given depth with spaces.
 *
 * @param os the outstream to indent
 * @param depth the indenting depth (one space)
 *
 * @return the indentend outstream
 */
static ostream &indent(ostream &os, unsigned depth) {
  assert(depth > 0);

  string indent;
  while (depth > 0) {
    indent += " ";
    --depth;
  }

  return os << indent;
}

/**
 * @brief Create a header guard header(!).
 *
 * Creates:
 *  #ifndef ISL_CXX_<CLASSNAME>_H
 *  #define ISL_CXX_<CLASSNAME>_H
 *
 * @param className the class name of the isl object
 *
 * @return a string containing the header guard header.
 */
static const string getGuardHeader(const string className) {
  return format("#ifndef ISL_CXX_{0}_H\n"
                "#define ISL_CXX_{0}_H\n"
                "\n",
                className);
}

/**
 * @brief Create a header guard footer.
 *
 * Creates:
 *  #endif // ISL_CXX_<CLASSNAME>_H
 *
 * @param className the class name of the isl object
 *
 * @return a string containing the header guard footer
 */
static const string getGuardFooter(const string className) {
  return format("#endif //ISL_CXX_{0}_H\n", className);
}

/**
 * @brief Convert an isl name to camel case.
 *
 * This is copied from the java bindings.
 *
 * @param name
 * @param startUpper
 *
 * @return
 */
static string name2camelcase(const string &name, bool startUpper) {
  bool mkUpper = startUpper;
  string cppname;
  for (auto c : name) {
    if (c == '_')
      mkUpper = true;
    else if (mkUpper) {
      cppname += toupper(c);
      mkUpper = false;
    } else
      cppname += c;
  }
  return cppname;
}

/**
 * @brief Return the initials of the name.
 *
 * Initials is a substring of name, containing only upper case letters.
 * Example:
 *  PwAff -> PA
 *
 * @param name the name to get the initials from.
 *
 * @return the initials of the name.
 */
static string name2initials(const string &name) {
  string cCname = name2camelcase(name, true);
  string iname;

  for (string::const_iterator it = cCname.begin(); it != cCname.end(); ++it) {
    char c = *it;
    if (isupper(c))
      iname += c;
  }
  return iname;
}

/**
 * @brief Convert an enum name to a cpp-compatible type name.
 *
 * We cut of the isl_ prefix and convert the rest to a camel-case name.
 *
 * Example:
 *  isl_ast_options -> AstOptions
 *
 * @param valname the name of the enum value
 *
 * @return
 */
static string enumval2cpp(const string &valname) {
  size_t n = valname.find("_");
  assert(n != string::npos);
  n = valname.find("_", n + 1);
  assert(n != string::npos);
  return name2camelcase(valname.substr(n + 1), true);
}

/**
 * @brief Drop the isl_ prefix of a name.
 *
 * This is not actually checking for the prefix, we just drop the first
 * 4 chars.
 *
 * @param name
 *
 * @return
 */
static string type2cpp(const string &name) {
  assert(name.length() >= 4);
  return name2camelcase(name.substr(4), true);
}

static void printHandleError(ostream &os, int level) {
  indent(os, level) << "static inline void handleError(std::string what) {"
                    << endl;
  os << "#ifdef __EXCEPTIONS" << endl;
  indent(os, level + 2) << "throw IslException(what);" << endl;
  os << "#else" << endl;
  indent(os, level + 2) << "std::cerr << what << std::endl;" << endl;
  indent(os, level + 2) << "std::abort();" << endl;
  os << "#endif" << endl;
  os << "}" << endl;
}

static void printHandleErrorCall(ostream &os, int level, string &&what) {
  indent(os, level) << "if (Ctx.hasError()) {" << endl;
  indent(os, level + 2) << "handleError(\"" << what << "\");" << endl;
  indent(os, level) << "}" << endl;
}

/**
 * @brief Print a CPP class.
 *
 * Provide methods to print various parts of a cpp file.
 * This includes:
 *  + Explicit constructors
 *  + Copy constructors
 *  + Copy assignment
 *  + Move constructor
 *  + Move assignment
 *
 *  + Wrapper methods for entering/leaving isl bindings.
 *
 *  + Destructor(s)
 *
 *  + Casts
 *  + Member methods
 */
class cpp_class_printer {
private:
  const isl_class &clazz;
  const bool subclass;
  const bool can_copy;
  const bool is_inplace;
  const string &super;
  const string base_class;
  string name;
  string p_name;

public:
  /**
   * @brief Create a Class printer
   *
   * @param clazz the isl_class to print
   * @param subclass true, if we are a subclass of another isl_class.
   * @param can_copy true, if we can create copies of ourself.
   * @param super our superclass.
   */
  explicit cpp_class_printer(const isl_class &clazz, bool subclass,
                             bool can_copy, const string &super,
                             bool is_inplace)
      : clazz(clazz), subclass(subclass), can_copy(can_copy), super(super),
        base_class((!subclass) ? "IslBase" : type2cpp(super)),
        is_inplace(is_inplace) {
    name = clazz.name;
    p_name = type2cpp(name);
  }

  /**
   * @brief Print declaration of explicit constructors
   *
   * @param os
   */
  void print_explicit_constructors_h(ostream &os) {
    if (can_copy) {
      print(os, "  explicit {}(Context &Ctx, {} *That) : "
                "{}(Ctx, (void *)That) {{/* empty */}}\n",
            p_name, name, base_class);
      print(os, "  explicit {}(Context &Ctx, void *That) : "
                "{}(Ctx, (void *)That) {{/* empty */}}\n",
            p_name, base_class);
    } else {
      print(os, "  explicit {}(Context &Ctx, std::shared_ptr<ptr> That)\n"
                "    : {}(Ctx, nullptr), This(That) {{/* empty */}}\n",
            p_name, base_class);
    }
  }

  /**
   * @brief Print declaration of a copy constructor
   *
   * @param os
   */
  void print_copy_constructor_h(ostream &os) {
    // 2. Copy constructor
    print(os, "  {0}(const {0} &Other) : {1}(Other.ctx(), {2}) {{}}\n", p_name,
          base_class,
          (can_copy) ? "Other.GetCopy()" : "nullptr), This(Other.GetCopy()");
  }

  /**
   * @brief Print definition of the copy assignment operator
   *
   * @param os
   */
  void print_copy_assignment(ostream &os) {
    if (!can_copy)
      return;

    print(os, "inline {0} &{0}::operator=(const {0} &Other) {{\n"
              "  {1} *New = Other.GetCopy();\n"
              "  {1}_free(({1} *)This);\n"
              "  This = New;\n"
              "  return *this;\n"
              "}}",
          p_name, name);
  }

  /**
   * @brief Print declaration of the copy assignment operator
   *
   * @param os
   */
  void print_copy_assignment_h(ostream &os) {
    // 3. Copy assignment
    os << endl;
    if (!can_copy)
      print(os, "  {0} &operator=(const {0} &Other) = delete;\n", p_name);
    else
      print(os, "  {0} &operator=(const {0} &Other);\n", p_name);
  }

  /**
   * @brief Print declaration of the move constructor
   *
   * @param os
   */
  void print_move_constructor_h(ostream &os) {
    // 4. Move constructor
    os << endl;
    if (can_copy)
      print(os, "  {0} ({0} && Other) : {1}(Other.ctx(), Other.Give()) {{}}\n",
            p_name, base_class);
    else
      print(os, "  {0} ({0} && Other) : {1}(Other.ctx(), nullptr), "
                "This(std::move(Other.This)) {{}}\n",
            p_name, base_class);
  }

  /**
   * @brief Print declaration of the move assignment operator
   *
   * @param os
   */
  void print_move_assignment_h(ostream &os) {
    // 5. Move assignment
    os << endl;
    print(os, "  {0} &operator=({0} && Other) {{\n", p_name);
    if (can_copy)
      print(os, "    {0} *New = Other.Give();\n"
                "    {0}_free(({0} *)This);\n"
                "    This = New;\n",
            name);
    else
      print(os, "    std::swap(This, Other.This);\n");
    print(os, "    return *this;\n"
              "  }}\n");
  }

  /**
   * @brief Print definition of the API wrapper
   *
   * @param os
   */
  void print_api_wrapper(ostream &os) {
    if (can_copy) {
      print(os, "inline {0} *{1}::GetCopy() const {{\n"
                "  return {0}_copy(({0} *)This);\n"
                "}}\n",
            name, p_name);
    } else {
      print(os,
            "inline std::shared_ptr<isl::{0}::ptr> {0}::GetCopy() const {{\n"
            "  return {0}::This;\n"
            "}}\n",
            p_name);
    }
  }

  /**
   * @brief Print the declaration of the API wrapper
   *
   * @param os
   */
  void print_api_wrapper_h(ostream &os) {
    // 1. Wrap an isl_* object.
    print(os,
          "  ///@brief Wrap an existing isl object.\n"
          "  ///\n"
          "  /// This serves as an entry point into the C++ API.\n"
          "  /// We take ownership of the isl object.\n"
          "  ///\n"
          "  ///@param That the {0} we want to wrap.\n"
          "  explicit {1}({0} *That) : {1}(Context::get({0}_get_ctx(That)), ",
          name, p_name);

    if (can_copy)
      print(os, "That) {{}}\n");
    else
      print(os, "std::make_shared<isl::{0}::ptr>(That)) {{}}\n", p_name);
    os << endl;

    if (can_copy)
      print(os, "  {0} *GetCopy() const;\n", name);
    else
      print(os, "  std::shared_ptr<isl::{0}::ptr> GetCopy() const;\n", p_name);
  }

  /**
   * @brief Print the definition of the API unwrapper
   *
   * The API Unwrapper returns a _copy_ of the ISL object, if possible.
   *
   * @param os
   */
  void print_api_unwrapper(ostream &os) {
    // Take (gets a copy of the raw pointer).
    os << endl;
    print(os, "///@brief Unwrap the stored isl object.\n"
              "///@return A the wrapped isl object.\n"
              "inline {0} *{1}::Get() const {{"
              "  return ({0} *){2};\n"
              "}}\n",
          name, p_name, (can_copy) ? "This" : "This.get()->p");
  }

  /**
   * @brief Print the declaration of the API unwrapper
   *
   * The API Unwrapper returns a _copy_ of the ISL object, if possible.
   *
   * @param os
   */
  void print_api_unwrapper_h(ostream &os) {
    // keep.
    os << endl;
    print(os, "  ///@brief unwrap the stored isl object.\n"
              "  ///@return a the wrapped isl object.\n"
              "  {0} *Get() const;\n",
          name);
  }

  /**
   * @brief Print the definition of the API Give
   *
   * Returns the wrapped pointer, this exits isl
   *
   * @param os
   */
  void print_api_give(ostream &os) {
    // Generate a Give method (for isl_take arguments). We 'give' the memory,
    // the isl takes it.
    os << endl;
    print(os, "/// @brief Release ownership of the wrapped object.\n"
              "///\n"
              "/// You are on your own now buddy.\n"
              "/// The wrapper cannot be used anymore after calling Give()\n"
              "///\n"
              "///@return the wrapped isl object.\n");
    if (can_copy)
      print(os, "inline {0} *{1}::Give() {{\n"
                "  {0} *res = ({0} *)This;\n"
                "  This = nullptr;\n"
                "  return res;\n"
                "}}\n",
            name, p_name);
    else
      print(os, "inline {0} *{1}::Give() {{\n"
                "  {0} *res = This.get()-p;\n"
                "  This.get()->p = nullptr;\n"
                "  This.reset();\n"
                "  return res;\n"
                "}}\n",
            name, p_name);
  }

  /**
   * @brief Print the declaration of the API Give
   *
   * Returns the wrapped pointer, this exits isl
   *
   * @param os
   */
  void print_api_give_h(ostream &os) {
    // Generate a Give method (for isl_take arguments). We 'give' the memory,
    // the isl takes it.
    os << endl;
    print(os, "  /// @brief Release ownership of the wrapped object.\n"
              "  ///\n"
              "  /// You are on your own now buddy.\n"
              "  /// The wrapper cannot be used anymore after calling Give()\n"
              "  ///\n"
              "  ///@return the wrapped isl object.\n"
              "  {0} *Give();\n",
          name);
  }

  /**
   * @brief Print the definition of a destructor
   *
   * @param os
   */
  void print_destructor(ostream &os) {
    if (!can_copy)
      return;
    os << endl;
    print(os, "inline {1}::~{1}() {{\n"
              "  {0}_free(({0} *)This);\n"
              "  This = nullptr;\n"
              "}}\n",
          name, p_name);
  }

  /**
   * @brief Print the declaration of a destructor
   *
   * @param os
   */
  void print_destructor_h(ostream &os) {
    os << endl;
    if (can_copy && !is_inplace)
      print(os, "  virtual ~{0}();\n", p_name);
    else
      print(os, "  virtual ~{0}() = default;\n", p_name);
  }

  /**
   * @brief Print the definition of a toStr() method.
   *
   * @param os
   */
  void print_print_methods(ostream &os) {
    os << endl;
    print(os, "inline std::string {0}::toStr(isl::Format F) const {{\n"
              "  Printer p = Printer::toStr();\n"
              "  p = p.setOutputFormat(F);\n"
              "  p = p.print{0}(*this);\n"
              "  return p.getStr();\n"
              "}}\n",
          p_name);
  }

  /**
   * @brief Print the declaration of a toStr() method.
   *
   * @param os
   */
  void print_print_methods_h(ostream &os) {
    os << endl;
    print(os,
          "  std::string toStr(isl::Format F = isl::Format::FIsl) const;\n");
  }
};

/**
 * @brief Convert the paramType to JNA compatible names.
 *
 * This is copied from the java bindings, with minor adjustments to cpp code
 * generation.

 * Get the parameter type of an argument of type "ty" of a Java function
 * representing a given isl function in the JNA interface.
 * When isBool is true, then a pointer to int argument is assumed to
 * denote a boolean output parameter.
 * When wrapperTypes is true, a wrapper class (e.g. "Integer") is returned
 * instead of the underlying primitive type (e.g. "int").
 *
 * @param ty
 * @param wrapperTypes
 * @param isBool
 *
 * @return
 */
string cpp_generator::paramtype2jna(QualType ty, bool wrapperTypes,
                                    bool isBool) {
  string type;
  if (is_isl_ctx(ty))
    type = "Context.Ptr";
  else if (is_isl_result_argument(ty)) {
    type = cppTypeName(ty->getPointeeType()) + ".Ptr[]";
  } else if (is_isl_class(ty)) {
    type = type2cpp(extract_type(ty)) + ".Ptr";
  } else if (is_isl_enum(ty)) {
    type = "int";
  } else if (is_string(ty))
    type = "std::string";
  else if (ty->isFunctionPointerType())
    type = "Callback";
  else if (ty->isPointerType()) {
    if (ty->getPointeeType().getAsString().compare("int") == 0)
      type = isBool ? "bool[]" : "int[]";
    type = ty.getAsString();
  } else if (ty->isVoidType())
    type = "void";
  else {
    type = ty.getAsString();
  }

  return type;
}

/**
 * @brief Convert a parameter type to a jna compatible name.
 *
 * Copied from cpp bindings.
 *
 * @param decl
 *
 * @return
 */
string cpp_generator::paramtype2jna(const ParmVarDecl *decl) {
  return paramtype2jna(decl->getOriginalType(), false, is_bool(decl));
}

/**
 * @brief Convert a parameter type to a cpp compatible name
 *
 * Copied from cpp bindings.
 *
 * @param decl
 *
 * @return
 */
string cpp_generator::paramtype2cpp(const ParmVarDecl *decl) {
  return paramtype2cpp(decl->getOriginalType(), false, is_bool(decl));
}

/**
 * @brief Convert a return type to a jna compatible name
 *
 * This is copied from the java bindings, with minor adjustments to c++
 *
 * Get the return type of the Java function representing a given isl
 * function in the JNA interface.
 *
 * @param method
 *
 * @return
 */
string cpp_generator::rettype2jna(const FunctionDecl *method) {
  return paramtype2jna(method->getReturnType());
}

/**
 * @brief Convert a param type to a cpp compatible name.
 *
 * This is copied from the java bindings, with minor adjustments to c++
 *
 * Get the Java type corresponding to a given parameter type
 * of an isl function.
 * When wrapperTypes is true, a wrapper class (e.g. "Integer") is
 * returned instead of the underlying primitive type (e.g. "int").
 *
 * @param type
 * @param wrapperTypes
 * @param isBool
 *
 * @return
 */
string cpp_generator::paramtype2cpp(QualType type, bool wrapperTypes,
                                    bool isBool) {
  if (is_isl_type(type)) {
    return cppTypeName(type);
  } else if (is_isl_result_argument(type)) {
    return "std::unique_ptr<" + cppTypeName(type->getPointeeType()) + "> *";
  } else if (type->isPointerType()) {
    QualType ptype = type->getPointeeType();
    if (ptype->isFunctionType()) {
      const FunctionProtoType *ft = ptype->getAs<FunctionProtoType>();
      unsigned nArgs = ft->getNumArgs();
      ostringstream os;
      os << "const std::function<" << ft->getReturnType().getAsString();
      os << "(";
      for (unsigned i = 0; i < nArgs; ++i) {
        os << (i > 0 ? ", " : "") << ft->getArgType(i).getAsString();
      }
      os << ")> &&";
      return os.str();
    }
  }

  return paramtype2jna(type, wrapperTypes, isBool);
}

/**
 * @brief Get the return type of a isl method in isl compatible form.
 *
 * Copied from cpp bindings.
 *
 * Get the return type of the Java method corresponding
 * to the given isl function.
 *
 * @param method
 *
 * @return
 */
string cpp_generator::rettype2cpp(const FunctionDecl *method) {
  return paramtype2cpp(method->getReturnType());
}

/**
 * @brief Keywords in C++, do not use these.
 */
static const char *keywords[] = {"void",  "and",     "or",
                                 "union", "foreach", nullptr};

string cpp_generator::methodname2cpp(const isl_class &clazz,
                                     const string &methodname) {
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

/**
 * @brief Get an isl_ptr. Depending on the memory management qualifiers.
 *
 * Either we get a copy or the wrapped objects.
 *
 * @param classname the isl classname (unused)
 * @param expression the isl expression to get a pointer from
 * @param is_takes True, if we don't need a copy of the wrapped object.
 *
 * @return an expression that returns an isl pointer type.
 */
string cpp_generator::isl_ptr(const string &classname, const string &expression,
                              bool is_takes) {
  return format("({0}).{1}()", expression, (is_takes) ? "Give" : "Get");
}

/**
 * @brief Return the isl type name of this type
 *
 * Copied from cpp bindings.
 *
 * @param ty
 *
 * @return
 */
string cpp_generator::cppTypeName(QualType ty) {
  return type2cpp(extract_type(ty));
}

/**
 * @brief Check, if we need to create a named constructor or not.
 *
 * A constructor need not be named when
 * - the name is "read_from_str", or
 * - the name is "from_" followed by a class name (e.g.
 * "isl_set_from_basic_set"
 *   to construct an isl_set from an isl_basic_set).
 *
 * @param clazz
 * @param cons
 *
 * @return
 */
bool cpp_generator::constructorShouldBeNamed(const isl_class &clazz,
                                             const FunctionDecl *cons) {
  const string fullname = cons->getName();
  const string mname = clazz.name_without_class(fullname);
  // A constructor need not be named when
  // - the name is "read_from_str", or
  // - the name is "from_" followed by a class name (e.g.
  // "isl_set_from_basic_set"
  //   to construct an isl_set from an isl_basic_set).
  bool unnamed = mname == "read_from_str" ||
                 (mname.find("from_") == 0 &&
                  classes.find("isl_" + mname.substr(5)) != classes.end());

  return !unnamed;
}

/**
 * @brief Prepare a function argument before it is used.
 *
 * @param os
 * @param param
 */
void cpp_generator::prepare_argument(ostream &os, const ParmVarDecl *param) {
  QualType type = param->getOriginalType();
  const string &name = param->getNameAsString();
  if (is_unsigned(type)) {
    print(os, "  assert({0} >= 0);\n", name);
  } else if (is_isl_result_argument(type)) {
    QualType pType = type->getPointeeType();
    print(os, "  {0} _{1} = nullptr;\n", pType.getAsString(), name);
  } else if (is_isl_class(type)) {
    // Make sure the isl object is of the right type,
    // i.e., it matches the compile time type of the
    // parameter (an actual argument for, e.g., isl_union_set
    // could be an isl_set at runtime).
    print(os, "  {0} _cast_{1} = {1}.as{0}();\n", cppTypeName(type), name);
  }
}

/**
 * @brief Tag for function pointers (auto-incremented).
 */
static int fn_ptr_id = 0;
/**
 * @brief Print the function argument.
 *
 * @param os
 * @param param
 */
void cpp_generator::print_argument(ostream &os, ParmVarDecl *param) {
  const string &name = param->getNameAsString();
  QualType type = param->getOriginalType();
  if (is_isl_result_argument(type)) {
    print(os, "({0}) ? &_{0} : nullptr", name);
  } else if (is_isl_enum(type)) {
    print(os, "({0}){1}", type.getAsString(), name);
  } else if (is_isl_class(type)) {
    string classname = extract_type(type);
    os << isl_ptr(classname, "_cast_" + name, takes(param));
  } else if (is_string(type)) {
    print(os, "{0}.c_str()", name);
  } else if (is_callback(type)) {
    print(os, "get_fn_ptr<{0}>({1})", fn_ptr_id++, name);
  } else {
    os << name;
  }
}

/**
 * @brief Perform post-processing on return values.
 *
 * @param os
 * @param ctx
 * @param param
 */
void cpp_generator::handle_result_argument(ostream &os, const string &ctx,
                                           const ParmVarDecl *param) {
  QualType type = param->getOriginalType();
  if (is_isl_result_argument(type)) {
    const string &name = param->getNameAsString();
    const string cppTyName = cppTypeName(type->getPointeeType());

    print(os, "  if({0}) {{\n", name);
    printHandleErrorCall(os, 2, name + " became a NULL pointer.");
    print(os, "    {0} _tmp_{1} = {0} (_{1});\n"
              "    {1}->reset(new {0}(_tmp_{1}));\n"
              "  }}\n",
          cppTyName, name);
  }
}

/**
 * @brief Convert return values into enum values
 *
 * @param os
 * @param res
 * @param enu
 */
void cpp_generator::handle_enum_return(ostream &os, const string &res,
                                       const isl_enum &enu) {
  print(os, "  return ({0}){1};\n", type2cpp(enu.name), res);
}

/**
 * @brief Handle all return values and perform necessary checks/conversion.
 *
 * @param os
 * @param method
 * @param resVar
 */
void cpp_generator::handle_return(ostream &os, const FunctionDecl *method,
                                  const string &resVar) {
  QualType rettype = method->getReturnType();
  string fullname = method->getName();
  if (is_isl_class(rettype)) {
    string type = type2cpp(extract_type(method->getReturnType()));
    printHandleErrorCall(os, 2, fullname + " returned a NULL pointer.");
    print(os, "  return {0}({1});\n", type, resVar);
  } else if (is_isl_enum(rettype)) {
    handle_enum_return(os, resVar, find_enum(rettype));
  } else if (is_bool(method)) {
    printHandleErrorCall(os, 2, fullname + " signaled an error.");
    print(os, "  return {0} != 0;\n", resVar);
  } else if (is_string(rettype)) {
    print(os, "  std::string {0}_;\n", resVar);
    printHandleErrorCall(os, 2, fullname + " returned a NULL pointer.");
    print(os, "  {0}_ = {0};\n", resVar);

    if (gives(method)) {
      print(os, "  free((void *){0});\n", resVar);
    }
    print(os, "  return {0}_;\n", resVar);
  } else {
    print(os, "  return {0};\n", resVar);
  }
}

string cpp_generator::get_argument_decl_list(FunctionDecl *method, int offset) {
  ostringstream os;
  int num_params = method->getNumParams();
  for (int i = offset; i < num_params; ++i) {
    ParmVarDecl *param = method->getParamDecl(i);
    bool isIsl = is_isl_class(param->getOriginalType());
    string prefix = (i>offset) ? ", " : "";

    if (isIsl)
      print(os, "{0}const {1} &{2}", prefix, paramtype2cpp(param),
            param->getNameAsString());
    else
      print(os, "{0}{1} {2}", prefix, paramtype2cpp(param),
            param->getNameAsString());
  }
  return os.str();
}

string cpp_generator::get_argument_list(FunctionDecl *method, int offset) {
  ostringstream os;
  int num_params = method->getNumParams();

  if (offset)
    os << "Ctx.unwrap()";
  for (int i = offset; i < num_params; ++i) {
    ParmVarDecl *param = method->getParamDecl(i);
    if (i)
      os << ", ";
    print_argument(os, param);
  }
  return os.str();
}

/**
 * @brief Print a method declaration
 *
 * Copied from java bindings:
 *
 * Print a cpp method corresponding to the C function "method".
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
 *
 * @param os
 * @param clazz
 * @param method
 * @param subclass
 * @param super
 */
void cpp_generator::print_method(ostream &os, isl_class &clazz,
                                 FunctionDecl *method, bool subclass,
                                 string super) {
  string fullname = method->getName();
  string cname = methodname2cpp(clazz, fullname);
  string retName = rettype2cpp(method);
  int num_params = method->getNumParams();

  os << endl;
  print(os, "  ///@brief Generated from:\n"
            "  ///       {0}\n"
            "  ///\n",
        method->getNameAsString());
  for (int i = 1; i < num_params; ++i) {
    ParmVarDecl *param = method->getParamDecl(i);
    string p_name = param->getNameAsString();
    bool is_give = is_isl_result_argument(param->getOriginalType());

    if (is_give)
      print(os, "  ///@param {0} output parameter (isl_give)\n", p_name);
    else
      print(os, "  ///@param {0}\n", p_name);
  }
  print(os, "  ///\n"
            "  ///@return A new {0}\n"
            "  {0} {1}({2}) const;\n",
        retName, cname, get_argument_decl_list(method, 1));
}

/**
 * @brief Print a method definition
 *
 * Print a cpp method corresponding to the C function "method".
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
 *
 * @param os
 * @param clazz
 * @param method
 * @param subclass
 * @param super
 */
void cpp_generator::print_method_impl(ostream &os, isl_class &clazz,
                                      FunctionDecl *method, bool subclass,
                                      string super) {
  string p_name = type2cpp(clazz.name);
  string fullname = method->getName();
  string cname = methodname2cpp(clazz, fullname);
  string retName = rettype2cpp(method);
  string retNameC = method->getReturnType().getAsString();
  int num_params = method->getNumParams();

  os << endl;
  print(os, "inline {0} {2}::{1}({3}) const {{\n"
            "  Ctx.lock();\n"
            "  {2} self = as{2}();\n",
        retName, cname, p_name, get_argument_decl_list(method, 1));

  for (int i = 1; i < num_params; ++i) {
    ParmVarDecl *param = method->getParamDecl(i);
    prepare_argument(os, param);
  }

  // Create argument list
  ostringstream param_os;
  for (int i = 1; i < num_params; ++i) {
    ParmVarDecl *param = method->getParamDecl(i);
    param_os << ", ";
    print_argument(param_os, param);
  }

  print(os, "  {0} res = {1}({2}{3});\n", retNameC, fullname,
        isl_ptr(clazz.name, "self", takes(method->getParamDecl(0))),
        param_os.str());

  for (int i = 1; i < num_params; ++i) {
    const ParmVarDecl *param = method->getParamDecl(i);
    handle_result_argument(os, "Ctx", param);
  }

  print(os, "  Ctx.unlock();\n");
  handle_return(os, method, "res");
  print(os, "}}\n");
}

/**
 * @brief Print definition of a constructor.
 *
 * Print part of the constructor for this isl_class.
 *
 * In particular, check if the actual arguments correspond to the
 * formal arguments of "cons" and if so call "cons" and put the
 * result in self.ptr and a reference to the default context in self.ctx.
 *
 * If the function consumes a reference, then we pass it a copy of
 * the actual argument.
 * @param os
 * @param clazz
 * @param cons
 * @param asNamedConstructor
 */
void cpp_generator::print_constructor(ostream &os, isl_class &clazz,
                                      FunctionDecl *cons,
                                      bool asNamedConstructor) {
  const string fullname = cons->getName();
  const string cname = methodname2cpp(clazz, fullname);
  const string jclass = type2cpp(clazz.name);
  int ctxSrc = -1;
  int num_params = cons->getNumParams();
  int drop_ctx = first_arg_is_isl_ctx(cons);
  const string obj = asNamedConstructor ? "That" : "This";

  print(os, "  /// @brief Constructor for {0}\n"
            "  ///\n",
        fullname);

  for (int i = drop_ctx; i < num_params; ++i) {
    ParmVarDecl *param = cons->getParamDecl(i);
    print(os, "  /// @param {0}\n", param->getNameAsString());
  }

  if (asNamedConstructor) {
    print(os, "  static {0} {1}({2});\n", jclass, cname,
          get_argument_decl_list(cons));
  } else {
    print(os, "  /// {0}\n"
              "  explicit {1}({2});\n",
          fullname, jclass, get_argument_decl_list(cons));
  }
}

/**
 * @brief Print a custom deleter lambda function (cleanup for smart-pointers)
 *
 * @param os
 * @param name
 * @param cname
 */
static void print_custom_deleter(ostream &os, string name, string cname) {
  print(os, "[=](ptr *{0}) {{\n"
            "  {1}_free({0}->p);\n"
            "  {0}->p = nullptr;\n"
            "}}",
        name, cname);
}

/**
 * @brief Print the constructor definition
 *
 * @param os
 * @param clazz
 * @param cons
 * @param asNamedConstructor
 */
void cpp_generator::print_constructor_impl(ostream &os, isl_class &clazz,
                                           FunctionDecl *cons,
                                           bool asNamedConstructor) {
  const string fullname = cons->getName();
  const string cname = methodname2cpp(clazz, fullname);
  const string jclass = type2cpp(clazz.name);
  string super;
  bool subclass = is_subclass(clazz.type, super);
  int ctxSrc = -1;
  int num_params = cons->getNumParams();
  int drop_ctx = first_arg_is_isl_ctx(cons);

  // Find us a context.
  for (int i = drop_ctx; i < num_params; ++i) {
    ParmVarDecl *param = cons->getParamDecl(i);
    if (is_isl_class(param->getOriginalType()))
      ctxSrc = i;
  }

  string context_source =
      (ctxSrc >= 0)
          ? format("{}.ctx()", cons->getParamDecl(ctxSrc)->getNameAsString())
          : "Context::get()";

  os << endl;
  if (asNamedConstructor) {
    print(os, "inline {0} {0}::{1}::{0}({2}) {{\n"
              "  Context &Ctx = {3};\n",
          jclass, cname, get_argument_decl_list(cons, drop_ctx), context_source);
  } else {
    string base_class = (!subclass) ? "IslBase" : type2cpp(super);
    print(os, "// {3}\n"
              "inline {0}::{0}({2}) : {4}({5}, (void *)NULL) {{\n",
          jclass, cname, get_argument_decl_list(cons, drop_ctx), fullname,
          base_class, context_source);
  }

  for (int i = drop_ctx; i < num_params; ++i) {
    ParmVarDecl *param = cons->getParamDecl(i);
    prepare_argument(os, param);
  }

  print(os, "  Ctx.lock();\n"
            "  {0} *That = {1}({2});\n",
        clazz.name, fullname, get_argument_list(cons, drop_ctx));

  printHandleErrorCall(os, 2, fullname + " returned a NULL pointer.");
  print(os, "  Ctx.unlock;\n");

  if (asNamedConstructor) {
    if (can_copy(clazz)) {
      print(os, "  return {0}(Ctx, That);\n", jclass);
    } else {
      print(os, "  std::shared_ptr<ptr> _That(new ptr(That));\n"
                "  return {0}(Ctx, _That);\n",
            jclass);
    }
  } else {
    if (can_copy(clazz)) {
      print(os, "  This = That;\n");
    } else {
      print(os, "  This = std::shared_ptr<ptr>(new ptr(That));\n");
    }
  }
  print(os, "}}\n");
}

/**
 * @brief Print a Pointer wrapper to store isl objects.
 *
 * We need a complete type for using smart pointers later on, so we wrap
 * isl pointers in a very basic way to provide a complete type.
 *
 * @param os
 * @param name
 */
static void print_ptr_wrapper(ostream &os, string name) {
  print(os, "  struct ptr {{\n"
            "    {0} *p;\n"
            "    explicit ptr({0} *p) : p(p) {{}}\n"
            "    ~ptr() {\n"
            "      {0}_free(p); }}\n"
            "    ptr(const ptr &other) = delete;\n"
            "    ptr &operator=(const ptr &other) = delete;\n"
            "    ptr(ptr && other) = delete;\n"
            "    ptr &operator=(ptr && other) = delete;\n"
            "  }};\n",
        name);
}

/**
 * @brief Print out the definition of this isl_class.
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
 *
 * @param clazz
 */
void cpp_generator::print_class(isl_class &clazz) {
  string super;
  bool subclass = is_subclass(clazz.type, super);
  bool can_cp = can_copy(clazz);
  cpp_class_printer p(clazz, subclass, can_cp, super, is_inplace(clazz));

  const string &name = clazz.name;
  string p_name = type2cpp(name);
  set<FunctionDecl *>::iterator in;
  string fileName = includePath + p_name + ".h";
  ostream &os = outputfile(fileName);
  set<pair<string, bool>> Deps = getDependencies(clazz);

  os << getGuardHeader(p_name) << endl;

  if (!IslExtraDeps.count(name))
    print(os, "#include \"isl/{}.h\"\n", getIncludeForIslObj(name));

  vector<string> extras = IslExtraDeps[name];
  for (string extra : extras) {
    print(os, "#include \"isl/{}.h\"\n", getIncludeForIslObj(extra));
  }

  os << getIncludes(getDependencies(clazz)) << endl;
  print(os, "#include \"isl/IslFnPtr.h\"\n");

  os << endl;

  string base_class = (subclass) ? type2cpp(super) : "IslBase";

  print(os, "namespace isl {{\n"
            "{0}"
            "\n"
            "class {1} : public {2} {{\n"
            "protected:\n",
        getForwardDecls(Deps), p_name, base_class);

  if (!can_cp) {
    print_ptr_wrapper(os, name);
    os << endl;
    print(os, "  std::shared_ptr<ptr> This;\n"
              "\n");
  }

  p.print_explicit_constructors_h(os);

  print(os, "\n"
            "public:\n");
  if (can_cp) {
    os << endl;
    print(
        os,
        "  /// @brief Implement lt via pointer comparison of the\n"
        "  ///        wrapped isl objects.\n"
        "  bool operator<(const {0} &RHS) const { return This < RHS.This; }\n",
        p_name);
    os << endl;
  }

  p.print_api_wrapper_h(os);
  p.print_api_give_h(os);

  p.print_copy_constructor_h(os);
  p.print_copy_assignment_h(os);
  p.print_move_constructor_h(os);
  p.print_move_assignment_h(os);

  for (in = clazz.constructors.begin(); in != clazz.constructors.end(); ++in) {
    bool asNamed = constructorShouldBeNamed(clazz, *in);
    // We always print a "named" constructor (i.e., a static function
    // to construct an object).
    os << endl;
    print_constructor(os, clazz, *in, true);
    // Some constructors are also made available as
    // constructors of the class.
    if (!asNamed)
      print_constructor(os, clazz, *in, false);
  }

  p.print_api_unwrapper_h(os);

  // We do not free objects of classes that have in-place update
  // (e.g., isl_band). These values exist only in dependence of
  // parent objects and are freed when the parent object goes away.
  p.print_destructor_h(os);

  if (can_be_printed(clazz)) {
    p.print_print_methods_h(os);
  }

  // Print conversion functions for every super class.
  os << endl;
  print(os, "  virtual {0} as{0}() const;\n", p_name);

  isl_class *s_clazz = &clazz;
  string s_name;
  while (is_subclass(s_clazz->type, s_name)) {
    s_clazz = &classes[s_name];
    s_name = type2cpp(s_name);
    os << endl;
    print(os, "  virtual {0} as{0}() const override;\n", s_name);
  }

  for (auto method : clazz.methods)
    print_method(os, clazz, method, subclass, super);

  print(os, "\n"
            "}};\n"
            "}} // namespace isl\n");
  // if (name.compare("isl_val") == 0)
  //  print_additional_val_methods(os);

  os << getGuardFooter(p_name);
}

/**
 * @brief Generate all
 */
void cpp_generator::generate() {
  generateClasses();
  generateEnums();
}

/**
 * @brief Print a special class for the isl_ctx objects.
 *
 * We perform locking in here to restore thread safety.
 */
void cpp_generator::print_isl_ctx_class() {
  string fileName = includePath + "Context.h";
  ostream &os = outputfile(fileName);

  os << getGuardHeader("Context");
  print(os,
        "#include \"isl/{0}.h\"\n"
        "\n"
        "#include <mutex>\n"
        "#include <isl/options.h>\n"
        "\n"
        "namespace isl{\n"
        "class Context {{\n"
        "private:\n"
        "  isl_ctx *This;\n"
        "  std::recursive_mutex M;\n"
        "\n"
        "public:\n"
        "  static Context &get(isl_ctx *Other = NULL) {{\n"
        "    static Context instance(Other);\n"
        "    return instance;\n"
        "  }}\n"
        "\n"
        "  void lock() {{\n"
        "    M.lock();\n"
        "  }}\n"
        "\n"
        "  void unlock() {{\n"
        "    M.unlock();\n"
        "  }}\n"
        "\n"
        "  isl_ctx *unwrap() {{\n"
        "    return This;\n"
        "  }}\n"
        "\n"
        "  bool hasError() {{\n"
        "    enum isl_error err = isl_ctx_las_error(This);\n"
        "    int goe = isl_options_get_in_error(This);\n"
        "    return (err != isl_error_none) && goe != ISL_ON_ERROR_CONTINUE;\n"
        "  }}\n"
        "\n"
        "private:\n"
        "  Context(isl_ctx *Other = NULL) {{\n"
        "    if (This)\n"
        "      return;\n"
        "    if (Other)\n"
        "      This = Other;\n"
        "    else\n"
        "      This = isl_ctx_alloc();\n"
        "  }} // Invisible.\n"
        "\n"
        "  Context(Context const &); // Do not implement\n"
        "  void operator=(Context const &); // Do not implement\n"
        "}};\n"
        "}} //namespace isl\n",
        getIncludeForIslObj("isl_ctx"));
  os << getGuardFooter("Context");
}

/**
 * @brief A list of non-copyable objects that are not declare uncopyable in isl.
 *
 * TODO: Fix this in isl, maybe?
 *
 * As we implement the isl_copy stuff via unwrap, we want to make sure to be
 * able to unwrap. So far only isl_schedule and isl_printer fail us.
 */
static set<string> NonCopyable = {"isl_schedule", "isl_printer"};
/**
 * @brief Return true, if the class name is in the NonCopyable set.
 *
 * @param clazz
 *
 * @return
 */
bool cpp_generator::can_copy(isl_class &clazz) {
  return NonCopyable.count(clazz.name) == 0;
}

/**
 * @brief Print the class implementation
 *
 * @param clazz
 */
void cpp_generator::print_class_impl(isl_class &clazz) {
  string super;
  const string &name = clazz.name;
  string p_name = type2cpp(name);
  bool subclass = is_subclass(clazz.type, super);
  bool can_cp = can_copy(clazz);

  cpp_class_printer p(clazz, subclass, can_cp, super, is_inplace(clazz));

  set<FunctionDecl *>::iterator in;
  string fileName = includePath + p_name + ".hpp";
  ostream &os = outputfile(fileName);

  os << getGuardHeader(p_name + "_IMPL");
  print(os, "#include \"isl/{0}.h\"\n"
            "\n"
            "{1}"
            "\n"
            "#include <cassert>\n"
            "\n"
            "namespace isl {{\n",
        p_name, getIncludes(getDependencies(clazz), true));

  p.print_api_wrapper(os);
  p.print_copy_assignment(os);

  for (in = clazz.constructors.begin(); in != clazz.constructors.end(); ++in) {
    bool asNamed = constructorShouldBeNamed(clazz, *in);
    // We always print a "named" constructor (i.e., a static function
    // to construct an object).
    print_constructor_impl(os, clazz, *in, true);
    // Some constructors are also made available as
    // constructors of the class.
    if (!asNamed)
      print_constructor_impl(os, clazz, *in, false);
  }

  // We do not free objects of classes that have in-place update
  // (e.g., isl_band). These values exist only in dependence of
  // parent objects and are freed when the parent object goes away.
  if (!is_inplace(clazz)) {
    p.print_destructor(os);
  }

  p.print_api_give(os);
  p.print_api_unwrapper(os);

  if (can_be_printed(clazz)) {
    p.print_print_methods(os);
  }

  // Print conversion functions for every super class.
  os << endl;
  if (can_cp) {
    print(os, "inline {0} {0}::as{0}() const {{\n"
              "  return {0}(GetCopy());\n"
              "}}\n",
          p_name);
  } else {
    print(os, "inline {0} {0}::as{0}() const {{\n"
              "  return *const_cast<{0} *>(this);\n"
              "}}\n",
          p_name);
  }

  isl_class *s_clazz = &clazz;
  string s_name;

  while (is_subclass(s_clazz->type, s_name)) {
    s_clazz = &classes[s_name];
    s_name = type2cpp(s_name);
    os << endl;
    print(os, "inline {0} {1}::as{0}() const {{\n"
              "  return {0}(*this);\n"
              "}}\n",
          s_name, p_name);
  }

  for (in = clazz.methods.begin(); in != clazz.methods.end(); ++in)
    print_method_impl(os, clazz, *in, subclass, super);

  os << endl;
  // if (name.compare("isl_val") == 0)
  //  print_additional_val_methods(os);
  print(os, "}} // namespace isl\n");
  os << getGuardFooter(p_name + "_IMPL");
}

/**
 * @brief Generate all classes.
 */
void cpp_generator::generateClasses() {
  print_isl_ctx_class();
  print_isl_obj_class();
  print_isl_exception_class();
  print_function_ptr_helper();

  map<string, isl_class>::iterator ci;
  for (ci = classes.begin(); ci != classes.end(); ++ci) {
    print_class(ci->second);
    print_class_impl(ci->second);
  }
}

/**
 * @brief Insert a dependency for inclusion/forward declaration.
 *
 * The function takes a set of dependencies to insert into and a
 * dependency pair. The first component is the class/file(w/o file-extension)
 * name we insert as dependency. The second component is true, if we
 * want to fullfil the dependency via a direct include and false, if
 * a forward decl is sufficient.
 *
 * @param Deps The set of dependencies we insert into.
 * @param Dep The dependency pair we want to include.
 */
static void insertDep(
    set<pair<string, bool>> &Deps,
    pair<
        string,
        bool /* True, if we want an include. False, if we want a forward decl */>
        Dep) {
  pair<string, bool> complement = make_pair(Dep.first, !Dep.second);
  if (Deps.count(complement))
    Deps.erase(complement);
  Deps.insert(Dep);
}

/**
 * @brief Update the dependency set, if necessary.
 *
 * @param p_name
 * @param Deps
 * @param Ty
 */
void cpp_generator::insertIfDependency(string p_name,
                                       set<pair<string, bool>> &Deps,
                                       QualType &Ty) {
  if (is_isl_class(Ty) || is_isl_enum(Ty)) {
    string d_name = paramtype2cpp(Ty);
    if (d_name != p_name) {
      insertDep(Deps, make_pair(d_name, is_isl_enum(Ty)));
    }
  } else if (is_isl_result_argument(Ty)) {
    QualType pTy = Ty->getPointeeType();
    insertIfDependency(p_name, Deps, pTy);
  } else if (Ty->isPointerType()) {
    QualType pTy = Ty->getPointeeType();
    if (pTy->isFunctionType()) {
      const FunctionProtoType *ft = pTy->getAs<FunctionProtoType>();
      unsigned nArgs = ft->getNumArgs();
      for (unsigned i = 0; i < nArgs; ++i) {
        QualType argTy = ft->getArgType(i);
        insertIfDependency(p_name, Deps, argTy);
      }
    }
  }
}

/**
 * @brief Get all dependencies of an isl class.
 *
 * @param clazz
 *
 * @return
 */
set<pair<string, bool>> cpp_generator::getDependencies(isl_class &clazz) {
  set<pair<string, bool>> Deps;
  set<clang::FunctionDecl *>::iterator it;
  set<clang::FunctionDecl *>::iterator ie;
  string p_name = type2cpp(clazz.name);

  if (can_be_printed(clazz))
    insertDep(Deps, make_pair("Printer", false));

  for (it = clazz.constructors.begin(), ie = clazz.constructors.end(); it != ie;
       ++it) {
    clang::FunctionDecl *C = *it;
    for (unsigned i = 0; i < C->getNumParams(); ++i) {
      const ParmVarDecl *P = C->getParamDecl(i);
      QualType Ty = P->getOriginalType();
      insertIfDependency(p_name, Deps, Ty);
    }
  }

  for (it = clazz.methods.begin(), ie = clazz.methods.end(); it != ie; ++it) {
    clang::FunctionDecl *M = *it;
    for (unsigned i = 0; i < M->getNumParams(); ++i) {
      const ParmVarDecl *P = M->getParamDecl(i);
      QualType Ty = P->getOriginalType();
      insertIfDependency(p_name, Deps, Ty);
    }

    clang::QualType retTy = M->getReturnType();
    insertIfDependency(p_name, Deps, retTy);
  }

  string super;
  bool subclass = is_subclass(clazz.type, super);
  if (subclass) {
    insertDep(Deps, make_pair(type2cpp(super), true));
  } else {
    insertDep(Deps, make_pair("IslBase", true));
  }

  // We need the isl::Format then
  if (can_be_printed(clazz)) {
    insertDep(Deps, make_pair("Format", true));
  }

  insertDep(Deps, make_pair("IslException", true));
  insertDep(Deps, make_pair("Context", true));

  return Deps;
}

/**
 * @brief Print a super class for al isl objects.
 *
 * This class holds the context and the wrapped isl object.
 */
void cpp_generator::print_isl_obj_class() {
  const string p_name = "IslBase";
  string fileName = includePath + p_name + ".h";
  ostream &os = outputfile(fileName);

  print(os, "#ifndef ISL_CXX_{0}_H\n"
            "#define ISL_CXX_{0}_H\n"
            "\n"
            "#include \"isl/Context.h\"\n"
            "#include <memory>\n"
            "#include <vector>\n"
            "#include <iostream>\n"
            "\n"
            "namespace isl {{\n"
            "class {0} {{\n"
            "protected:\n"
            "  Context &Ctx;\n"
            "  void * This;\n"
            "\n"
            "  {0}(Context &Ctx, void *That) : Ctx(Ctx), This(That) {{}}\n"
            "public:\n"
            "  Context &ctx() const {{ return Ctx; }}\n"
            "}};\n"
            "\n",
        p_name);

  printHandleError(os, 0);

  // Generate an inplace map function template that can be applied on all
  // member methods that do not require parameters:
  os << endl;
  print(os,
        "template <class T, typename... Args>\n"
        "void map_inplace(T (T::*fn)(Args& ...args) const, "
        "std::vector<std::reference_wrapper<T>> &&objs, Args && ...args) {{\n"
        "  for (T &obj : objs)\n"
        "    obj = (obj.*fn)(std::forward<Args>(args)...);\n"
        "}}\n"
        "}} //namespace isl\n");
  os << getGuardFooter("Context");
}

/**
 * @brief Print an enum.
 *
 * @param enu
 */
void cpp_generator::print_enum(const isl_enum &enu) {
  const string e_name = type2cpp(enu.name);
  string fileName = includePath + e_name + ".h";
  ostream &os = outputfile(fileName);

  os << getGuardHeader(e_name);
  print(os, "namespace isl {{\n"
            "enum {0} {{\n",
        e_name);

  for (auto EnumValue : enu.values) {
    print(os, "  {0}{1} = {2},\n", name2initials(e_name),
          enumval2cpp(EnumValue.first), EnumValue.second);
  }
  print(os, "}};\n"
            "}} // namespace isl\n");
  os << getGuardFooter(e_name);
}

/**
 * @brief Print an isl exception class
 */
void cpp_generator::print_isl_exception_class() {
  const string p_name = "IslException";
  string fileName = includePath + p_name + ".h";
  ostream &os = outputfile(fileName);

  os << getGuardHeader(p_name);
  print(os, "#include <exception>\n"
            "#include <string>\n"
            "\n"
            "namespace isl {{\n"
            "class {0} : public std::exception {{\n"
            "  std::string What;\n"
            "\n"
            "public:\n"
            "  {0}(std::string What) : What(What) {{}}\n"
            "  virtual const char *what() const throw() override {\n"
            "    return What.c_str();\n"
            "  }}\n"
            "}};\n"
            "}} //namespace isl\n",
        p_name);
  os << getGuardFooter(p_name);
}

/**
 * @brief Print the isl function pointer helper class.
 *
 * This creates a binding between parameters and a std::function compatible
 * callback function. This way we can bind the arguments in our wrapper
 * functions to the callback, without knowing what callback the user gives
 * us.
 *
 * The fn_ptr_helper class we generate holds a static function ptr()
 * that can be used like any other function pointer.
 */
void cpp_generator::print_function_ptr_helper() {
  const string p_name = "IslFnPtr";
  string fileName = includePath + p_name + ".h";
  ostream &os = outputfile(fileName);

  os << getGuardHeader("FN_PTR");
  print(os,
        "#include <unistd.h>\n"
        "#include <thread>\n"
        "#include <chrono>\n"
        "#include <mutex>\n"
        "#include <functional>\n"
        "#include <iostream>\n"
        "#include <cmath>\n"
        "\n"
        "template <const size_t _UniqueId, typename _Res, typename... "
        "_ArgTypes>\n"
        "struct fun_ptr_helper\n"
        "{\n"
        "public:\n"
        "  typedef std::function<_Res(_ArgTypes...)> function_type;\n"
        "\n"
        "  static void bind(function_type&& f)\n"
        "  {{ instance().fn_.swap(f); }}\n"
        "\n"
        "  static void bind(const function_type& f)\n"
        "  {{ instance().fn_=f; }}\n"
        "\n"
        "  static _Res invoke(_ArgTypes... args)\n"
        "  {{ return instance().fn_(args...); }}\n"
        "\n"
        "  typedef decltype(&fun_ptr_helper::invoke) pointer_type;\n"
        "  static pointer_type ptr()\n"
        "  {{ return &invoke; }}\n"
        "\n"
        "private:\n"
        "  static fun_ptr_helper& instance()\n"
        "  {{\n"
        "    static fun_ptr_helper inst_;\n"
        "    return inst_;\n"
        "  }}\n"
        "\n"
        "  fun_ptr_helper() {{}}\n"
        "  function_type fn_;\n"
        "}};\n"
        "\n"
        "template <const size_t _UniqueId, typename _Res, typename... "
        "_ArgTypes>\n"
        "typename fun_ptr_helper<_UniqueId, _Res, _ArgTypes...>::pointer_type\n"
        "get_fn_ptr(const std::function<_Res(_ArgTypes...)>& f)\n"
        "{{\n"
        "  fun_ptr_helper<_UniqueId, _Res, _ArgTypes...>::bind(f);\n"
        "  return fun_ptr_helper<_UniqueId, _Res, _ArgTypes...>::ptr();\n"
        "}}\n");
  os << getGuardFooter("FN_PTR");
}

/**
 * @brief Generate all enums.
 */
void cpp_generator::generateEnums() {
  map<string, isl_enum>::iterator ei;
  for (ei = enums.begin(); ei != enums.end(); ++ei)
    print_enum(ei->second);
}

/**
 * @brief Construct a new cpp_generator.
 *
 * @param types
 * @param functions
 * @param enums
 */
cpp_generator::cpp_generator(set<RecordDecl *> &types,
                             set<FunctionDecl *> &functions,
                             set<EnumDecl *> &enums)
    : generator(types, functions, enums) {}
