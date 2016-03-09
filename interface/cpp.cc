#include "isl_config.h"
#include "cpp.h"
#include "generator.h"

#include <cctype>
#include <cstdio>
#include <sstream>
#include <string>
#include <map>
#include <algorithm>

#define FMT_HEADER_ONLY
#include "cppformat/format.h"

#define DEBUG(X) \
  do { X; } while (0)

static const std::string srcPath = "";
static const std::string includePath = "";

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
	{"isl_union_set_list", {"isl_union_set"}},
	{"isl_constraint_list", {"isl_constraint"}},
	{"isl_id_list", {"isl_id"}},
	{"isl_val_list", {"isl_val"}},
	{"isl_multi_val", {"isl_val"}},
	{"isl_ctx", {"isl_options", "isl_band"}},
};

using fmt::print;
using fmt::format;

/**
 * \brief Get the necessary include for an isl object
 *
 * \param islName the isl-name of the object
 *
 * @return the include name (without .h suffix).
 */
static const string getIncludeForIslObj(const string &islName)
{
	assert(islName.length() >= 4);
	string sName = islName.substr(4);
	string lName;
	for (string::const_iterator it = sName.begin(), ie = sName.end();
	     it != ie; ++it) {
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

static set<HeaderInfo> complement(const set<HeaderInfo> &LHS,
		                          const set<HeaderInfo> &RHS) {
	set<HeaderInfo> Res;
	for (auto Elem : LHS) {
		if (!RHS.count(Elem)) {
			Res.insert(Elem);
		}
	}

	return Res;
}

/**
 * \brief Get the includes for a given set of dependencies.
 *
 * This function is used to create the include preamble of a .cpp or .h
 * file of the isl bindings.
 *
 * \param deps the header dependencies. The set contains pairs of string and
 *        bool. If the bool is True, we require the full header.
 * \param impl True, if we are a .cpp file, not a header.
 *
 * @return a string containing the include preamble for a .cpp or .h file
 */
const string cpp_generator::getIncludes(isl_class &clazz, bool impl = false)
{
	auto deps = getDependences(clazz);
	string includes;

	if (impl) {
		for (auto Dep : deps.Forwards) {
			includes += format("#include \"{0}\"\n", Dep.getAsImplString());
		}

		for (auto Dep : deps.Includes) {
			includes += format("#include \"{0}\"\n", Dep.getAsImplString());
		}
	} else {
		for (auto Dep : deps.Includes) {
			includes += format("#include \"{0}\"\n", Dep.getAsDeclString());
		}
	}

	if (clazz.is_ctx()) {
		includes += "#include <mutex>\n";
	}

	includes += "#include <string>\n";

	return includes;
}

/**
 * \brief Get the forward declarations for a given set of dependencies.
 *
 * This function is used to create the forward declarations required in the
 * .cpp/.h file of the isl bindings.
 *
 * \param deps the header dependencies. The set contains pairs of string and
 *        bool. If the bool is True, we require the full header.
 *
 * @return a string containing the forward declarations for a .cpp or .h file
 */
static const string getForwardDecls(Dependences &Deps)
{
	string forwards;
	for (auto Dep : complement(Deps.Forwards, Deps.Includes)) {
		forwards += "class " + Dep.getAsString() + ";\n";
	}

	return forwards;
}

/**
 * \brief Indent the outstream by a given depth with spaces.
 *
 * \param os the outstream to indent
 * \param depth the indenting depth (one space)
 *
 * @return the indentend outstream
 */
static string indent(unsigned depth)
{
	string indt;
	while (depth > 0) {
		indt += " ";
		--depth;
	}

	return indt;
}

/**
 * \brief Create a header guard header(!).
 *
 * Creates:
 *  #ifndef ISL_CXX_<CLASSNAME>_H
 *  #define ISL_CXX_<CLASSNAME>_H
 *
 * \param className the class name of the isl object
 *
 * @return a string containing the header guard header.
 */
static const string getGuardHeader(const string className)
{
	return format("#ifndef ISL_CXX_{0}_H\n"
		      "#define ISL_CXX_{0}_H\n"
		      "\n",
		      className);
}

/**
 * \brief Create a header guard footer.
 *
 * Creates:
 *  #endif // ISL_CXX_<CLASSNAME>_H
 *
 * \param className the class name of the isl object
 *
 * @return a string containing the header guard footer
 */
static const string getGuardFooter(const string className)
{
	return format("#endif //ISL_CXX_{0}_H\n", className);
}

/**
 * \brief Convert an isl name to camel case.
 *
 * This is copied from the java bindings.
 *
 * \param name
 * \param startUpper
 *
 * @return
 */
static string name2camelcase(const string &name, bool startUpper)
{
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
 * \brief Convert an enum name to a cpp-compatible type name.
 *
 * We cut of the isl_ prefix and convert the rest to a camel-case name.
 *
 * Example:
 *  isl_ast_options -> AstOptions
 *
 * \param valname the name of the enum value
 *
 * @return
 */
static string enumval2cpp(const string &valname)
{
	size_t n = valname.find("_");
	assert(n != string::npos);
	n = valname.find("_", n + 1);
	assert(n != string::npos);
	return name2camelcase(valname.substr(n + 1), true);
}

/**
 * \brief Drop the isl_ prefix of a name.
 *
 * This is not actually checking for the prefix, we just drop the first
 * 4 chars.
 *
 * \param name
 *
 * @return
 */
static string type2cpp(const string &name)
{
	assert(name.length() >= 4);
	return name2camelcase(name.substr(4), true);
}

static void printHandleError(ostream &os, int level)
{
	print(os, "{0}static inline void handleError(std::string what) {{\n"
		  "{0}#ifdef __EXCEPTIONS\n"
		  "{0}  throw IslException(what);\n"
		  "{0}#else\n"
		  "{0}  std::cerr << what << std::endl;\n"
		  "{0}  std::abort();\n"
		  "{0}#endif\n"
		  "{0}}}\n",
	      indent(level));
}

static void printHandleErrorCall(ostream &os, int level, string &&what,
				 string context = "ctx")
{
	print(os, "{0}if ({2}.hasError()) {{\n"
		  "{0}  handleError(\"{1}\");\n"
		  "{0}}}\n",
	      indent(level), what, context);
}

/**
 * \brief Print a Pointer wrapper to store isl objects.
 *
 * We need a complete type for using smart pointers later on, so we wrap
 * isl pointers in a very basic way to provide a complete type.
 *
 * \param os
 * \param name
 */
static void print_ptr_wrapper(ostream &os, string name)
{
	print(os, "  struct ptr {{\n"
		  "    {0} *p;\n"
		  "    explicit ptr({0} *p) : p(p) {{}}\n"
		  "    ~ptr() {{\n"
		  "      {0}_free(p);\n"
		  "    }}\n"
		  "    ptr(const ptr &other) = delete;\n"
		  "    ptr &operator=(const ptr &other) = delete;\n"
		  "    ptr(ptr && other) = delete;\n"
		  "    ptr &operator=(ptr && other) = delete;\n"
		  "  }};\n",
	      name);
}

/**
 * \brief Print a CPP class.
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
class cpp_class_printer
{
      protected:
	const isl_class &clazz;
	const bool subclass;
	const bool can_copy;
	const bool is_inplace;
	const string &super;
	const string base_class;
	string name;
	string p_name;

	void print_explicit_copyable_subclass(ostream &os)
	{
		print_explicit_copyable(os);
	}

	void print_explicit_copyable(ostream &os) {
		print(os, "protected:\n"
			  "  Ctx ctx;\n"
			  "  void * This;\n"
			  "public:\n"
			  "  explicit {0}(Ctx ctx, {1} *That) : "
			  "ctx(ctx), This(That) {{}}\n",
		      p_name, name);
	}

	void print_explicit_non_copyable(ostream &os) {
		print(os, "protected:\n"
			  "  Ctx ctx;\n"
			  "  std::shared_ptr<ptr> This;\n"
			  "public:\n"
			  "  explicit {0}(Ctx ctx, {1} *That) : "
			  "ctx(ctx), This(std::make_shared<ptr>(That)) {{}}\n",
		      p_name, name);
	}

      public:
	/**
	 * \brief Create a Class printer
	 *
	 * \param clazz the isl_class to print
	 * \param subclass true, if we are a subclass of another isl_class.
	 * \param can_copy true, if we can create copies of ourself.
	 * \param super our superclass.
	 */
	explicit cpp_class_printer(const isl_class &clazz, bool subclass,
		bool can_copy, const string &super, bool is_inplace) :
			clazz(clazz), subclass(subclass), can_copy(can_copy),
			is_inplace(is_inplace), super(super),
			base_class((!subclass) ? "IslBase" : type2cpp(super))
	{
		name = clazz.name;
		p_name = type2cpp(name);
	}

	/**
	 * \brief Print declaration of explicit constructors
	 *
	 * \param os
	 */
	virtual void print_explicit_constructors_h(ostream &os)
	{
		if (!subclass && can_copy) {
			print_explicit_copyable(os);
		} else if (!subclass) {
			print_explicit_non_copyable(os);
		} else if (subclass && can_copy) {
			print_explicit_copyable_subclass(os);
		} else {
			print(cerr, "not implemented!\n");
			assert(0);
		}
	}

	/**
	 * \brief Print declaration of a copy constructor
	 *
	 * \param os
	 */
	virtual void print_copy_constructor_h(ostream &os)
	{
		print(os, "  {0}(const {0} &Other) : "
			  "ctx(Other.Context()), This(Other.GetCopy())"
			  " {{}}\n",
		      p_name, base_class);
	}

	/**
	 * \brief Print definition of the copy assignment operator
	 *
	 * \param os
	 */
	virtual void print_copy_assignment(ostream &os)
	{
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
	 * \brief Print declaration of the copy assignment operator
	 *
	 * \param os
	 */
	virtual void print_copy_assignment_h(ostream &os)
	{
		if (!can_copy)
			print(os,
			      "  {0} &operator=(const {0} &Other) = delete;\n",
			      p_name);
		else
			print(os, "  {0} &operator=(const {0} &Other);\n",
			      p_name);
	}

	/**
	 * \brief Print declaration of the move constructor
	 *
	 * \param os
	 */
	virtual void print_move_constructor_h(ostream &os)
	{
		print(os,
		      "  {0} ({0} && Other) : ctx(Other.Context()), "
		      "This(Other.This)"
		      " {{}}\n",
		      p_name, base_class);
	}

	/**
	 * \brief Print declaration of the move assignment operator
	 *
	 * \param os
	 */
	virtual void print_move_assignment_h(ostream &os)
	{
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
	 * \brief Print definition of the API wrapper
	 *
	 * \param os
	 */
	virtual void print_api_wrapper(ostream &os)
	{
		std::endl(os);
		if (can_copy) {
			print(os, "inline {0} *{1}::GetCopy() const {{\n"
				  "  return {0}_copy(({0} *)This);\n"
				  "}}\n",
			      name, p_name);
		} else {
			print(os, "inline std::shared_ptr<isl::{0}::ptr> "
				  "{0}::GetCopy() const {{\n"
				  "  return {0}::This;\n"
				  "}}\n",
			      p_name);
		}
	}

	/**
	 * \brief Print the declaration of the API wrapper
	 *
	 * \param os
	 */
	virtual void print_api_wrapper_h(ostream &os)
	{
		std::endl(os);
		if (can_copy) {
			print(os, "  __isl_give {0} *GetCopy() const;\n", name);
		} else {
			print(os, "  std::shared_ptr<isl::{0}::ptr> GetCopy() "
				  "const;\n",
			      p_name);
		}
	}

	/**
	 * \brief Print the definition of the API unwrapper
	 *
	 * The API Unwrapper returns a _copy_ of the ISL object, if possible.
	 *
	 * \param os
	 */
	virtual void print_api_unwrapper(ostream &os)
	{
		print(os, "/// \\brief Unwrap the stored isl object.\n"
			  "/// \\returns A the wrapped isl object.\n"
			  "inline {0} *{1}::Get() const {{"
			  "  return ({0} *){2};\n"
			  "}}\n",
		      name, p_name, (can_copy) ? "This" : "This.get()->p");
	}

	/**
	 * \brief Print the declaration of the API unwrapper
	 *
	 * The API Unwrapper returns a _copy_ of the ISL object, if possible.
	 *
	 * \param os
	 */
	virtual void print_api_unwrapper_h(ostream &os)
	{
		print(os, "  /// \\brief unwrap the stored isl object.\n"
			  "  /// \\return a the wrapped isl object.\n"
			  "  __isl_give {0} *Get() const;\n",
		      name);
	}

	/**
	 * \brief Print the definition of the API Give
	 *
	 * Returns the wrapped pointer, this exits isl
	 *
	 * \param os
	 */
	virtual void print_api_give(ostream &os)
	{
		print(os,
		      "/// \brief Release ownership of the wrapped object.\n"
		      "///\n"
		      "/// You are on your own now buddy.\n"
		      "/// The wrapper cannot be used anymore after calling "
		      "Give()\n"
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
				  "  {0} *res = This.get()->p;\n"
				  "  This.get()->p = nullptr;\n"
				  "  This.reset();\n"
				  "  return res;\n"
				  "}}\n",
			      name, p_name);
	}

	/**
	 * \brief Print the declaration of the API Give
	 *
	 * Returns the wrapped pointer, this exits isl
	 *
	 * \param os
	 */
	virtual void print_api_give_h(ostream &os)
	{
		print(os,
		      "  /// \\brief Release ownership of the wrapped object.\n"
		      "  ///\n"
		      "  /// You are on your own now buddy.\n"
		      "  /// The wrapper cannot be used anymore after calling "
		      "Give()\n"
		      "  ///\n"
		      "  /// \\returns the wrapped isl object.\n"
		      "  __isl_give {0} *Give();\n",
		      name);
	}

	/**
	 * \brief Print the definition of a destructor
	 *
	 * \param os
	 */
	virtual void print_destructor(ostream &os)
	{
		if (!can_copy)
			return;
		print(os, "inline {1}::~{1}() {{\n"
			  "  {0}_free(({0} *)This);\n"
			  "  This = nullptr;\n"
			  "}}\n",
		      name, p_name);
	}

	/**
	 * \brief Print the declaration of a destructor
	 *
	 * \param os
	 */
	virtual void print_destructor_h(ostream &os)
	{
		if (can_copy && !is_inplace)
			print(os, "public:\n"
				  "  virtual ~{0}();\n",
			      p_name);
		else
			print(os, "public:\n"
				  "  virtual ~{0}() = default;\n",
			      p_name);
	}

	/**
	 * \brief Print the definition of a toStr() method.
	 *
	 * \param os
	 */
	virtual void print_print_methods(ostream &os)
	{
		print(os,
		      "inline std::string {0}::toStr(isl::Format F) const {{\n"
		      "  Printer p = Printer::toStr(ctx);\n"
		      "  p = p.setOutputFormat(F);\n"
		      "  p = p.print{0}(*this);\n"
		      "  return p.getStr();\n"
		      "}}\n",
		      p_name);
	}

	/**
	 * \brief Print the declaration of a toStr() method.
	 *
	 * \param os
	 */
	virtual void print_print_methods_h(ostream &os)
	{
		print(os, "  std::string toStr(isl::Format F = "
			  "isl::Format::FIsl) const;\n");
	}

	/**
	 * Print additional methods required for this class.
	 */
	virtual void print_extra_methods_h(ostream &os) {}
	virtual void print_constructor_h(ostream &os) {}
	virtual void print_constructor(ostream &os) {}
};

class context_class_printer : public cpp_class_printer
{
      public:
	/**
	 * \brief Create a Class printer
	 *
	 * \param clazz the isl_class to print
	 * \param subclass true, if we are a subclass of another isl_class.
	 * \param can_copy true, if we can create copies of ourself.
	 * \param super our superclass.
	 */
	explicit context_class_printer(const isl_class &clazz, bool subclass,
				       bool can_copy, const string &super,
				       bool is_inplace)
	    : cpp_class_printer(clazz, subclass, can_copy, super, is_inplace)
	{
	}

	/**
	 * \brief Print the declaration of the API wrapper
	 *
	 * \param os
	 */
	void print_api_wrapper_h(ostream &os) override
	{
		// 1. Wrap an isl_* object.
		print(os,
		      "  /// \\brief Wrap an existing isl object.\n"
		      "  ///\n"
		      "  /// This serves as an entry point into the C++ API.\n"
		      "  /// We take ownership of the isl object.\n"
		      "  ///\n"
		      "  /// \\param That the {0} we want to wrap.\n"
		      "  explicit {1}({0} *That) : ", name, p_name);

		print(os, "This(std::make_shared<isl::{0}::ptr>(That)) "
			  "{{}}\n", p_name);

	}

	void print_explicit_constructors_h(ostream &os) override
	{
		print(os, "protected:\n"
			  "  std::shared_ptr<ptr> This;\n"
			  "public:\n"
			  "  explicit {}(std::shared_ptr<ptr> That) : "
			  "This(That) {{}}\n",
		      p_name);
	}

	void print_extra_methods_h(ostream &os) override {
		print(os,
		     	  "private:\n"
		  	  "  mutable std::recursive_mutex M;\n"
		  	  "public:\n"
		      	  "  void lock() const {{\n"
			  "    M.lock();\n"
			  "  }}\n"
			  "\n"
			  "  void unlock() const {{\n"
			  "    M.unlock();\n"
			  "  }}\n"
			  "\n"
			  "  bool hasError() const {{\n"
			  "    isl_ctx *ctx = Get();\n"
			  "    enum isl_error err = isl_ctx_last_error(ctx);\n"
			  "    int goe = isl_options_get_on_error(ctx);\n"
			  "    return (err != isl_error_none) && goe != "
			  "ISL_ON_ERROR_CONTINUE;\n"
			  "  }}\n");
	}

	void print_copy_constructor_h(ostream &os) override {
		print(os, "  {0}(const {0} &Other) : {0}(Other.This) {{}}",
		      p_name);
	}
	void print_copy_assignment(ostream &os) override {}
	void print_copy_assignment_h(ostream &os) override {}
	void print_move_constructor_h(ostream &os) override {}
	void print_move_assignment_h(ostream &os) override {}
	void print_api_wrapper(ostream &os) override {}
	void print_print_methods(ostream &os) override {}
	void print_print_methods_h(ostream &os) override {}
};

/**
 * \brief Get an isl_ptr. Depending on the memory management qualifiers.
 *
 * Either we get a copy or the wrapped object.
 *
 * \param expression the isl expression to get a pointer from
 * \param is_takes True, if we don't need a copy of the wrapped object.
 * \param can_copy True, if we cannot create a copy of the wrapped object.
 *
 * @return an expression that returns an isl pointer type.
 */
string cpp_generator::isl_ptr(const string &expression, bool is_takes,
			      bool can_copy)
{
	if (is_takes)
		if (can_copy)
			return format("({0}).{1}()", expression,
					"GetCopy");
	return format("({0}).{1}()", expression,
			"Get");
}

/**
 * \brief Convert a parameter type to a cpp compatible name
 *
 * Copied from cpp bindings.
 *
 * \param decl
 *
 * @return
 */
string cpp_generator::paramtype2cpp(const ParmVarDecl *decl)
{
	return paramtype2cpp(decl->getOriginalType(), false, is_bool(decl));
}

/**
 * \brief Convert a param type to a cpp compatible name.
 *
 * This is copied from the java bindings, with minor adjustments to c++
 *
 * Get the Java type corresponding to a given parameter type
 * of an isl function.
 * When wrapperTypes is true, a wrapper class (e.g. "Integer") is
 * returned instead of the underlying primitive type (e.g. "int").
 *
 * \param type
 * \param wrapperTypes
 * \param isBool
 *
 * @return
 */
string cpp_generator::paramtype2cpp(QualType type, bool wrapperTypes,
	bool isBool)
{
	if (is_isl_type(type)) {
		return cppTypeName(type);
	} else if (is_isl_result_argument(type)) {
		return format("std::unique_ptr<{0}> *",
			      cppTypeName(type->getPointeeType()));
	} else if (is_isl_ctx(type)) {
		return "Context &";
	} else if (is_isl_enum(type)) {
	 	return "int";
	} else if (is_string(type)) {
		return "std::string";
	} else if (type->isVoidType()) {
		return "void";
	} else if (type->isPointerType()) {
		QualType ptype = type->getPointeeType();
		if (ptype->isFunctionType()) {
			const FunctionProtoType *ft =
			    ptype->getAs<FunctionProtoType>();
			unsigned nArgs = ft->getNumArgs();

			ostringstream arg_decl_list;
			for (unsigned i = 0; i < nArgs; ++i) {
				arg_decl_list
				    << (i > 0 ? ", " : "")
				    << ft->getArgType(i).getAsString();
			}

			return format("const std::function<{0}({1})> &&",
				      ft->getReturnType().getAsString(),
				      arg_decl_list.str());
		}
	}

	return type.getAsString();
}

/**
 * \brief Get the return type of a isl method in isl compatible form.
 *
 * Copied from cpp bindings.
 *
 * Get the return type of the Java method corresponding
 * to the given isl function.
 *
 * \param method
 *
 * @return
 */
string cpp_generator::rettype2cpp(const FunctionDecl *method)
{
	return paramtype2cpp(method->getReturnType());
}

/**
 * \brief Keywords in C++, do not use these.
 */
static const char *keywords[] = {"void",  "and", "or", "union", "foreach",
	"delete", nullptr};

string cpp_generator::methodname2cpp(const isl_class &clazz,
	const string &methodname)
{
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
 * \brief Return the isl type name of this type
 *
 * Copied from cpp bindings.
 *
 * \param ty
 *
 * @return
 */
string cpp_generator::cppTypeName(QualType ty)
{
	return type2cpp(extract_type(ty));
}

/**
 * \brief Prepare a function argument before it is used.
 *
 * \param os
 * \param param
 */
void cpp_generator::prepare_argument(ostream &os, const ParmVarDecl *param)
{
	QualType type = param->getOriginalType();
	const string &name = param->getNameAsString();
	if (is_isl_result_argument(type)) {
		QualType pType = type->getPointeeType();
		print(os, "  {0} _{1} = nullptr;\n", pType.getAsString(), name);
	}
}

/**
 * \brief Tag for function pointers (auto-incremented).
 */
static int fn_ptr_id = 0;
/**
 * \brief Print the function argument.
 *
 * \param os
 * \param param
 */
void cpp_generator::print_argument(ostream &os, ParmVarDecl *param)
{
	const string &name = param->getNameAsString();
	QualType type = param->getOriginalType();
	if (is_isl_result_argument(type)) {
		print(os, "({0}) ? &_{0} : nullptr", name);
	} else if (is_isl_enum(type)) {
		print(os, "({0}){1}", type.getAsString(), name);
	} else if (is_isl_ctx(type)) {
		print(os, "({0}.Get())", name);
	} else if (is_isl_class(type)) {
		os << isl_ptr(name, takes(param), can_copy(type));
	} else if (is_string(type)) {
		print(os, "{0}.c_str()", name);
	} else if (is_callback(type)) {
		print(os, "get_fn_ptr<{0}>({1})", fn_ptr_id++, name);
	} else {
		os << name;
	}
}

/**
 * \brief Perform post-processing on return values.
 *
 * \param os
 * \param ctx
 * \param param
 */
void cpp_generator::handle_result_argument(ostream &os, const string &ctx,
	const ParmVarDecl *param)
{
	const string &name = param->getNameAsString();
	QualType type = param->getOriginalType();
	if (is_isl_result_argument(type)) {
		const string cppTyName = cppTypeName(type->getPointeeType());

		print(os, "  if({0}) {{\n", name);
		printHandleErrorCall(os, 2, name + " became a NULL pointer.");
		print(os, "    {0} _tmp_{1} = {0}(ctx, _{1});\n"
			  "    {1}->reset(new {0}(_tmp_{1}));\n"
			  "  }}\n",
		      cppTyName, name);
	}
}

/**
 * \brief Convert return values into enum values
 *
 * \param os
 * \param res
 * \param enu
 */
void cpp_generator::handle_enum_return(ostream &os, const string &res,
	const isl_enum &enu)
{
	print(os, "  return ({0}){1};\n", type2cpp(enu.name), res);
}

static bool can_assign(const clang::QualType &Ty) {
	return !Ty->isVoidType();
}

/**
 * \brief Handle all return values and perform necessary checks/conversion.
 *
 * \param os
 * \param method
 * \param resVar
 */
void cpp_generator::handle_return(ostream &os, FunctionDecl *method,
	const string &resVar)
{
	QualType rettype = method->getReturnType();
	if (!can_assign(rettype))
		return;

	string fullname = method->getName();
	if (is_isl_class(rettype)) {
		string type = type2cpp(extract_type(method->getReturnType()));
		printHandleErrorCall(os, 2,
				     fullname + " returned a NULL pointer.");
		print(os, "  return {0}(ctx, {1});\n", type, resVar);
	} else if (is_isl_enum(rettype)) {
		handle_enum_return(os, resVar, find_enum(rettype));
	} else if (is_bool(method)) {
		printHandleErrorCall(os, 2, fullname + " signaled an error.");
		print(os, "  return {0} != 0;\n", resVar);
	} else if (is_string(rettype)) {
		print(os, "  std::string {0}_;\n", resVar);
		printHandleErrorCall(os, 2,
				     fullname + " returned a NULL pointer.");
		print(os, "  {0}_ = {0};\n", resVar);

		if (gives(method)) {
			print(os, "  free((void *){0});\n", resVar);
		}
		print(os, "  return {0}_;\n", resVar);
	} else {
		print(os, "  return {0};\n", resVar);
	}
}

/**
 * \brief Create a list of argument declarations
 *
 * \param method the method we create the list for
 * \param offset skip the first <offset> arguments
 *
 * @return list of argument declarations
 */
string cpp_generator::get_argument_decl_list(FunctionDecl *method, int offset)
{
	ostringstream os;
	int num_params = method->getNumParams();
	for (int i = offset; i < num_params; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		bool isIsl = is_isl_class(param->getOriginalType());
		string prefix = (i > offset) ? ", " : "";

		if (isIsl)
			print(os, "{0}const {1} &{2}", prefix,
			      paramtype2cpp(param), param->getNameAsString());
		else
			print(os, "{0}{1} {2}", prefix, paramtype2cpp(param),
			      param->getNameAsString());
	}
	return os.str();
}

/**
 * \brief Create a list of arguments
 *
 * \param method the method we create the list for
 * \param offset skip the first <offset> arguments
 *
 * @return
 */
string cpp_generator::get_argument_list(FunctionDecl *method, int offset)
{
	ostringstream os;
	int num_params = method->getNumParams();

	for (int i = offset; i < num_params; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		if (i)
			os << ", ";
		print_argument(os, param);
	}
	return os.str();
}

/**
 * \brief Print a method declaration
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
 * \param os
 * \param clazz
 * \param method
 * \param subclass
 * \param super
 */
void cpp_generator::print_method(ostream &os, isl_class &clazz,
				 FunctionDecl *method, bool subclass,
				 string super)
{
	string IslMethod = method->getName();
	string CxxMethod = methodname2cpp(clazz, IslMethod);
	string CxxRetType = rettype2cpp(method);
	int NumParams = method->getNumParams();

	// Create a comment block for our parameters.
	ostringstream comment;
	for (int i = 1; i < NumParams; ++i) {
		ParmVarDecl *Param = method->getParamDecl(i);
		string ParamName = Param->getNameAsString();
		bool Gives = is_isl_result_argument(Param->getOriginalType());

		if (Gives)
			print(comment,
			      "  /// \\param [out] {0} (isl_give)\n",
			      ParamName);
		else
			print(comment, "  /// \\param [in] {0}\n", ParamName);
	}

	print(os, "  /// \\brief Generated from  ::<{0}>\n"
		  "  ///\n"
		  "{1}"
		  "  ///\n"
		  "  /// \\returns A new {2}\n"
		  "  {2} {3}({4}) const;\n"
		  "\n",
	      method->getNameAsString(), comment.str(), CxxRetType, CxxMethod,
	      get_argument_decl_list(method, 1));
}

/**
 * \brief Print a method definition
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
 * \param os
 * \param clazz
 * \param method
 * \param subclass
 * \param super
 */
void cpp_generator::print_method_impl(ostream &os, isl_class &clazz,
				      FunctionDecl *method, bool,
				      string)
{
	string IslMethod = method->getName();
	string IslRetType = can_assign(method->getReturnType()) ?
		format("{0} res = ", method->getReturnType().getAsString()) :
		"";
	string CxxClass = type2cpp(clazz.name);
	string CxxMethod = methodname2cpp(clazz, IslMethod);
	string CxxRetType = rettype2cpp(method);
	int NumParams = method->getNumParams();
	string Context = (clazz.is_ctx()) ? "(*this)" : "ctx";

	// Prepare arguments
	ostringstream prepare_os;
	for (int i = 1; i < NumParams; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		prepare_argument(prepare_os, param);
	}

	// Create argument list
	ostringstream param_os;
        param_os << get_argument_list(method, 1);

	// Handle result args
	ostringstream result_os;
	for (int i = 1; i < NumParams; ++i) {
		const ParmVarDecl *param = method->getParamDecl(i);
		handle_result_argument(result_os, "Ctx", param);
	}

	ostringstream handle_error_os;
	printHandleErrorCall(handle_error_os, 2,
			     IslMethod + " returned a NULL pointer.",
			     Context);

	// Handle return
	ostringstream return_os;
	handle_return(return_os, method, "res");

	os << endl;
	print(os, "inline {0} {2}::{1}({3}) const {{\n"
		  "  {11}.lock();\n"
		  "{4}"
		  "  {5} {6}({7}{8});\n"
		  "{9}"
		  "  {11}.unlock();\n"
		  "{10}"
		  "}}\n",
	      CxxRetType, CxxMethod, CxxClass,
	      get_argument_decl_list(method, 1), prepare_os.str(), IslRetType,
	      IslMethod, isl_ptr("*this", takes(method->getParamDecl(0)),
				 can_copy(method->getParamDecl(0)->getType())),
	      param_os.str(), result_os.str(), return_os.str(), Context);
}

/**
 * \brief Print definition of a constructor.
 *
 * Print part of the constructor for this isl_class.
 *
 * In particular, check if the actual arguments correspond to the
 * formal arguments of "cons" and if so call "cons" and put the
 * result in self.ptr and a reference to the default context in self.ctx.
 *
 * If the function consumes a reference, then we pass it a copy of
 * the actual argument.
 * \param os
 * \param clazz
 * \param cons
 */
void cpp_generator::print_constructor(ostream &os, isl_class &clazz,
				      FunctionDecl *cons)
{
	const string IslMethod = cons->getName();
	const string CxxMethod = methodname2cpp(clazz, IslMethod);
	const string CxxClass = type2cpp(clazz.name);
	int NumParams = cons->getNumParams();

	os << endl;
	print(os, "  /// \\brief Constructor for {0}\n"
		  "  ///\n",
	      IslMethod);

	for (int i = 0; i < NumParams; ++i) {
		ParmVarDecl *param = cons->getParamDecl(i);
		print(os, "  /// \\param {0}\n", param->getNameAsString());
	}

        print(os, "  static {0} {1}({2});\n", CxxClass, CxxMethod,
              get_argument_decl_list(cons));
}

/**
 * \brief Print the constructor definition
 *
 * \param os
 * \param clazz
 * \param cons
 */
void cpp_generator::print_constructor_impl(ostream &os, isl_class &clazz,
					   FunctionDecl *cons)
{
	const string IslMethod = cons->getName();
	const string CxxMethod = methodname2cpp(clazz, IslMethod);
	const string CxxClass = type2cpp(clazz.name);
	int NumParams = cons->getNumParams();
	int ContextSource = find_context_source(cons);
	bool IsContext = clazz.is_ctx();
	string Context = (IsContext) ? "That" : "_ctx";

	ostringstream prepare_os;
	for (int i = 0; i < NumParams; ++i) {
		ParmVarDecl *param = cons->getParamDecl(i);
		prepare_argument(prepare_os, param);
	}

	string ArgumentList = get_argument_list(cons);
	string ArgumentDeclList = get_argument_decl_list(cons);

	ostringstream handle_error_os;
	printHandleErrorCall(handle_error_os, 2,
			     IslMethod + " returned a NULL pointer.", Context);

	ostringstream result_os;
	for (int i = 0; i < NumParams; ++i) {
		const ParmVarDecl *param = cons->getParamDecl(i);
		handle_result_argument(result_os, "Ctx", param);
	}

	os << endl;
	print(os, "inline {0} {0}::{1}({2}) {{\n", CxxClass, CxxMethod,
	      ArgumentDeclList);
	if (ContextSource >= 0) {
		std::string Context =
		    cons->getParamDecl(ContextSource)->getNameAsString();
		print(os, "  const Ctx &_ctx = {0}.Context();\n"
			  	  "  _ctx.lock();\n",
			  Context);
	}

	if(clazz.is_ctx()) {
		print(os, "  return {0}({1}({2}));\n",
		  	  CxxClass, IslMethod, ArgumentList);
	} else {
		print(os, "{0}"
		  	      "  {1} *That = {2}({3});\n"
		  	      "{5}\n"
		  	      "  _ctx.unlock();\n"
		  	      "{4}\n",
		  	  prepare_os.str(), clazz.name, IslMethod, ArgumentList,
		  	  handle_error_os.str(), result_os.str());
		print(os, "  return {0}(_ctx, That);\n", CxxClass);
	}

	print(os, "}}\n");
}

/**
 * \brief Generate all
 */
void cpp_generator::generate()
{
	generateClasses();
	generateEnums();
}

/**
 * \brief A list of non-copyable objects that are not declare uncopyable in isl.
 *
 * TODO: Fix this in isl, maybe?
 *
 * As we implement the isl_copy stuff via unwrap, we want to make sure to be
 * able to unwrap. So far only isl_schedule and isl_printer fail us.
 */
static set<string> NonCopyable = {"isl_ctx", "isl_schedule", "isl_printer"};
/**
 * \brief Return true, if the class name is in the NonCopyable set.
 *
 * \param clazz
 *
 * @return
 */
bool cpp_generator::can_copy(isl_class &clazz)
{
	return NonCopyable.count(clazz.name) == 0;
}

bool cpp_generator::can_copy(const QualType &type)
{
	if (is_isl_class(type))
		return can_copy(classes[extract_type(type)]);
	return false;
}

/**
 * \brief Filter a set of class methods by name, not pointer.
 *
 * \param methods A set of methods we want to make unique by name.
 *
 * \returns a map from method name to method pointer.
 **/
static std::map<const llvm::StringRef, clang::FunctionDecl *> make_unique(
	const std::set<clang::FunctionDecl *> &methods) {
	std::map<const llvm::StringRef, clang::FunctionDecl *> UniqueFns;
	for (auto method : methods) {
		UniqueFns.insert(std::make_pair(method->getName(), method));
	}
	return UniqueFns;
}

bool cpp_generator::has_method(const std::string &method_name,
			       const isl_class &clazz)
{
	for (auto &MethodKV : clazz.methods)
		for (auto method : MethodKV.second) {
			const string &name =
			    methodname2cpp(clazz, method->getNameAsString());
			if (name.compare(method_name) == 0 /*equal*/) {
				return true;
			}
		}
	return false;
}

/**
 * \brief Print out the definition of this isl_class.
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
 * \param clazz
 */
void cpp_generator::print_class(isl_class &clazz)
{
	string super;
	bool subclass = is_subclass(clazz.type, super);
	bool can_cp = can_copy(clazz);
	std::unique_ptr<cpp_class_printer> p;
	if (clazz.is_ctx())
		p = std::unique_ptr<context_class_printer>(
		    new context_class_printer(clazz, subclass, can_cp, super,
					      is_inplace(clazz)));
	else
		p = std::unique_ptr<cpp_class_printer>(
		    new cpp_class_printer(clazz, subclass, can_cp, super,
					  is_inplace(clazz)));

	const string &name = clazz.name;
	string p_name = type2cpp(name);
	set<FunctionDecl *>::iterator in;
	string fileName = includePath + p_name + ".h";
	ostream &os = outputfile(fileName);
	Dependences Deps = getDependences(clazz);

	os << getGuardHeader(p_name) << endl;

	if (!IslExtraDeps.count(name))
		print(os, "#include \"isl/{}.h\"\n", getIncludeForIslObj(name));
	else {
		vector<string> extras = IslExtraDeps[name];
		for (string extra : extras) {
			print(os, "#include \"isl/{}.h\"\n", getIncludeForIslObj(extra));
		}
	}

	os << getIncludes(clazz) << endl;

	print(os, "#include \"isl/IslFnPtr.h\"\n\n");
	print(os, "namespace isl {{\n{0}\n", getForwardDecls(Deps));
	print(os, "class {0} {{\n", p_name);

	if (!can_cp)
		print_ptr_wrapper(os, name);

	p->print_explicit_constructors_h(os);

	std::endl(os);
	print(os, "  const Ctx &Context() const {{ return {0}; }}\n",
	      clazz.is_ctx() ? "*this" : "ctx");

	p->print_api_wrapper_h(os);
	p->print_api_give_h(os);
	p->print_api_unwrapper_h(os);

	for (auto &in : clazz.constructors) {
		os << endl;
		print_constructor(os, clazz, in);
	}

	p->print_destructor_h(os);
	p->print_extra_methods_h(os);

	for (auto &subclass : super_to_subclass[name]) {
		os << endl;
		string subclass_name = type2cpp(subclass.name);
		if (has_method(format("from{0}()", subclass_name), subclass)) {
			print(
			    os,
			    "  /// \\brief Implicit conversion from {1}.\n"
			    "  {0}({1} &From) : {0}({0}::from{1}(From)) {{}}\n",
			    type2cpp(name), subclass_name);
		}
	}

	for (auto &MethodKV : clazz.methods)
		for (auto &method : make_unique(MethodKV.second)) {
		print_method(os, clazz, method.second, subclass, super);
	}

	p->print_copy_constructor_h(os);
	p->print_copy_assignment_h(os);
	p->print_move_constructor_h(os);
	p->print_move_assignment_h(os);

	if (can_cp) {
		print(
		    os,
		    "  /// \\brief Implement lt via pointer comparison of the\n"
		    "  ///         wrapped isl objects.\n"
		    "  bool operator<(const {0} &RHS) const {{ return This < "
		    "RHS.This; }}\n",
		    p_name);
		os << endl;
	}

	print(os, "}};\n"
		  "}} // namespace isl\n");

	os << getGuardFooter(p_name);
}

/**
 * \brief Print the class implementation
 *
 * \param clazz
 */
void cpp_generator::print_class_impl(isl_class &clazz)
{
	string super;
	const string &name = clazz.name;
	string p_name = type2cpp(name);
	bool subclass = is_subclass(clazz.type, super);
	bool can_cp = can_copy(clazz);

	std::unique_ptr<cpp_class_printer> p;
	if (clazz.is_ctx())
		p = std::unique_ptr<context_class_printer>(
		    new context_class_printer(clazz, subclass, can_cp, super,
					      is_inplace(clazz)));
	else
		p = std::unique_ptr<cpp_class_printer>(
		    new cpp_class_printer(clazz, subclass, can_cp, super,
					  is_inplace(clazz)));

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
	      p_name, getIncludes(clazz, true));

	p->print_api_wrapper(os);
	p->print_copy_assignment(os);

        for (auto &in : clazz.constructors) {
		print_constructor_impl(os, clazz, in);
        }

	if (!is_inplace(clazz)) {
		p->print_destructor(os);
	}

	p->print_api_give(os);
	p->print_api_unwrapper(os);

	for (auto &MethodKV : clazz.methods)
		for (auto &method : make_unique(MethodKV.second)) {
			print_method_impl(os, clazz, method.second, subclass,
					  super);
		}

	os << endl;
	print(os, "}} // namespace isl\n");
	os << getGuardFooter(p_name + "_IMPL");
}

/**
 * \brief Generate all classes.
 */
void cpp_generator::generateClasses()
{
	print_isl_obj_class();
	print_isl_exception_class();
	print_function_ptr_helper();

	for (auto &KV : classes) {
		string name = KV.first;
		isl_class &clazz = KV.second;
		if (name == "isl_printer")
			continue;
		print_class(clazz);
		print_class_impl(clazz);
	}
}

/**
 * \brief Update the dependency set, if necessary.
 *
 * \param p_name
 * \param Deps
 * \param Ty
 */
void cpp_generator::insertIfDependency(isl_class &clazz,
	Dependences &Deps, QualType && Ty)
{
	string p_name = type2cpp(clazz.name);
	bool isEnum = is_isl_enum(Ty);
	bool isCtx = is_isl_ctx(Ty);

	auto is_self = [&] (const string &name) {
		return p_name == name;
	};

	auto is_isl_class_or_enum = [&] (QualType &T) {
		return is_isl_class(T) || is_isl_enum(T);
	};

	auto collect_callback_deps = [&] (Dependences &Deps, QualType Ty) {
		QualType pTy = Ty->getPointeeType();
		const FunctionProtoType *ft = pTy->getAs<FunctionProtoType>();
		unsigned nArgs = ft->getNumArgs();
		for (unsigned i = 0; i < nArgs; ++i) {
			QualType argTy = ft->getArgType(i);
			string d_name = paramtype2cpp(argTy);
			if (is_isl_class_or_enum(argTy) && !is_self(d_name)) {
				string c_name = argTy->getPointeeType().getAsString();

				if (!IslExtraDeps.count(c_name)) {
					string inc = getIncludeForIslObj(c_name);
					Deps.insertInclude(HeaderInfo(inc, false));
				} else {
					for (string extra : IslExtraDeps[c_name]) {
						string inc = getIncludeForIslObj(extra);
						Deps.insertInclude(HeaderInfo(inc, false));
					}
				}
			}
		}
	};

	if (is_isl_result_argument(Ty))
		insertIfDependency(clazz, Deps, Ty->getPointeeType());

	if (is_isl_class_or_enum(Ty)) {
		string d_name = paramtype2cpp(Ty);
		if (!is_self(d_name)) {
			Deps.insert(HeaderInfo(d_name, !isEnum), isEnum || isCtx);
		}
	} else if (is_callback(Ty)) {
		collect_callback_deps(Deps, Ty);
	}
}

/**
 * \brief Get all dependencies of an isl class.
 *
 * \param clazz
 *
 * @return
 */
Dependences cpp_generator::getDependences(isl_class &clazz)
{
	string p_name = type2cpp(clazz.name);
	Dependences Deps;
	set<clang::FunctionDecl *>::iterator it, ie;

	auto ScanFunctionArgs = [&] (const clang::FunctionDecl *const F) {
		for (auto P : F->params()) {
			insertIfDependency(clazz, Deps, P->getOriginalType());
		}
	};

	std::for_each(clazz.constructors.begin(), clazz.constructors.end(),
		ScanFunctionArgs);
	for (auto &MethodsKV : clazz.methods) {
		const set<FunctionDecl *> methods = MethodsKV.second;
		std::for_each(methods.begin(), methods.end(),
			      [&](const FunctionDecl *const M) {
				      ScanFunctionArgs(M);
				      insertIfDependency(clazz, Deps,
							 M->getReturnType());
			      });
	}

	string super;
	if (is_subclass(clazz.type, super))
		Deps.insertInclude(HeaderInfo(type2cpp(super), true, false));
	else
		Deps.insertInclude(HeaderInfo("IslBase", false));

	if (can_be_printed(clazz))
		Deps.insertInclude(HeaderInfo("Format", false));
	Deps.insertInclude(HeaderInfo("IslException", false));

	return Deps;
}

/**
 * \brief Print a super class for al isl objects.
 *
 * This class holds the context and the wrapped isl object.
 */
void cpp_generator::print_isl_obj_class()
{
	const string p_name = "IslBase";
	string fileName = includePath + p_name + ".h";
	ostream &os = outputfile(fileName);

	print(os,
	      "#ifndef ISL_CXX_{0}_H\n"
	      "#define ISL_CXX_{0}_H\n"
	      "#include \"isl/IslException.h\"\n"
	      "\n"
	      "#include <memory>\n"
	      "#include <vector>\n"
	      "\n"
	      "namespace isl {{\n",
	      p_name);

	printHandleError(os, 0);

	// Generate an inplace map function template that can be applied on all
	// member methods that do not require parameters:
	os << endl;
	print(os, "template <class T, typename... Args>\n"
		  "void map_inplace(T (T::*fn)(Args& ...args) const, "
		  "std::vector<std::reference_wrapper<T>> &&objs, Args && "
		  "...args) {{\n"
		  "  for (T &obj : objs)\n"
		  "    obj = (obj.*fn)(std::forward<Args>(args)...);\n"
		  "}}\n"
		  "}} //namespace isl\n");
	os << getGuardFooter("Context");
}

/**
 * \brief Print an enum.
 *
 * \param enu
 */
void cpp_generator::print_enum(const isl_enum &enu)
{
	const string e_name = type2cpp(enu.name);
	string fileName = includePath + e_name + ".h";
	ostream &os = outputfile(fileName);

	os << getGuardHeader(e_name);
	print(os, "namespace isl {{\n"
		  "enum class {0} {{\n",
	      e_name);

	for (auto EnumValue : enu.values) {
		print(os, "  {0} = {1},\n",
		      enumval2cpp(EnumValue.first), EnumValue.second);
	}
	print(os, "}};\n"
		  "}} // namespace isl\n");
	os << getGuardFooter(e_name);
}

/**
 * \brief Print an isl exception class
 */
void cpp_generator::print_isl_exception_class()
{
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
		  "  virtual const char *what() const throw() override {{\n"
		  "    return What.c_str();\n"
		  "  }}\n"
		  "}};\n"
		  "}} //namespace isl\n",
	      p_name);
	os << getGuardFooter(p_name);
}

/**
 * \brief Print the isl function pointer helper class.
 *
 * This creates a binding between parameters and a std::function compatible
 * callback function. This way we can bind the arguments in our wrapper
 * functions to the callback, without knowing what callback the user gives
 * us.
 *
 * The fn_ptr_helper class we generate holds a static function ptr()
 * that can be used like any other function pointer.
 */
void cpp_generator::print_function_ptr_helper()
{
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
	      "#include <cmath>\n"
	      "\n"
	      "template <const size_t _UniqueId, typename _Res, typename... "
	      "_ArgTypes>\n"
	      "struct fun_ptr_helper\n"
	      "{{\n"
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
	      "typename fun_ptr_helper<_UniqueId, _Res, "
	      "_ArgTypes...>::pointer_type\n"
	      "get_fn_ptr(const std::function<_Res(_ArgTypes...)>& f)\n"
	      "{{\n"
	      "  fun_ptr_helper<_UniqueId, _Res, _ArgTypes...>::bind(f);\n"
	      "  return fun_ptr_helper<_UniqueId, _Res, _ArgTypes...>::ptr();\n"
	      "}}\n");
	os << getGuardFooter("FN_PTR");
}

/**
 * \brief Generate all enums.
 */
void cpp_generator::generateEnums()
{
	map<string, isl_enum>::iterator ei;
	for (ei = enums.begin(); ei != enums.end(); ++ei)
		print_enum(ei->second);
}

/**
 * \brief Construct a cpp interface generator.
 *
 * \param types
 * \param functions
 * \param enums
 */
cpp_generator::cpp_generator(set<RecordDecl *> &types,
			     set<FunctionDecl *> &functions,
			     set<EnumDecl *> &enums)
    : generator(types, functions, enums){};
