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
	{"isl_ctx", {"isl_options"}},
};

/**
 * @brief Get the necessary include for an isl object
 *
 * @param islName the isl-name of the object
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
const string cpp_generator::getIncludes(isl_class &clazz,
					bool impl = false)
{
	auto deps = getDependencies(clazz);
	string includes;
	for (auto DepPair : deps) {
		if (DepPair.second)
			includes +=
			    format("#include \"isl/{0}.h\"\n", DepPair.first);
		else if (impl)
			includes +=
			    format("#include \"isl/{0}.hpp\"\n", DepPair.first);
	}

	if (clazz.is_ctx()) {
		includes += "#include <mutex>\n";
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
static const string getForwardDecls(set<pair<string, bool>> deps)
{
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
static string indent(unsigned depth)
{
	assert(depth > 0);

	string indt;
	while (depth > 0) {
		indt += " ";
		--depth;
	}

	return indt;
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
static const string getGuardHeader(const string className)
{
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
static const string getGuardFooter(const string className)
{
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
static string name2initials(const string &name)
{
	string cCname = name2camelcase(name, true);
	string iname;

	for (string::const_iterator it = cCname.begin(); it != cCname.end();
	     ++it) {
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
static string enumval2cpp(const string &valname)
{
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
 * @brief Print a custom deleter lambda function (cleanup for smart-pointers)
 *
 * @param os
 * @param name
 * @param cname
 */
static void print_custom_deleter(ostream &os, string name, string cname)
{
	print(os, "[=](ptr *{0}) {{\n"
		  "  {1}_free({0}->p);\n"
		  "  {0}->p = nullptr;\n"
		  "}}",
	      name, cname);
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
		print(os, "  explicit {0}(Ctx ctx, {1} *That) : "
			  "{2}(ctx, (void *)That) {{/* empty */}}\n"
			  "  explicit {0}(Ctx ctx, void *That) : "
			  "{2}(ctx, (void *)That) {{/* empty */}}\n",
		      p_name, name, base_class);
	}
	void print_explicit_copyable(ostream &os) {
		print(os, "  Ctx ctx;\n");
		print(os, "  void * This;\n");

		print(os, "  explicit {0}(Ctx ctx, {1} *That) : "
		      "ctx(ctx), This((void *)That) {{}}\n",
		      p_name, name);
		print(os, "  explicit {0}(Ctx ctx, void *That) : "
		      "ctx(ctx), This(That) {{}}\n",
		      p_name);
	}
	void print_explicit_non_copyable(ostream &os) {
		print(os, "  Ctx ctx;\n");
		print(os, "  std::shared_ptr<ptr> This;\n");

		print(os, "  explicit {}(Ctx ctx, "
			  "std::shared_ptr<ptr> That) : "
			  "ctx(ctx), This(That) {{}}\n",
		      p_name);
	}

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
		bool can_copy, const string &super, bool is_inplace) :
			clazz(clazz), subclass(subclass), can_copy(can_copy),
			super(super),
			base_class((!subclass) ? "IslBase" : type2cpp(super)),
			is_inplace(is_inplace)
	{
		name = clazz.name;
		p_name = type2cpp(name);
	}

	/**
	 * @brief Print declaration of explicit constructors
	 *
	 * @param os
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
	 * @brief Print declaration of a copy constructor
	 *
	 * @param os
	 */
	virtual void print_copy_constructor_h(ostream &os)
	{
		if (subclass)
			print(os,
			      "  {0}(const {0} &Other) : {1}(Other.Context(), "
			      "Other.GetCopy()) {{}}\n",
			      p_name, base_class);
		else
			print(os,
			      "  {0}(const {0} &Other) : {0}(Other.Context(), "
			      "Other.GetCopy()) {{}}\n",
			      p_name, base_class);
	}

	/**
	 * @brief Print definition of the copy assignment operator
	 *
	 * @param os
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
	 * @brief Print declaration of the copy assignment operator
	 *
	 * @param os
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
	 * @brief Print declaration of the move constructor
	 *
	 * @param os
	 */
	virtual void print_move_constructor_h(ostream &os)
	{
		if (subclass)
			print(os, "  {0} ({0} && Other) : {1}(Other.Context(), "
				  "Other.This) {{}}\n",
			      p_name, base_class);
		else
			print(os, "  {0} ({0} && Other) : {0}(Other.Context(), "
				  "Other.This) {{}}\n",
			      p_name, base_class);
	}

	/**
	 * @brief Print declaration of the move assignment operator
	 *
	 * @param os
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
	 * @brief Print definition of the API wrapper
	 *
	 * @param os
	 */
	virtual void print_api_wrapper(ostream &os)
	{
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
	 * @brief Print the declaration of the API wrapper
	 *
	 * @param os
	 */
	virtual void print_api_wrapper_h(ostream &os)
	{
		// 1. Wrap an isl_* object.
		print(os,
		      "  ///@brief Wrap an existing isl object.\n"
		      "  ///\n"
		      "  /// This serves as an entry point into the C++ API.\n"
		      "  /// We take ownership of the isl object.\n"
		      "  ///\n"
		      "  ///@param That the {0} we want to wrap.\n"
		      "  explicit {1}({0} *That) : ", name, p_name);

		print(os,
		      "{1}(Ctx({0}_get_ctx(That)), ",
		      name, p_name);

		if (can_copy) {
			print(os, "That) {{}}\n");
			print(os, "  {0} *GetCopy() const;\n", name);
		} else {
			print(os,
			      "std::make_shared<isl::{0}::ptr>(That)) {{}}\n",
			      p_name);
			print(os, "  std::shared_ptr<isl::{0}::ptr> GetCopy() "
				  "const;\n",
			      p_name);
		}
	}

	/**
	 * @brief Print the definition of the API unwrapper
	 *
	 * The API Unwrapper returns a _copy_ of the ISL object, if possible.
	 *
	 * @param os
	 */
	virtual void print_api_unwrapper(ostream &os)
	{
		print(os, "/// @brief Unwrap the stored isl object.\n"
			  "/// @return A the wrapped isl object.\n"
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
	virtual void print_api_unwrapper_h(ostream &os)
	{
		print(os, "  /// @brief unwrap the stored isl object.\n"
			  "  /// @return a the wrapped isl object.\n"
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
	virtual void print_api_give(ostream &os)
	{
		print(os,
		      "/// @brief Release ownership of the wrapped object.\n"
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
	 * @brief Print the declaration of the API Give
	 *
	 * Returns the wrapped pointer, this exits isl
	 *
	 * @param os
	 */
	virtual void print_api_give_h(ostream &os)
	{
		print(os,
		      "  /// @brief Release ownership of the wrapped object.\n"
		      "  ///\n"
		      "  /// You are on your own now buddy.\n"
		      "  /// The wrapper cannot be used anymore after calling "
		      "Give()\n"
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
	 * @brief Print the declaration of a destructor
	 *
	 * @param os
	 */
	virtual void print_destructor_h(ostream &os)
	{
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
	 * @brief Print the declaration of a toStr() method.
	 *
	 * @param os
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
	virtual void print_extra_methods(ostream &os) {}

	virtual void print_constructor_h(ostream &os) {
	}

	virtual void print_constructor(ostream &os) {
	}
};

class context_class_printer : public cpp_class_printer
{
      public:
	/**
	 * @brief Create a Class printer
	 *
	 * @param clazz the isl_class to print
	 * @param subclass true, if we are a subclass of another isl_class.
	 * @param can_copy true, if we can create copies of ourself.
	 * @param super our superclass.
	 */
	explicit context_class_printer(const isl_class &clazz, bool subclass,
				       bool can_copy, const string &super,
				       bool is_inplace)
	    : cpp_class_printer(clazz, subclass, can_copy, super, is_inplace)
	{
	}

	/**
	 * @brief Print the declaration of the API wrapper
	 *
	 * @param os
	 */
	void print_api_wrapper_h(ostream &os) override
	{
		// 1. Wrap an isl_* object.
		print(os,
		      "  ///@brief Wrap an existing isl object.\n"
		      "  ///\n"
		      "  /// This serves as an entry point into the C++ API.\n"
		      "  /// We take ownership of the isl object.\n"
		      "  ///\n"
		      "  ///@param That the {0} we want to wrap.\n"
		      "  explicit {1}({0} *That) : ", name, p_name);

		print(os, "This(std::make_shared<isl::{0}::ptr>(That)) "
			  "{{}}\n", p_name);

	}

	void print_explicit_constructors_h(ostream &os) override
	{
		print(os, "  std::shared_ptr<ptr> This;\n"
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
	//void print_destructor(ostream &os) override {}
	//void print_destructor_h(ostream &os) override {}
	void print_print_methods(ostream &os) override {}
	void print_print_methods_h(ostream &os) override {}
};

/**
 * @brief Convert a parameter type to a cpp compatible name
 *
 * Copied from cpp bindings.
 *
 * @param decl
 *
 * @return
 */
string cpp_generator::paramtype2cpp(const ParmVarDecl *decl)
{
	return paramtype2cpp(decl->getOriginalType(), false, is_bool(decl));
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
	bool isBool)
{
	if (is_isl_type(type)) {
		return cppTypeName(type);
	} else if (is_isl_result_argument(type)) {
		return format("std::unique_ptr<{0}> *",
			      cppTypeName(type->getPointeeType()));
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
string cpp_generator::rettype2cpp(const FunctionDecl *method)
{
	return paramtype2cpp(method->getReturnType());
}

/**
 * @brief Keywords in C++, do not use these.
 */
static const char *keywords[] = {"void",  "and", "or", "union", "foreach",
	nullptr};

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
	bool is_takes)
{
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
string cpp_generator::cppTypeName(QualType ty)
{
	return type2cpp(extract_type(ty));
}

/**
 * @brief Prepare a function argument before it is used.
 *
 * @param os
 * @param param
 */
void cpp_generator::prepare_argument(ostream &os, const ParmVarDecl *param)
{
	QualType type = param->getOriginalType();
	const string &name = param->getNameAsString();
	if (is_unsigned(type)) {
		print(os, "  assert({0} >= 0);\n", name);
	} else if (is_isl_result_argument(type)) {
		QualType pType = type->getPointeeType();
		print(os, "  {0} _{1} = nullptr;\n", pType.getAsString(), name);
	} else if (is_isl_ctx(type)) {
		print(os, "  {0}.lock();\n", name);
	} else if (is_isl_class(type)) {
		// Make sure the isl object is of the right type,
		// i.e., it matches the compile time type of the
		// parameter (an actual argument for, e.g., isl_union_set
		// could be an isl_set at runtime).
		print(os, "  {0} _cast_{1} = {1}.as{0}();\n", cppTypeName(type),
		      name);
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
		os << isl_ptr(extract_type(type), "_cast_" + name,
			      takes(param));
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
	const ParmVarDecl *param)
{
	const string &name = param->getNameAsString();
	QualType type = param->getOriginalType();
	if (is_isl_result_argument(type)) {
		const string cppTyName = cppTypeName(type->getPointeeType());

		print(os, "  if({0}) {{\n", name);
		printHandleErrorCall(os, 2, name + " became a NULL pointer.");
		print(os, "    {0} _tmp_{1} = {0} (_{1});\n"
			  "    {1}->reset(new {0}(_tmp_{1}));\n"
			  "  }}\n",
		      cppTyName, name);
	} else if (is_isl_ctx(type)) {
		print(os, "  {0}.unlock();\n", name);
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
	const isl_enum &enu)
{
	print(os, "  return ({0}){1};\n", type2cpp(enu.name), res);
}

/**
 * @brief Handle all return values and perform necessary checks/conversion.
 *
 * @param os
 * @param method
 * @param resVar
 */
void cpp_generator::handle_return(ostream &os, FunctionDecl *method,
	const string &resVar)
{
	QualType rettype = method->getReturnType();
	string fullname = method->getName();
	if (is_isl_class(rettype)) {
		string type = type2cpp(extract_type(method->getReturnType()));
		printHandleErrorCall(os, 2,
				     fullname + " returned a NULL pointer.");
		print(os, "  return {0}({1});\n", type, resVar);
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
 * @brief Create a list of argument declarations
 *
 * @param method the method we create the list for
 * @param offset skip the first <offset> arguments
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
		bool isCtx = is_isl_ctx(param->getOriginalType());
		string prefix = (i > offset) ? ", " : "";

		//if (isCtx)
		//	print(os, "{0}{1} &{2}", prefix,
		//	      paramtype2cpp(param), param->getNameAsString());
		//else if (isIsl)
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
 * @brief Create a list of arguments
 *
 * @param method the method we create the list for
 * @param offset skip the first <offset> arguments
 *
 * @return
 */
string cpp_generator::get_argument_list(FunctionDecl *method, int offset)
{
	ostringstream os;
	int num_params = method->getNumParams();

	for (int i = 0; i < num_params; ++i) {
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
				 string super)
{
	string fullname = method->getName();
	string cname = methodname2cpp(clazz, fullname);
	string retName = rettype2cpp(method);
	int num_params = method->getNumParams();

	// Create a comment block for our parameters.
	ostringstream comment;
	for (int i = 1; i < num_params; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		string p_name = param->getNameAsString();
		bool is_give = is_isl_result_argument(param->getOriginalType());

		if (is_give)
			print(comment,
			      "  ///@param {0} output parameter (isl_give)\n",
			      p_name);
		else
			print(comment, "  ///@param {0}\n", p_name);
	}

	os << endl;
	print(os, "  ///@brief Generated from:\n"
		  "  ///       {0}\n"
		  "{1}"
		  "  ///\n"
		  "  ///@return A new {2}\n"
		  "  {2} {3}({4}) const;\n",
	      method->getNameAsString(), comment.str(), retName, cname,
	      get_argument_decl_list(method, 1));

	if (is_isl_class(method->getReturnType())) {
		os << endl;
		print(os, "  ///@brief Generated from:\n"
			  "  ///       {0}\n"
			  "{1}"
			  "  ///\n"
			  "  void {2}Inplace({3});\n",
		      method->getNameAsString(), comment.str(), cname,
		      get_argument_decl_list(method, 1));
	}
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
				      string super)
{
	string p_name = type2cpp(clazz.name);
	string fullname = method->getName();
	string cname = methodname2cpp(clazz, fullname);
	string retName = rettype2cpp(method);
	string retNameC = method->getReturnType().getAsString();
	int num_params = method->getNumParams();
	string context = (clazz.is_ctx()) ? "(*this)" : "ctx";

	// Prepare arguments
	ostringstream prepare_os;
	for (int i = 1; i < num_params; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		prepare_argument(prepare_os, param);
	}

	// Create argument list
	ostringstream param_os;
	for (int i = 1; i < num_params; ++i) {
		ParmVarDecl *param = method->getParamDecl(i);
		param_os << ", ";
		print_argument(param_os, param);
	}

	// Handle result args
	ostringstream result_os;
	for (int i = 1; i < num_params; ++i) {
		const ParmVarDecl *param = method->getParamDecl(i);
		handle_result_argument(result_os, "Ctx", param);
	}

	ostringstream handle_error_os;
	printHandleErrorCall(handle_error_os, 2,
			     fullname + " returned a NULL pointer.", context);

	// Handle return
	ostringstream return_os;
	handle_return(return_os, method, "res");

	os << endl;
	print(os, "inline {0} {2}::{1}({3}) const {{\n"
		  "  {11}.lock();\n"
		  "  {2} self = as{2}();\n"
		  "  // Prepare arguments\n"
		  "{4}"
		  "  // Call {6}\n"
		  "  {5} res = {6}({7}{8});\n"
		  "  // Handle result argument(s)\n"
		  "{9}"
		  "  {11}.unlock();\n"
		  "  // Handle return\n"
		  "{10}"
		  "}}\n",
	      retName, cname, p_name, get_argument_decl_list(method, 1),
	      prepare_os.str(), retNameC, fullname,
	      isl_ptr(clazz.name, "self", takes(method->getParamDecl(0))),
	      param_os.str(), result_os.str(), return_os.str(), context);

	if (is_isl_class(method->getReturnType()) && can_copy(clazz)) {
		os << endl;
		print(
		    os, "/// @brief inplace variant\n"
			"inline void {1}::{0}Inplace({2}) {{\n"
			"  {10}.lock();\n"
			"  // Prepare arguments\n"
			"{3}"
			"  // Call {5}\n"
			"  This = (void *){5}(({4} *)This{7});\n"
			"  // Handle result argument(s)\n"
			"{8}"
			"  {10}.unlock();\n"
			"{9}"
			"}}\n",
		    cname, p_name, get_argument_decl_list(method, 1),
		    prepare_os.str(), clazz.name, fullname,
		    isl_ptr(clazz.name, "self", takes(method->getParamDecl(0))),
		    param_os.str(), result_os.str(), handle_error_os.str(),
		    context);
	}
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
 */
void cpp_generator::print_constructor(ostream &os, isl_class &clazz,
				      FunctionDecl *cons)
{
	const string fullname = cons->getName();
	const string cname = methodname2cpp(clazz, fullname);
	const string jclass = type2cpp(clazz.name);
	int num_params = cons->getNumParams();
	int drop_ctx = first_arg_is_isl_ctx(cons);
	int ctxSrc = find_context_source(cons);

	print(os, "  /// @brief Constructor for {0}\n"
		  "  ///\n",
	      fullname);

	if (!(ctxSrc > 0))
		drop_ctx = 0;

	for (int i = drop_ctx; i < num_params; ++i) {
		ParmVarDecl *param = cons->getParamDecl(i);
		print(os, "  /// @param {0}\n", param->getNameAsString());
	}

        print(os, "  static {0} {1}({2});\n", jclass, cname,
              get_argument_decl_list(cons, drop_ctx));
}

/**
 * @brief Print the constructor definition
 *
 * @param os
 * @param clazz
 * @param cons
 */
void cpp_generator::print_constructor_impl(ostream &os, isl_class &clazz,
					   FunctionDecl *cons)
{
	const string fullname = cons->getName();
	const string cname = methodname2cpp(clazz, fullname);
	const string jclass = type2cpp(clazz.name);
	string super;
	bool subclass = is_subclass(clazz.type, super);
	int num_params = cons->getNumParams();
	int drop_ctx = first_arg_is_isl_ctx(cons);
	int ctxSrc = find_context_source(cons);
	bool is_ctx = clazz.is_ctx();
	string context = (is_ctx) ? "That" : "_ctx";

	if (!(ctxSrc > 0))
		drop_ctx = 0;

	ostringstream prepare_os;
	for (int i = 0; i < num_params; ++i) {
		ParmVarDecl *param = cons->getParamDecl(i);
		prepare_argument(prepare_os, param);
	}

	string argument_list = get_argument_list(cons);
	string argument_decl_list = get_argument_decl_list(cons, drop_ctx);

	ostringstream handle_error_os;
	printHandleErrorCall(handle_error_os, 2,
			     fullname + " returned a NULL pointer.", context);

	// Handle result args
	ostringstream result_os;
	for (int i = 0; i < num_params; ++i) {
		const ParmVarDecl *param = cons->getParamDecl(i);
		handle_result_argument(result_os, "Ctx", param);
	}

	os << endl;
	print(os, "inline {0} {0}::{1}({2}) {{\n", jclass, cname,
	      argument_decl_list);
	if (ctxSrc >= 0) {
		const ParmVarDecl *param = cons->getParamDecl(ctxSrc);
		std::string context =
		    cons->getParamDecl(ctxSrc)->getNameAsString();
		if (!is_isl_ctx(param->getOriginalType())) {
			print(os, "  Ctx _ctx = {0}.Context();\n"
				  "  _ctx.lock();\n",
			      context);
		} else {
			print(os, "  Ctx _ctx = {0};\n"
				  "  _ctx.lock();\n",
			      context);
		}
	}
	print(os, "{0}"
		  "  {1} *That = {2}({3});\n"
		  "{5}\n"
		  "{4}\n",
	      prepare_os.str(), clazz.name, fullname, argument_list,
	      handle_error_os.str(), result_os.str());
	if (!clazz.is_ctx()) {
		print(os, "  _ctx.unlock();\n");
	}
	if (can_copy(clazz)) {
		print(os, "  return {0}(_ctx, That);\n", jclass);
	} else {
		print(os,
		      "  std::shared_ptr<ptr> _That(new ptr(That));\n"
		      "  return {0}({1}, _That);\n",
		      jclass, context);
	}

	print(os, "}}\n");
}

/**
 * @brief Generate all
 */
void cpp_generator::generate()
{
	generateClasses();
	generateEnums();
}

/**
 * @brief A list of non-copyable objects that are not declare uncopyable in isl.
 *
 * TODO: Fix this in isl, maybe?
 *
 * As we implement the isl_copy stuff via unwrap, we want to make sure to be
 * able to unwrap. So far only isl_schedule and isl_printer fail us.
 */
static set<string> NonCopyable = {"isl_ctx", "isl_schedule", "isl_printer"};
/**
 * @brief Return true, if the class name is in the NonCopyable set.
 *
 * @param clazz
 *
 * @return
 */
bool cpp_generator::can_copy(isl_class &clazz)
{
	return NonCopyable.count(clazz.name) == 0;
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
	set<pair<string, bool>> Deps = getDependencies(clazz);

	os << getGuardHeader(p_name) << endl;

	if (!IslExtraDeps.count(name))
		print(os, "#include \"isl/{}.h\"\n", getIncludeForIslObj(name));

	vector<string> extras = IslExtraDeps[name];
	for (string extra : extras) {
		print(os, "#include \"isl/{}.h\"\n",
		      getIncludeForIslObj(extra));
	}

	os << getIncludes(clazz) << endl;
	print(os, "#include \"isl/IslFnPtr.h\"\n"
		  "\n");
	print(os, "namespace isl {{\n"
		  "{0}"
		  "\n", getForwardDecls(Deps));
	if (subclass) {
		string base_class = type2cpp(super);
		print(os, "class {0} : public {1} {{\n", p_name, base_class);
	} else {
		print(os, "class {0} {{\n", p_name);
	}
	print(os, "protected:\n");

	if (!can_cp)
		print_ptr_wrapper(os, name);

	p->print_explicit_constructors_h(os);

	print(os, "\n"
		  "public:\n");

	if (!clazz.is_ctx() && !subclass) {
		print(os, "  const Ctx &Context() const {{ return ctx; }}\n");
	}

	p->print_api_wrapper_h(os);
	p->print_api_give_h(os);
	p->print_api_unwrapper_h(os);

	os << endl;

	for (in = clazz.constructors.begin(); in != clazz.constructors.end();
	     ++in) {
		print_constructor(os, clazz, *in);
	}

	// We do not free objects of classes that have in-place update
	// (e.g., isl_band). These values exist only in dependence of
	// parent objects and are freed when the parent object goes away.
	p->print_destructor_h(os);
	p->print_extra_methods_h(os);

	if (can_be_printed(clazz)) {
		p->print_print_methods_h(os);
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

	p->print_copy_constructor_h(os);
	p->print_copy_assignment_h(os);
	p->print_move_constructor_h(os);
	p->print_move_assignment_h(os);

	if (can_cp) {
		print(
		    os,
		    "  /// @brief Implement lt via pointer comparison of the\n"
		    "  ///        wrapped isl objects.\n"
		    "  bool operator<(const {0} &RHS) const {{ return This < "
		    "RHS.This; }}\n",
		    p_name);
		os << endl;
	}

	print(os, "}};\n"
		  "}} // namespace isl\n");
	// if (name.compare("isl_val") == 0)
	//  print_additional_val_methods(os);

	os << getGuardFooter(p_name);
}

/**
 * @brief Print the class implementation
 *
 * @param clazz
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
	      p_name, getIncludes(clazz, true));

	p->print_api_wrapper(os);
	p->print_copy_assignment(os);

	for (in = clazz.constructors.begin(); in != clazz.constructors.end();
	     ++in) {
		print_constructor_impl(os, clazz, *in);
	}

	// We do not free objects of classes that have in-place update
	// (e.g., isl_band). These values exist only in dependence of
	// parent objects and are freed when the parent object goes away.
	if (!is_inplace(clazz)) {
		p->print_destructor(os);
	}

	p->print_extra_methods(os);
	p->print_api_give(os);
	p->print_api_unwrapper(os);

	if (can_be_printed(clazz)) {
		p->print_print_methods(os);
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
void cpp_generator::generateClasses()
{
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
static void insertDep(set<pair<string, bool>> &Deps, pair<string, bool> Dep)
{
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
	set<pair<string, bool>> &Deps, QualType &Ty)
{
	if (is_isl_class(Ty) || is_isl_enum(Ty)) {
		string d_name = paramtype2cpp(Ty);
		bool includeRequired = is_isl_enum(Ty) || is_isl_ctx(Ty);
		if (d_name != p_name) {
			insertDep(Deps, make_pair(d_name, includeRequired));
		}
	} else if (is_isl_result_argument(Ty)) {
		QualType pTy = Ty->getPointeeType();
		insertIfDependency(p_name, Deps, pTy);
	} else if (Ty->isPointerType()) {
		QualType pTy = Ty->getPointeeType();
		if (pTy->isFunctionType()) {
			const FunctionProtoType *ft =
			    pTy->getAs<FunctionProtoType>();
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
set<pair<string, bool>> cpp_generator::getDependencies(isl_class &clazz)
{
	set<pair<string, bool>> Deps;
	set<clang::FunctionDecl *>::iterator it;
	set<clang::FunctionDecl *>::iterator ie;
	string p_name = type2cpp(clazz.name);

	if (can_be_printed(clazz))
		insertDep(Deps, make_pair("Printer", false));

	for (it = clazz.constructors.begin(), ie = clazz.constructors.end();
	     it != ie; ++it) {
		clang::FunctionDecl *C = *it;
		for (unsigned i = 0; i < C->getNumParams(); ++i) {
			const ParmVarDecl *P = C->getParamDecl(i);
			QualType Ty = P->getOriginalType();
			insertIfDependency(p_name, Deps, Ty);
		}
	}

	for (it = clazz.methods.begin(), ie = clazz.methods.end(); it != ie;
	     ++it) {
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
	if (subclass)
		insertDep(Deps, make_pair(type2cpp(super), true));
	else
		insertDep(Deps, make_pair("IslBase", true));


	// We need the isl::Format then
	if (can_be_printed(clazz)) {
		insertDep(Deps, make_pair("Format", true));
	}

	insertDep(Deps, make_pair("IslException", true));

	return Deps;
}

/**
 * @brief Print a super class for al isl objects.
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
	      "#include <iostream>\n"
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
 * @brief Print an enum.
 *
 * @param enu
 */
void cpp_generator::print_enum(const isl_enum &enu)
{
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
	      "#include <iostream>\n"
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
 * @brief Generate all enums.
 */
void cpp_generator::generateEnums()
{
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
	set<EnumDecl *> &enums) : generator(types, functions, enums)
{
}
