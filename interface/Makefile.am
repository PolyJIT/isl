AUTOMAKE_OPTIONS = nostdinc subdir-objects


noinst_PROGRAMS = extract_interface
if HAVE_JNI
lib_LTLIBRARIES = libisl_jni.la
endif

AM_CXXFLAGS = $(CLANG_CXXFLAGS) -std=c++11 -fno-rtti
AM_LDFLAGS = $(CLANG_LDFLAGS)

includes = -I$(top_builddir) -I$(top_srcdir) \
	-I$(top_builddir)/include -I$(top_srcdir)/include

extract_interface_CPPFLAGS = $(includes)
extract_interface_SOURCES = \
	generator.h \
	generator.cc \
	python.h \
	python.cc \
	java.h \
	java.cc \
	cpp.h \
	cpp.cc \
	extract_interface.h \
	extract_interface.cc
extract_interface_LDADD = \
	-lclangFrontend -lclangSerialization -lclangParse -lclangSema \
	$(LIB_CLANG_EDIT) \
	-lclangAnalysis -lclangAST -lclangLex -lclangBasic -lclangDriver \
	$(CLANG_LIBS) $(CLANG_LDFLAGS)

libisl_jni_la_SOURCES  = java/src/isl_jni.c
libisl_jni_la_CPPFLAGS = $(includes) -I$(JNI_INCLUDE)
libisl_jni_la_LIBADD   = $(top_builddir)/libisl.la
libisl_jni_la_LDFLAGS  = -version-info @versioninfo@

JAVAC_FLAGS=
SCALAC_FLAGS=-cp isl.jar

.PHONY: test
test: python/isl.py
	cat python/isl.py

python/isl.py: extract_interface$(EXEEXT)
	./extract_interface$(EXEEXT) $(includes) $(srcdir)/all.h \
		--output-dir=python --language=python

isl.py: isl.py.top python/isl.py
	cat $(srcdir)/isl.py.top python/isl.py >isl.py


isl.jar java/src/isl_jni.c: extract_interface$(EXEEXT) java/Manifest.txt
	./extract_interface$(EXEEXT) $(includes) $(srcdir)/all.h --language=java --output-dir=java/src
	$(MKDIR_P) java/target
	$(JAVAC) $(JAVAC_FLAGS) -d java/target `find java/src -name \*.java`
	$(JAR) -cfm isl.jar java/Manifest.txt -C java/target .

ISL_CXX_H=\
	isl/Aff.h \
	isl/Aff.hpp \
	isl/AffList.h \
	isl/AffList.hpp \
	isl/AstBuild.h \
	isl/AstBuild.hpp \
	isl/AstExpr.h \
	isl/AstExpr.hpp \
	isl/AstExprList.h \
	isl/AstExprList.hpp \
	isl/AstExprType.h \
	isl/AstNode.h \
	isl/AstNode.hpp \
	isl/AstNodeList.h \
	isl/AstNodeList.hpp \
	isl/AstNodeType.h \
	isl/AstOpType.h \
	isl/AstPrintOptions.h \
	isl/AstPrintOptions.hpp \
	isl/Band.h \
	isl/Band.hpp \
	isl/BandList.h \
	isl/BandList.hpp \
	isl/BasicMap.h \
	isl/BasicMap.hpp \
	isl/BasicSet.h \
	isl/BasicSet.hpp \
	isl/BasicSetList.h \
	isl/BasicSetList.hpp \
	isl/Constraint.h \
	isl/Constraint.hpp \
	isl/ConstraintList.h \
	isl/ConstraintList.hpp \
	isl/Context.h \
	isl/DimType.h \
	isl/Fold.h \
	isl/Format.h \
	isl/Id.h \
	isl/Id.hpp \
	isl/IdList.h \
	isl/IdList.hpp \
	isl/IdToAstExpr.h \
	isl/IdToAstExpr.hpp \
	isl/IslBase.h \
	isl/IslException.h \
	isl/IslFnPtr.h \
	isl/LocalSpace.h \
	isl/LocalSpace.hpp \
	isl/Map.h \
	isl/Map.hpp \
	isl/Mat.h \
	isl/Mat.hpp \
	isl/MultiAff.h \
	isl/MultiAff.hpp \
	isl/MultiPwAff.h \
	isl/MultiPwAff.hpp \
	isl/Point.h \
	isl/Point.hpp \
	isl/Printer.h \
	isl/Printer.hpp \
	isl/PwAff.h \
	isl/PwAff.hpp \
	isl/PwAffList.h \
	isl/PwAffList.hpp \
	isl/PwMultiAff.h \
	isl/PwMultiAff.hpp \
	isl/PwQpolynomial.h \
	isl/PwQpolynomial.hpp \
	isl/PwQpolynomialFold.h \
	isl/PwQpolynomialFold.hpp \
	isl/Qpolynomial.h \
	isl/Qpolynomial.hpp \
	isl/QpolynomialFold.h \
	isl/QpolynomialFold.hpp \
	isl/Schedule.h \
	isl/Schedule.hpp \
	isl/Set.h \
	isl/Set.hpp \
	isl/SetList.h \
	isl/SetList.hpp \
	isl/Space.h \
	isl/Space.hpp \
	isl/Term.h \
	isl/Term.hpp \
	isl/UnionMap.h \
	isl/UnionMap.hpp \
	isl/UnionPwMultiAff.h \
	isl/UnionPwMultiAff.hpp \
	isl/UnionPwQpolynomial.h \
	isl/UnionPwQpolynomial.hpp \
	isl/UnionPwQpolynomialFold.h \
	isl/UnionPwQpolynomialFold.hpp \
	isl/UnionSet.h \
	isl/UnionSet.hpp \
	isl/Val.h \
	isl/Val.hpp \
	isl/ValList.h \
	isl/ValList.hpp \
	isl/Vec.h \
	isl/Vec.hpp

isl.cpp: extract_interface$(EXEEXT) $(srcdir)/all.h
	$(AM_V_GEN)./extract_interface$(EXEEXT) $(includes) $(srcdir)/all.h \
	  --language=c++ --output-dir=isl

pkginclude_HEADERS= $(ISL_CXX_H)

java/Manifest.txt:
	$(MKDIR_P) java
	echo "" >$@

isl-scala.jar: isl.jar scala/src/isl/*.scala
	$(CP) isl.jar isl-scala.jar
	$(MKDIR_P) scala/target
	$(SCALAC) $(SCALAC_FLAGS) -d scala/target `find scala/src -name \*.scala`
	$(JAR) -uf $@ -C scala/target .

.PHONY: clean
clean:
	$(RM) -rf python java/src java/target scala/target
	$(RM) -f isl.py java/Manifest.txt isl.jar isl-scala.jar *.o
	$(RM) -rf isl

dist-hook: isl.py
	cp isl.py $(distdir)/
