AUTOMAKE_OPTIONS = nostdinc

noinst_PROGRAMS = extract_interface

AM_CXXFLAGS = $(CLANG_CXXFLAGS) -fno-rtti
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
	extract_interface.h \
	extract_interface.cc
extract_interface_LDADD = \
	-lclangFrontend -lclangSerialization -lclangParse -lclangSema \
	$(LIB_CLANG_EDIT) \
	-lclangAnalysis -lclangAST -lclangLex -lclangBasic -lclangDriver \
	$(CLANG_LIBS) $(CLANG_LDFLAGS)

JAVAC_FLAGS=-cp $(JNA_JAR)
SCALAC_FLAGS=-cp isl.jar:$(JNA_JAR)

.PHONY: test
test: isl.py
	cat isl.py

isl.py: extract_interface$(EXEEXT) isl.py.top
	./extract_interface$(EXEEXT) $(includes) $(srcdir)/all.h --output-dir=python --language=python
	cat isl.py.top python/isl.py > isl.py

isl.jar: extract_interface$(EXEEXT) java/Manifest.txt
	./extract_interface$(EXEEXT) $(includes) $(srcdir)/all.h --language=java --output-dir=java/src
	$(MKDIR_P) java/target
	$(JAVAC) $(JAVAC_FLAGS) -d java/target `find java/src -name \*.java`
	$(JAR) -cfm $@ java/Manifest.txt -C java/target .

java/Manifest.txt:
	$(MKDIR_P) java
	echo "Class-Path: $(JNA_JAR)" >$@

isl-scala.jar: isl.jar scala/src/isl/*.scala
	$(CP) isl.jar isl-scala.jar
	$(MKDIR_P) scala/target
	$(SCALAC) $(SCALAC_FLAGS) -d scala/target `find scala/src -name \*.scala`
	$(JAR) -uf $@ -C scala/target .

.PHONY: clean
clean:
	$(RM) -rf python java/src java/target scala/target
	$(RM) -f isl.py java/Manifest.txt isl.jar isl-scala.jar *.o

dist-hook: isl.py
	cp $^ $(distdir)/
