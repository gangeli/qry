# -- VARIABLES --
NAME=qry
# (locations)
SRC=src
TEST_SRC=test/src
BUILD=bin
DOC=doc
TEST_BUILD=test/bin
DIST=dist
TMP=tmp
# (compiler)
SCALAC=scalac
SCALADOC=scaladoc
# (paths)
PLUGINS_CP=plugin_lib/typesafe-config-1.0.0.jar

default: ${DIST}/${NAME}.jar

${DIST}/qry_unbundled.jar: $(wildcard ${SRC}/qry/*.scala) Makefile
	@echo "--------------------------------------------------------------------------------"
	@echo "                          BUILDING Qry.jar"
	@echo "--------------------------------------------------------------------------------"
	mkdir -p ${BUILD}
	mkdir -p ${DIST}
	#(compile)
	@${SCALAC} -deprecation -cp ${PLUGINS_CP}:${CLASSPATH} -d ${BUILD} `find ${SRC} -name "*.scala"`
	#(jar)
	jar cfm ${DIST}/${NAME}.jar ${SRC}/Manifest -C $(BUILD) .
	jar uf ${DIST}/${NAME}.jar -C $(SRC) .
	cp ${DIST}/${NAME}.jar ${DIST}/${NAME}_unbundled.jar

# -- BUILD --
${DIST}/${NAME}.jar: ${DIST}/qry_unbundled.jar ${SRC}/Manifest
	@echo "--------------------------------------------------------------------------------"
	@echo "                          BUNDLING Qry.jar"
	@echo "--------------------------------------------------------------------------------"
  #(dependencies)
	mkdir -p ${TMP}
  #((scala-library))
	rm -rf ${TMP}/scala-library
	unzip ${SCALA_HOME}/lib/scala-library.jar -d ${TMP}/scala-library > /dev/null
	rm -r ${TMP}/scala-library/META-INF
	jar uf ${DIST}/qry.jar -C ${TMP}/scala-library/ .
	rm -rf ${TMP}/scala-library
  #((scala-compiler))
	rm -rf ${TMP}/scala-compiler
	unzip ${SCALA_HOME}/lib/scala-compiler.jar -d ${TMP}/scala-compiler > /dev/null
	rm -r ${TMP}/scala-compiler/META-INF
	jar uf ${DIST}/qry.jar -C ${TMP}/scala-compiler/ .
	rm -rf ${TMP}/scala-compiler
  #((scala-reflect))
	rm -rf ${TMP}/scala-reflect
	unzip ${SCALA_HOME}/lib/scala-reflect.jar -d ${TMP}/scala-reflect > /dev/null
	rm -r ${TMP}/scala-reflect/META-INF
	jar uf ${DIST}/qry.jar -C ${TMP}/scala-reflect/ .
	rm -rf ${TMP}/scala-reflect
  #((jline))
	rm -rf ${TMP}/jline
	unzip ${SCALA_HOME}/lib/jline.jar -d ${TMP}/jline > /dev/null
	rm -r ${TMP}/jline/META-INF
	jar uf ${DIST}/qry.jar -C ${TMP}/jline/ .
	rm -rf ${TMP}/jline
  #(plugins)
  #((typesafe))
	rm -rf ${TMP}/typesafe
	unzip plugin_lib/typesafe-config-1.0.0.jar -d ${TMP}/typesafe > /dev/null
	rm -r ${TMP}/typesafe/META-INF
	jar uf ${DIST}/qry.jar -C ${TMP}/typesafe/ .
	rm -rf ${TMP}/typesafe

doc:
	mkdir -p ${DOC}
	#(compile)
	${SCALADOC} -deprecation -d ${DOC} `find ${SRC} -name "*.scala"`

clean:
	rm -f ${DIST}/${NAME}.jar
	rmdir ${DIST} || [ ! -d ${DIST} ]
	rm -rf ${BUILD}/*
	rmdir ${BUILD} || [ ! -d ${BUILD} ]
	rm -rf ${DOC}/*
	rmdir ${DOC} || [ ! -d ${DOC} ]
