# -- VARIABLES --
NAME=qry
# (locations)
SRC=src
TEST_SRC=test/src
BUILD=bin
DOC=doc
TEST_BUILD=test/bin
DIST=dist
# (compiler)
SCALAC=scalac
SCALADOC=scaladoc

default: Makefile ${DIST}/${NAME}.jar

# -- BUILD --
${DIST}/${NAME}.jar: $(wildcard ${SRC}/qry/*.scala)
	@echo "--------------------------------------------------------------------------------"
	@echo "                          BUILDING Qry.jar"
	@echo "--------------------------------------------------------------------------------"
	mkdir -p ${BUILD}
	mkdir -p ${DOC}
	mkdir -p ${DIST}
	#(compile)
	${SCALAC} -deprecation -d ${BUILD} `find ${SRC} -name "*.scala"`
	#(compile)
	${SCALADOC} -deprecation -d ${DOC} `find ${SRC} -name "*.scala"`
	#(jar)
	jar cf ${DIST}/${NAME}.jar -C $(BUILD) .
	jar uf ${DIST}/${NAME}.jar -C $(SRC) .

clean:
	rm -f ${DIST}/${NAME}.jar
	rmdir ${DIST} || [ ! -d ${DIST} ]
	rm -rf ${BUILD}/*
	rmdir ${BUILD} || [ ! -d ${BUILD} ]
	rm -rf ${DOC}/*
	rmdir ${DOC} || [ ! -d ${DOC} ]
