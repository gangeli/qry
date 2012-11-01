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
SCALAC=/home/gabor/programs/scala/bin/scalac
SCALADOC=/home/gabor/programs/scala/bin/scaladoc

default: ${DIST}/${NAME}.jar

# -- BUILD --
${DIST}/${NAME}.jar: $(wildcard ${SRC}/qry/*.scala) Makefile
	@echo "--------------------------------------------------------------------------------"
	@echo "                          BUILDING Qry.jar"
	@echo "--------------------------------------------------------------------------------"
	mkdir -p ${BUILD}
	mkdir -p ${DIST}
	#(compile)
	${SCALAC} -deprecation -d ${BUILD} `find ${SRC} -name "*.scala"`
	#(jar)
	jar cf ${DIST}/${NAME}.jar -C $(BUILD) .
	jar uf ${DIST}/${NAME}.jar -C $(SRC) .

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
