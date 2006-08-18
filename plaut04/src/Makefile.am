bin_PROGRAMS = plaut04
INCLUDES = -I@top_srcdir@/plaut04/includes  -Wno-deprecated 
LIBS=$(DEPS_LIBS)

plaut04_SOURCES = gReadBif.c++         gReadSol.c++          drawCoords.c++ \
	normalizeSolData.c++   normalizeBifData.c++   stringtrim.c++ \
	tube.c++             polygon3d.c++          readFM.c++             createBoundingBox.c++\
	createCoords.c++    axis.c++             createLegend.c++       gmain.c++ \
	rounding.c++

all-local: $(bin_PROGRAMS)
	mv $(bin_PROGRAMS) @top_srcdir@/bin
