# ===> Makefile
#
#   Makefile            Xianjun WANG            3/1994
#
#   used to compile AUTO libraries
#
#
FC = f77 -O -c
OBJ= autlib1.o autlib2.o autlib3.o autlib4.o autlib5.o eispack.o
RM=rm -f
#
all: header compile autlab double triple kpbif listlabels reduce relabel zero move
#
header: 
	@cp ../include/auto.h ./auto.h
#
compile: $(OBJ)
#
autlib1.o: autlib1.f auto.h
	$(FC) autlib1.f -o $@
#
autlib2.o: autlib2.f auto.h
	$(FC) autlib2.f -o $@
#
autlib3.o: autlib3.f auto.h
	$(FC) autlib3.f -o $@
#
autlib4.o: autlib4.f auto.h
	$(FC) autlib4.f -o $@
#
autlib5.o: autlib5.f auto.h
	$(FC) autlib5.f -o $@
#
autlab: autlab.f auto.h
	f77 -O autlab.f -o ../bin/autlab
#
double: double.f auto.h
	f77 -O double.f -o ../bin/double
#
triple: triple.f auto.h
	f77 -O triple.f -o ../bin/triple
#
kpbif: kpbif.f auto.h
	f77 -O kpbif.f -o ../bin/kpbif
#
listlabels: listlabels.f auto.h
	f77 -O listlabels.f -o ../bin/listlabels
#
reduce: reduce.f auto.h
	f77 -O reduce.f -o ../bin/reduce
#
relabel: relabel.f auto.h
	f77 -O relabel.f -o ../bin/relabel
#
x: zero.f auto.h
	f77 -O zero.f -o ../bin/zero	
#
eispack.o: eispack.f
	$(FC) eispack.f -o $@
#
move: $(OBJ)
	@mv $(OBJ) ../lib
	@$(RM) auto.h
#
#  DO NOT ADD ANYTHING AFTER THIS LINE
#
