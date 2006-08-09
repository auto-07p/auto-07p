#===> Makefile
#
#   Makefile originally written by Xianjun WANG in March 1994.
#
SHELL=/bin/sh
#
DIRM=src plaut tek2ps
DIRC=gui plaut tek2ps
DIR1=gui plaut test
DIR2=bin 
DIR3=lib
#
cmd:
	@for d in ${DIRM}; do \
	cd $$d; make; cd ..; \
	done
#
clean:
	@for d in ${DIRC}; do \
	cd $$d; echo `pwd`; make clean; cd ..; \
	done
#
superclean:
	@for d in ${DIR1}; do \
	cd $$d; echo `pwd`; make clean; cd ..; \
	done
	@for d in ${DIR2}; do \
	cd $$d; echo `pwd`; rm -f AUTO97 plaut autlab 86to97 94to97 double triple; cd ..; \
	done
	@for d in ${DIR3}; do \
	cd $$d; echo `pwd`; rm -f *.o; cd ..; \
	done
	@echo "Super cleaning ... done"
