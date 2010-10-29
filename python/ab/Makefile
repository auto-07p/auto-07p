#
#   Makefile		3/1994		Xianjun WANG
#
PGM    = ab
FC     = f95
FFLAGS = -O
SRC    = $(PGM).f
EXE    = $(PGM).exe
LIBS   = $(AUTO_DIR)/lib/*.o
RM     = rm -f
#
all: clean $(EXE) run
#
$(EXE): $(SRC:.f=.o)
	$(FC) $(FFLAGS) $(SRC:.f=.o) -o $@ $(LIBS)
#
run: first second third fourth fifth
#
first:	$(EXE)
	@echo " "
	@echo "Demo $(PGM) is started"
	@echo " "
	@echo "$(PGM) : first run : stationary solutions"
	@cp c.$(PGM).1 fort.2
	@touch fort.3
	@./$(EXE)
	@cp fort.7 b.$(PGM)
	@cp fort.8 s.$(PGM)
	@cp fort.9 d.$(PGM)
	@echo " Saved as *.$(PGM)"
	@$(RM) fort.*
#
second:	$(EXE)
	@echo " "
	@echo "$(PGM) : second run : periodic solutions"
	@cp c.$(PGM).2 fort.2
	@cp s.$(PGM)   fort.3
	@./$(EXE)
	@cat fort.7 >> b.$(PGM)
	@cat fort.8 >> s.$(PGM)
	@cat fort.9 >> d.$(PGM)
	@echo " Appended to *.$(PGM)"
	@$(RM) fort.*
#
third:	$(EXE)
	@echo " "
	@echo "$(PGM) : third run : a 2-parameter locus of folds"
	@cp c.$(PGM).3 fort.2
	@cp s.$(PGM)   fort.3
	@./$(EXE)
	@cp fort.7 b.2p
	@cp fort.8 s.2p
	@cp fort.9 d.2p
	@echo " Saved as *.2p"
	@$(RM) fort.*
#
fourth:	$(EXE)
	@echo " "
	@echo "$(PGM) : fourth run : the locus of folds in reverse direction"
	@cp c.$(PGM).4 fort.2
	@cp s.$(PGM)   fort.3
	@./$(EXE)
	@cat fort.7 >> b.2p
	@cat fort.8 >> s.2p
	@cat fort.9 >> d.2p
	@echo " Appended to *.2p"
	@$(RM) fort.*
#
fifth:	$(EXE)
	@echo " "
	@echo "$(PGM) : fifth run : a 2-parameter locus of Hopf points"
	@cp c.$(PGM).5 fort.2
	@cp s.$(PGM)   fort.3
	@./$(EXE)
	@cat fort.7 >> b.2p
	@cat fort.8 >> s.2p
	@cat fort.9 >> d.2p
	@echo " Appended to *.2p"
	@$(RM) fort.*
	@echo " "
	@echo "Demo $(PGM) is done"
#
clean:
	@echo "Cleaning $(PGM) ..."
	@cp $(SRC) $(EXE)
	@$(RM) *.exe *.o b.* s.* d.* fort.* *~ core
	@cp c.$(PGM).1 c.$(PGM)
	@echo "Cleaning ... done"
 
