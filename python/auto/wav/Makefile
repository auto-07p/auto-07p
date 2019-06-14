#
#   Makefile		3/1994		Xianjun WANG
#
PGM    = wav
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
#run: first second third fourth fifth sixth
run: first second third fourth fifth
#
first:	$(EXE)
	@echo " "
	@echo "Demo $(PGM) is started"
	@echo " "
	@echo "$(PGM) : first run : stationary solutions of the ODE"
	@cp c.$(PGM).1 fort.2
	@touch fort.3
	@./$(EXE)
	@echo " Saved as *.ode"
	@cp fort.7 b.ode
	@cp fort.8 s.ode
	@cp fort.9 d.ode
	@$(RM) fort.*
#
second:	$(EXE)
	@echo " "
	@echo "$(PGM) : second run : stationary solutions of the reduced PDE"
	@cp c.$(PGM).2 fort.2
	@./$(EXE)
	@cat fort.7 > b.$(PGM)
	@cat fort.8 > s.$(PGM)
	@cat fort.9 > d.$(PGM)
	@echo " Saved as *.$(PGM)"
	@$(RM) fort.*
#
third:	$(EXE)
	@echo " "	
	@echo "$(PGM) : third run : wave train solutions of fixed wave speed"
	@cp c.$(PGM).3 fort.2
	@cp s.$(PGM)   fort.3
	@./$(EXE)
	@cat fort.7 >> b.$(PGM)
	@cat fort.8 >> s.$(PGM)
	@cat fort.9 >> d.$(PGM)
	@echo " Appended to *.$(PGM)"
	@$(RM) fort.*
#
fourth:	$(EXE)
	@echo " "
	@echo "$(PGM) : fourth run : fixed wave length waves on a ring"
	@cp c.$(PGM).4 fort.2
	@cp s.$(PGM)   fort.3
	@./$(EXE)
	@cat fort.7 > b.rng
	@cat fort.8 > s.rng
	@cat fort.9 > d.rng
	@echo " Saved as *.rng"
	@$(RM) fort.*
#
fifth:	$(EXE)
	@echo " "
	@echo "$(PGM) : fifth run : time evolution computation"
	@cp c.$(PGM).5 fort.2
	@cp s.$(PGM)   fort.3
	@./$(EXE)
	@cat fort.7 > b.tim
	@cat fort.8 > s.tim
	@cat fort.9 > d.tim
	@echo " Saved as *.tim"
	@$(RM) fort.*
	@echo " "
	@echo "Demo $(PGM) is done"
#
sixth:	$(EXE)
	@echo " "
	@echo "$(PGM) : sixth run : stationary wave continuation"
	@cp c.$(PGM).6 fort.2
	@cp s.$(PGM)   fort.3
	@./$(EXE)
	@cat fort.7 >> b.$(PGM)
	@cat fort.8 >> s.$(PGM)
	@cat fort.9 >> d.$(PGM)
	@echo " Appended to *.$(PGM)"
	@$(RM) fort.*
 
#
clean:
	@echo "Cleaning $(PGM) ..."
	@cp $(SRC) $(EXE)
	@$(RM) *.exe *.o b.* s.* d.* fort.* *~ core
	@cp c.$(PGM).1 c.$(PGM)
	@echo "Cleaning ... done"
 
