#===> cmds.make
#
#   Makefile written by Xianjun WANG in March 1994
#
#   This Makefile is used under command mode
#
#   EQUATION_NAME      :     AUTO-Equation name
#
.IGNORE:
#
FC    	   = f77
FFLAGS 	   = -c
OPT    	   = -O
SRC        = $(EQUATION_NAME).f
OBJ        = $(EQUATION_NAME).o
EXE        = $(EQUATION_NAME).exe
LIBS       = $(AUTO_DIR)/lib/*.o
RM         = rm -f
# @rn
run: $(EXE)
	@echo "Starting $(EQUATION_NAME) ..."
	@$(EXE)
	@rm -f fort.2 fort.3
	@echo "$(EQUATION_NAME) ... done"
#
$(EXE): $(OBJ) $(LIBS)
	$(FC) $(OPT) $(OBJ) -o $@ $(LIBS)
#
$(OBJ): $(SRC)
	$(FC) $(FFLAGS) $(OPT) $(SRC) -o $@

#
# DO NOT ADD ANYTHING AFTER THIS LINE
#
