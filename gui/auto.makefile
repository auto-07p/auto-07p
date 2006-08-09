#===> auto.makefile
#
#   Makefile written by Xianjun WANG in March 1994
#
#   This Makefile is called from the Graphic User Interface (GUI)
#
#   The following environment variables are controlled in GUI program 
#   at run time:
#
#   PROGRAMNAME        :     the name of the program
#   NEW_PROGRAMNAME    :     the output data name saved by "save as ..." button
#   NEW_PLOTNAME       :     the output data name to be plotted by "plot name ..." button
#   RESTARTNAME        :     the restart data name to be set to unit 3 (fort.3)
#   RESTARTDATA        :     same as RESTARTNAME, but only needed in GUI program
#   COPYFROM           :     the name of the output data to be copied from
#   COPYTO             :     the name of the output data to be copied to
#   MOVEFROM           :     the name of the output data to be moved from
#   MOVETO             :     the name of the output data to be moved to
#   APPENDFROM         :     the name of the output data to be appended from
#   APPENDTO           :     the name of the output data to be appended to
#   DELETEDATA         :     the name of the output data to be deleted
#   
#
.IGNORE:
#
FC    	   = f77
FFLAGS 	   = -c
OPT    	   = -O
SRC        = $(PROGRAMNAME).f
OBJ        = $(PROGRAMNAME).o
EXE        = $(PROGRAMNAME).exe
LIBS       = $(AUTO_DIR)/lib/*.o
RM         = rm -f
#
start: $(EXE) run
#
run:
	@echo " "
	@echo "Starting $(PROGRAMNAME) ..."
	@echo " "
	@cp r.$(PROGRAMNAME) fort.2
	@$(EXE)
	@rm -f fort.3
	@echo " "
	@echo "$(PROGRAMNAME) ... done"
#
restart: $(EXE) rerun
rerun:
	@echo " "
	@echo "Restarting $(PROGRAMNAME) ..."
	@echo " "        
	@cp r.$(PROGRAMNAME) fort.2
	@cp q.$(RESTARTNAME) fort.3
	@$(EXE)
	@rm -f fort.3
	@echo " " 
	@echo "$(PROGRAMNAME) ... done"
#
$(EXE): $(OBJ)
	$(FC) $(OPT) $(OBJ) -o $@ $(LIBS)
#
$(OBJ): $(SRC)
	$(FC) $(FFLAGS) $(OPT) $(SRC) -o $@
#
save:
	@echo " "
	@echo "Starting save ..."
	@cp fort.7  p.$(PROGRAMNAME)
	@echo "Saved fort.7 as p.$(PROGRAMNAME)"
	@cp fort.8  q.$(PROGRAMNAME)
	@echo "Saved fort.8 as q.$(PROGRAMNAME)"
	@cp fort.9  d.$(PROGRAMNAME)
	@echo "Saved fort.9 as d.$(PROGRAMNAME)"
	@echo "Saving ... done"
	@echo " "
#
saveas:
	@echo " "
	@echo "Starting save ..."
	@cp fort.7  p.$(NEW_PROGRAMNAME)
	@echo "Saved fort.7 as p.$(NEW_PROGRAMNAME)"
	@cp fort.8  q.$(NEW_PROGRAMNAME)
	@echo "Saved fort.8 as q.$(NEW_PROGRAMNAME)"
	@cp fort.9  d.$(NEW_PROGRAMNAME)
	@echo "Saved fort.9 as d.$(NEW_PROGRAMNAME)"
	@cp fort.2  r.$(NEW_PROGRAMNAME)
	@echo "Saved fort.2 as r.$(NEW_PROGRAMNAME)"
	@echo "Saving ... done"
	@echo " "
#
append:
	@echo " "
	@echo "Starting append ..."
	@cat fort.7 >> p.$(PROGRAMNAME)
	@echo "Appended fort.7 to p.$(PROGRAMNAME)"
	@cat fort.8 >> q.$(PROGRAMNAME)
	@echo "Appended fort.8 to q.$(PROGRAMNAME)"
	@cat fort.9 >> d.$(PROGRAMNAME)
	@echo "Appended fort.9 to d.$(PROGRAMNAME)"
	@echo "Appending ... done"
	@echo " "
#
appendto:
	@echo " "
	@echo "Starting append ..."
	@cat fort.7 >> p.$(NEW_PROGRAMNAME)
	@echo "Appended fort.7 to p.$(NEW_PROGRAMNAME)"
	@cat fort.8 >> q.$(NEW_PROGRAMNAME)
	@echo "Appended fort.8 to q.$(NEW_PROGRAMNAME)"
	@cat fort.9 >> d.$(NEW_PROGRAMNAME)
	@echo "Appended fort.9 to d.$(NEW_PROGRAMNAME)"
	@echo "Appending ... done"
	@echo " "
#
plot_current:
	@echo " "
	@echo "Starting plaut ..."
	@cp p.$(PROGRAMNAME) fort.17
	@cp q.$(PROGRAMNAME) fort.18
	@echo "Plotting p.$(PROGRAMNAME) and q.$(PROGRAMNAME) ..."
	@xterm -bg black -fg white -t -e $(AUTO_DIR)/bin/plaut
	@$(RM) fort.17
	@$(RM) fort.18
	@echo "Plotting ... done"
	@echo " "
#
plot_other:
	@echo " "
	@echo "Starting plaut ..."
	@cp p.$(NEW_PLOTNAME) fort.17
	@cp q.$(NEW_PLOTNAME) fort.18
	@echo "Plotting p.$(NEW_PLOTNAME) and q.$(NEW_PLOTNAME) ..."
	@xterm -bg black -fg white -t -e $(AUTO_DIR)/bin/plaut
	@$(RM) fort.17
	@$(RM) fort.18
	@echo "Plotting ... done"
	@echo " "
#
restartdata:
	@echo " "
	@echo "Setting q.$(RESTARTDATA) as restart file ... done"
#
copydata:
	@echo " "
	@cp p.$(COPYFROM) p.$(COPYTO)
	@echo "Copying p.$(COPYFROM) to p.$(COPYTO) ... done"
	@cp q.$(COPYFROM) q.$(COPYTO)
	@echo "Copying q.$(COPYFROM) to q.$(COPYTO) ... done"
	@cp d.$(COPYFROM) d.$(COPYTO)
	@echo "Copying d.$(COPYFROM) to d.$(COPYTO) ... done"
	@cp r.$(COPYFROM) r.$(COPYTO)
	@echo "Copying r.$(COPYFROM) to r.$(COPYTO) ... done"
#
appenddata:
	@echo " "
	@cat p.$(APPENDFROM) >> p.$(APPENDTO)
	@echo "Appending p.$(APPENDFROM) to p.$(APPENDTO) ... done"
	@cat q.$(APPENDFROM) >> q.$(APPENDTO)
	@echo "Appending q.$(APPENDFROM) to q.$(APPENDTO) ... done"
	@cat d.$(APPENDFROM) >> d.$(APPENDTO)
	@echo "Appending d.$(APPENDFROM) to d.$(APPENDTO) ... done"
#
movedata:
	@echo " "
	@mv -f p.$(MOVEFROM) p.$(MOVETO)
	@echo "Moving p.$(MOVEFROM) to p.$(MOVETO) ... done"
	@mv -f q.$(MOVEFROM) q.$(MOVETO)
	@echo "Moving q.$(MOVEFROM) to q.$(MOVETO) ... done"
	@mv -f d.$(MOVEFROM) d.$(MOVETO)
	@echo "Moving d.$(MOVEFROM) to d.$(MOVETO) ... done"
	@mv -f r.$(MOVEFROM) r.$(MOVETO)
	@echo "Moving r.$(MOVEFROM) to r.$(MOVETO) ... done"
#
deletedata:
	@echo " "
	@$(RM) p.$(DELETEDATA)
	@echo "Deleting p.$(DELETEDATA) ... done"
	@$(RM) q.$(DELETEDATA)
	@echo "Deleting q.$(DELETEDATA) ... done"
	@$(RM) d.$(DELETEDATA)
	@echo "Deleting d.$(DELETEDATA) ... done"
#
clean:
	@echo " "
	@echo "Cleaning ..."
	@$(RM) *.o fort.* *.exe *.trace core
	@echo "Cleaning ... done"
	@echo " "
#
# DO NOT ADD ANYTHING AFTER THIS LINE
#
