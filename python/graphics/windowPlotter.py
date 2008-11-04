#! /usr/bin/env python

import Tkinter
import Pmw
import AUTOutil
import plotter
import types
import string

# FIXME:  No regression tester (except as part of interactiveBindings)
class WindowPlotter(Pmw.MegaToplevel):
    def __init__(self,grapherClass,parent=None,cnf={},**kw):
        optiondefs = []
        self.defineoptions(AUTOutil.cnfmerge((cnf,kw)),optiondefs)
        Pmw.MegaToplevel.__init__(self, parent)

        interior = self.interior()
        self.helpBalloon = self.createcomponent('helpBalloon',
                                                (), None,
                                                Pmw.Balloon, interior)
                
        self.menuBar = self.createcomponent('menuBar',
                                            (), None,
                                            Pmw.MenuBar,interior,
                                            hotkeys="true",
                                            hull_relief = 'raised',
                                            hull_borderwidth = 1,
                                            balloon=self.helpBalloon)
        self.menuBar.addmenu("File","File operations")
        self.menuBar.addmenu("Options","View and set options")
        self.menuBar.addmenu("Help","View help on the plotting widget",side="right")
        self.menuBar.pack(fill=Tkinter.X)

        topbox = Tkinter.Frame(interior,relief="raised",borderwidth=2)
        topbox.pack(side=Tkinter.BOTTOM)

        self.grapher = self.createcomponent('grapher',
                                            (), None,
                                            grapherClass,interior)

        self.menuBar.addmenuitem("File",
                                 "command",
                                 "Save the plot as postscript...",
                                 label = "Save Postscript...",
                                 command = self.grapher.generatePostscript
                                 )
        self.menuBar.addmenuitem("File",
                                 "command",
                                 "Destroy the plot",
                                 label = "Quit",
                                 command = self.destroywindow
                                 )
        self.menuBar.addmenuitem("Options",
                                 "command",
                                 label="Options...",
                                 command=self._setOptionWindow
                                 )


        box = Tkinter.Frame(topbox)

        self.grapher.pack(expand=1,fill=Tkinter.BOTH)
        if self.grapher.cget("type") == "bifurcation":
            labelEntry = self.createcomponent('labelEntry',
                                              (), None,
                                              Pmw.OptionMenu,box,
                                              labelpos="w",
                                              label_text="Type",
                                              items=("'bifurcation'","'solution'"))
        else:
            labelEntry = self.createcomponent('labelEntry',
                                              (), None,
                                              Pmw.OptionMenu,box,
                                              labelpos="w",
                                              label_text="Type",
                                              items=("'solution'","'bifurcation'"))
            
        labelEntry.grid(row=0,column=0)
        labelEntry.configure(command = lambda value,obj=self:obj._modifyOption("type",value))
        self.labelEntry = labelEntry

        typeEntry = self.createcomponent('typeEntry',
                                         (), None,
                                         Pmw.ComboBox,box,
                                         labelpos="w",
                                         label_text="Label")
        
        typeEntry.grid(row=0,column=1)
        typeEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("label",entry))
        self.typeEntry = typeEntry
        box.grid(row=0)

        box = Tkinter.Frame(topbox)
        self._extraButtons(box)
        box.grid(row=1)
        
        # Let the appropriate things grow
        topbox.rowconfigure(1,weight=1)
        topbox.columnconfigure(0,weight=1)

        self.initialiseoptions(WindowPlotter)

    def __labelFunction(self,list):
        # The return value of a ScrolledListBox is a list of strings, so we change them
        # to integers here
        list=map(int,list)
        # but the _modifyOption method expects a string, so we change the whole thing to a string here
        self._modifyOption("label",str(list))

    def _setOptionWindow(self):
        keys = self.grapher.configure().keys()
        keys.sort()
        list = []
        for key in keys:
            if self.grapher._isInternalOption(key):
                list.append(key)
        self.optionSelctionDialog = Pmw.SelectionDialog(title = 'Options',
                                                        buttons = ('OK', 'Cancel'),
                                                        defaultbutton = 'OK',
                                                        scrolledlist_labelpos = 'n',
                                                        label_text = 'Setable options',
                                                        scrolledlist_items = list,
                                                        command = self.optionSelectionDialogCommand)
        
    def _extraButtons(self,box):
        pass

    def optionSelectionDialogCommand(self,button):
        if button == "OK":
            self.setOptionDialog(self.optionSelctionDialog.getcurselection()[0])
        if button == "Cancel":
            self.optionSelctionDialog.destroy()

    def _shortstr(self,list):
        return "[" + string.join(map(str,list),",") + "]"
        
    def setOptionDialog(self,key):
        self.diag = Pmw.Dialog(self.interior(),
                          buttons=("Ok","Cancel"))
        optionLabel = Pmw.EntryField(self.diag.interior(),
                                         labelpos="w",
                                         label_text="Option Name",
                                         entry_state=Tkinter.DISABLED)
        optionLabel.pack(side="top")

        valueLabel = Pmw.EntryField(self.diag.interior(),
                                         labelpos="w",
                                         label_text="Old Value",
                                         entry_state=Tkinter.DISABLED)
        valueLabel.pack(side="top")

        valueEntry = Pmw.EntryField(self.diag.interior(),
                                    labelpos="w",
                                    label_text="New Value")
        valueEntry.pack(side="top")
        valueEntry.configure(command = lambda button="Ok",entry=valueEntry,data=key,obj=self:obj.__dialogFunction(button,data,entry.get()))

        self.diag.configure(command = lambda button,entry=valueEntry,data=key,obj=self:obj.__dialogFunction(button,data,entry.get()))
        

        optionLabel.setentry(key)
        valueLabel.setentry(str(self.grapher.cget(key)))
        valueEntry.clear()
        if type(self.grapher.cget(key)) == types.IntType:
            valueEntry.configure(validate={"validator":"integer"})
        elif type(self.grapher.cget(key)) == types.FloatType:
            valueEntry.configure(validate={"validator":"real"})
        elif type(self.grapher.cget(key)) == types.StringType:
            pass

    def __dialogFunction(self,button,key,entry):
        if button == "Ok":
            self._modifyOption(key,entry)
        self.diag.destroy()
        
    def _modifyOption(self,key,entry):
        try:
            self.grapher[key] = eval(entry,{},{})
        except (NameError,SyntaxError):
            entry = string.strip(entry)
            entry = string.replace(entry,"[","['")
            entry = string.replace(entry,"]","']")
            entry = string.replace(entry,",","','")
            if entry == "" or entry[0] != '[':
                entry = "'" + entry + "'"
            self.grapher[key] = eval(entry,{},{})
        if key == "type":
            self.typeUpdateCallback()

    def typeUpdateCallback(self):
        pass

    def config(self,cnf=None,**kw):
        if type(cnf) == types.StringType or (cnf is None and len(kw) == 0):
            return self.grapher.config(cnf)
        dict = AUTOutil.cnfmerge((cnf,kw))
        self.grapher.config(dict)
        for key,value in dict.items():
            if key == "type":
                self.labelEntry.setvalue(value)
            if key in ["type","label","label_defaults",
                       "bifurcation_diagram_filename",
                       "solution_filename",
                       "bifurcation_column_defaults",
                       "bifurcation_diagram","bifurcation_x","bifurcation_y",
                       "bifurcation_coordnames",
                       "solution_column_defaults",
                       "solution_indepvarname","solution_coordnames",
                       "solution","solution_x","solution_y"]:
                self.typeUpdateCallback()

    configure = config

    def cget(self,option):
        return self.grapher.cget(option)

    def __getitem__(self,key):
        try:
            return self.grapher[key]
        except:
            return Pmw.MegaToplevel.__getitem__(self,key)

    def update(self):
        self.grapher.update()
        Pmw.MegaToplevel.update(self)

    def destroywindow(self):
        self.destroy()

class WindowPlotter2D(WindowPlotter):
    def __init__(self,parent=None,cnf={},**kw):
        WindowPlotter.__init__(self,plotter.plotter,parent,AUTOutil.cnfmerge((cnf,kw)))

    def _extraButtons(self,box):
        self.xEntry = self.createcomponent('xEntry',
                                      (), None,
                                      Pmw.ComboBox,box,
                                      labelpos="w",
                                      label_text="X")
        self.xEntry.grid(row=0,column=0)

        self.yEntry = self.createcomponent('yEntry',
                                      (), None,
                                      Pmw.ComboBox,box,
                                      labelpos="w",
                                      label_text="Y")
        self.yEntry.grid(row=0,column=1)

        self.typeUpdateCallback()

    def typeUpdateCallback(self):
        if self.grapher.cget("type") == "bifurcation":
            ox = "bifurcation_x"
            oy = "bifurcation_y"
            ocd = "bifurcation_column_defaults"
            o = "bifurcation_diagram"
        else:
            ox = "solution_x"
            oy = "solution_y"
            ocd = "solution_column_defaults"
            o = "solution"
        self.xEntry.configure(selectioncommand = lambda entry,
                              obj=self,oa=ox:obj._modifyOption(oa,entry))
        self.yEntry.configure(selectioncommand = lambda entry,
                              obj=self,oa=oy:obj._modifyOption(oa,entry))

        lst = []
        if not(self.grapher.cget(ocd) is None):
            for x in self.grapher.cget(ocd):
                lst.append(str(x))
        sol = self.grapher.cget(o)
        if self.grapher.cget("type") == "solution":
            indepvarname = ""
            if hasattr(sol,"indepvarname"):
                indepvarname = sol.indepvarname
            if self.grapher.cget(ox[:-1]+"indepvarname"):
                indepvarname = self.grapher.cget(ox[:-1]+"indepvarname")
            if indepvarname != "":
                lst.append("[%s]"%indepvarname)
        coordnames = []
        if hasattr(sol,"coordnames"):
            coordnames = sol.coordnames
        elif len(sol) > 0:
            coordnames = sol[0].coordnames
        if self.grapher.cget(ox[:-1]+"coordnames"):
            coordnames = self.grapher.cget(ox[:-1]+"coordnames")
        for s in coordnames:
            lst.append("[%s]"%s)
        self.xEntry.setlist(lst)
        self.yEntry.setlist(lst)
        xlist = self.grapher.cget(ox)
        ylist = self.grapher.cget(oy)
        if type(xlist) == type((0,)):
            xlist = list(xlist)
        if type(ylist) == type((0,)):
            ylist = list(ylist)
        if len(sol) > 0:
            for xylist in [xlist,ylist]:
                for i in range(len(xylist)):
                    if type(xylist[i]) == type(1):
                        if xylist[i] == -1:    
                            xylist[i] = indepvarname
                        elif coordnames != []:
                            xylist[i] = coordnames[xylist[i]]
                        else:
                            xylist[i] = str(i) 
        self.xEntry.setentry(self._shortstr(xlist))
        self.yEntry.setentry(self._shortstr(ylist))
        labels = []
        if not(self.grapher.cget("label_defaults") is None):
            for x in self.grapher.cget("label_defaults"):
                labels.append(str(x))
        default_labels = self.grapher.cget("solution").getLabels()
        for i in range(len(default_labels)):
            labels.append("[%d]"%default_labels[i])
        labels.append(self._shortstr(default_labels))
        if self.grapher.cget("label") == [0]:
            self.typeEntry.setentry(labels[0])
        else:
            self.typeEntry.setentry(self._shortstr(self.grapher.cget("label")))
        self.typeEntry.setlist(labels)



