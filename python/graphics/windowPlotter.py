#! /usr/bin/env python

import Tkinter
import Pmw
import AUTOutil
import plotter
try:
    import plotterDataViewer3D
except:
    pass
try:
    import plotterOpenInventor3D
except:
    pass
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
                                 "Destory the plot",
                                 label = "Quit",
                                 command = self.withdraw
                                 )
        self.menuBar.addmenuitem("Options",
                                 "command",
                                 label="Options...",
                                 command=self._setOptionWindow
                                 )


        topbox = Tkinter.Frame(interior,relief="raised",borderwidth=2)
        topbox.pack(side=Tkinter.BOTTOM)
        box = Tkinter.Frame(topbox)

        self.grapher.pack(expand=True,fill=Tkinter.BOTH)
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

        #FIXME:  This being here is a bug.  It needs to be part of the configure stuff,
        #        otherwise you can't change solution files.
        labels = []
        if not(self.grapher.cget("label_defaults") is None):
            for x in self.grapher.cget("label_defaults"):
                labels.append(str(x))
        default_labels = self.grapher.cget("solution").getLabels()
        for i in range(len(default_labels)):
            labels.append("[%d]"%default_labels[i])
        all = "["
        for i in range(len(default_labels)):
            all = all + str(default_labels[i]) + ","
        all = all[:-1]+"]"
        labels.append(all)
        typeEntry = self.createcomponent('typeEntry',
                                         (), None,
                                         Pmw.ComboBox,box,
                                         labelpos="w",
                                         label_text="Label")
        
        typeEntry.grid(row=0,column=1)
        typeEntry.setentry(labels[0])
        typeEntry.setlist(labels)
        typeEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("label",entry))
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
        list=map(string.atoi,list)
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
        self.grapher[key] = eval(entry)
        if key == "type":
            self.typeUpdateCallback()

    def typeUpdateCallback(self):
        pass

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
            self.xEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("bifurcation_x",entry))
            self.yEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("bifurcation_y",entry))

            list = []
            if not(self.grapher.cget("bifurcation_column_defaults") is None):
                for x in self.grapher.cget("bifurcation_column_defaults"):
                    list.append(str(x))
            if len(self.grapher.cget("bifurcation_diagram")) > 0:
                for i in range(len(self.grapher.cget("bifurcation_diagram")[0]["data"])):
                    list.append("[%d]"%i)
            self.xEntry.setlist(list)
            self.yEntry.setlist(list)

            self.xEntry.setentry(self.grapher.cget("bifurcation_x"))
            self.yEntry.setentry(self.grapher.cget("bifurcation_y"))
        else:
            self.xEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("solution_x",entry))
            self.yEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("solution_y",entry))

            list = []
            if not(self.grapher.cget("solution_column_defaults") is None):
                for x in self.grapher.cget("solution_column_defaults"):
                    list.append(str(x))
            list.append("['t']")
            if len(self.grapher.cget("solution")) > 0:
                for i in range(len(self.grapher.cget("solution").getIndex(0)["data"][0]["u"])):
                    list.append("[%d]"%i)
            self.xEntry.setlist(list)
            self.yEntry.setlist(list)

            self.xEntry.setentry(self.grapher.cget("solution_x"))
            self.yEntry.setentry(self.grapher.cget("solution_y"))

class WindowPlotter3D(WindowPlotter):
    def __init__(self,parent=None,cnf={},**kw):
        WindowPlotter.__init__(self,plotterDataViewer3D.plotter3D,parent,AUTOutil.cnfmerge((cnf,kw)))
#        WindowPlotter.__init__(self,plotterOpenInventor3D.plotter3D,parent,AUTOutil.cnfmerge((cnf,kw)))
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

        self.zEntry = self.createcomponent('zEntry',
                                      (), None,
                                      Pmw.ComboBox,box,
                                      labelpos="w",
                                      label_text="Z")
        self.zEntry.grid(row=1,column=0)

        self.colorEntry = self.createcomponent('colorEntry',
                                      (), None,
                                      Pmw.ComboBox,box,
                                      labelpos="w",
                                      label_text="COLOR")
        self.colorEntry.grid(row=1,column=1)

        self.typeUpdateCallback()
        
    def typeUpdateCallback(self):
        if self.grapher.cget("type") == "bifurcation":
            self.xEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("bifurcation_x",entry))
            self.yEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("bifurcation_y",entry))
            self.zEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("bifurcation_z",entry))
            self.colorEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("bifurcation_color",entry))

            list = []
            if not(self.grapher.cget("bifurcation_column_defaults") is None):
                for x in self.grapher.cget("bifurcation_column_defaults"):
                    list.append(str(x))
            if len(self.grapher.cget("bifurcation_diagram")) > 0:
                for i in range(len(self.grapher.cget("bifurcation_diagram")[0]["data"])):
                    list.append("[%d]"%i)
            self.xEntry.setlist(list)
            self.yEntry.setlist(list)
            self.zEntry.setlist(list)
            self.colorEntry.setlist(list)

            self.xEntry.setentry(self.grapher.cget("bifurcation_x"))
            self.yEntry.setentry(self.grapher.cget("bifurcation_y"))
            self.zEntry.setentry(self.grapher.cget("bifurcation_z"))
            self.colorEntry.setentry(self.grapher.cget("bifurcation_color"))
        else:
            self.xEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("solution_x",entry))
            self.yEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("solution_y",entry))
            self.zEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("solution_z",entry))
            self.colorEntry.configure(selectioncommand = lambda entry,obj=self:obj._modifyOption("solution_color",entry))

            list = []
            if not(self.grapher.cget("solution_column_defaults") is None):
                for x in self.grapher.cget("solution_column_defaults"):
                    list.append(str(x))
            list.append("['t']")
            if len(self.grapher.cget("solution")) > 0:
                for i in range(len(self.grapher.cget("solution")[0]["data"][0]["u"])):
                    list.append("[%d]"%i)
            self.xEntry.setlist(list)
            self.yEntry.setlist(list)
            self.zEntry.setlist(list)
            self.colorEntry.setlist(list)

            self.xEntry.setentry(self.grapher.cget("solution_x"))
            self.yEntry.setentry(self.grapher.cget("solution_y"))
            self.zEntry.setentry(self.grapher.cget("solution_z"))
            self.colorEntry.setentry(self.grapher.cget("solution_color"))







