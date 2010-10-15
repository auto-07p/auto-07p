#! /usr/bin/env python

try:
    import Tkinter
except ImportError:
    import tkinter as Tkinter # Python 3
from graphics import Pmw
import parseC
import AUTOutil
from graphics import plotter
import sys

# FIXME:  No regression tester (except as part of interactiveBindings)
class WindowPlotter(Pmw.MegaToplevel):
    def __init__(self,grapherClass,parent=None,**kw):
        optiondefs = []
        self.defineoptions(kw,optiondefs)
        if kw.get('grapher_hide'):
            if 'graphics.grapher_mpl' in sys.modules:
                kwnew = {}
                for k in kw:
                    if k.startswith('grapher_'):
                        kwnew[k[8:]] = kw[k]
                self.grapher = grapherClass(**kwnew)
                self.savefig = self.grapher.savefig
                return
            # without matplotlib, retract the Tk() window
            parent=Tkinter.Tk()
            parent.withdraw()
            print("\nCannot hide the on-screen plot without matplotlib.")
        Pmw.MegaToplevel.__init__(self, parent)

        interior = self.interior()
        self.helpBalloon = self.createcomponent('helpBalloon',
                                                (), None,
                                                Pmw.Balloon, interior)
                
        self.menuBar = self.createcomponent('menuBar',
                                            (), None,
                                            Pmw.MainMenuBar,interior,
                                            hotkeys="true",
                                            hull_relief = 'raised',
                                            hull_borderwidth = 1,
                                            balloon=self.helpBalloon)
        interior.configure(menu=self.menuBar)
        self.menuBar.addmenu("File","File operations")
        self.menuBar.addmenu("Options","View and set options")
        self.menuBar.addmenu("Help","View help on the plotting widget",name='help')

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
        labelEntry = self.createcomponent('labelEntry',
                                          (), None,
                                          Pmw.RadioSelect,box,
                                          labelpos="w",
                                          label_text="Type")
        labelEntry.add("bifurcation")
        labelEntry.add("solution")
        labelEntry.invoke(self.grapher.cget("type"))
        labelEntry.configure(command = lambda entry:
                                 self._modifyOption("type",entry))
        labelEntry.grid(row=0,column=0)
        self.labelEntry = labelEntry

        typeEntry = self.createcomponent('typeEntry',
                                         (), None,
                                         Pmw.ComboBox,box,
                                         labelpos="w",
                                         label_text="Label")
        
        typeEntry.grid(row=0,column=1)
        typeEntry.configure(selectioncommand = lambda entry:self._modifyOption("label",entry))
        self.typeEntry = typeEntry
        box.grid(row=0)

        box = Tkinter.Frame(topbox)
        self._extraButtons(box)
        box.grid(row=1)
        
        # Let the appropriate things grow
        topbox.rowconfigure(1,weight=1)
        topbox.columnconfigure(0,weight=1)

        self.initialiseoptions(WindowPlotter)
        self.savefig = self.grapher.savefig

    def __labelFunction(self,lst):
        # The return value of a ScrolledListBox is a list of strings, so we change them
        # to integers here
        lst=map(int,lst)
        # but the _modifyOption method expects a string, so we change the whole thing to a string here
        self._modifyOption("label",str(lst))

    def _setOptionWindow(self):
        lst = sorted([key for key in self.grapher.configure() 
                      if self.grapher._isInternalOption(key)])
        self.optionSelctionDialog = Pmw.SelectionDialog(title = 'Options',
                                                        buttons = ('OK', 'Cancel'),
                                                        defaultbutton = 'OK',
                                                        scrolledlist_labelpos = 'n',
                                                        label_text = 'Setable options',
                                                        scrolledlist_items = lst,
                                                        command = self.optionSelectionDialogCommand)
        
    def _extraButtons(self,box):
        pass

    def optionSelectionDialogCommand(self,button):
        if button == "OK":
            self.setOptionDialog(self.optionSelctionDialog.getcurselection()[0])
        if button == "Cancel":
            self.optionSelctionDialog.destroy()

    def _shortstr(self,x):
        if isinstance(x,(list,tuple)):
            return ",".join(map(str,x))
        return x

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
        valueEntry.configure(command = lambda: self.__dialogFunction("Ok",key,valueEntry.get()))

        self.diag.configure(command = lambda button:self.__dialogFunction(button,key,valueEntry.get()))
        

        optionLabel.setentry(key)
        valueLabel.setentry(str(self.grapher.cget(key)))
        valueEntry.clear()

    def __dialogFunction(self,button,key,entry):
        if button == "Ok":
            self._modifyOption(key,entry)
        self.diag.destroy()
        
    def _modifyOption(self,key,entry):
        if key[-2:] == "_z" and entry == " 2D Plot ":
            self.grapher[key] = None
            return
        self.grapher[key] = self.grapher.parseoption(key, entry, 
                                                     parseC.parseC())
        if key == "type":
            self.typeUpdateCallback()

    def typeUpdateCallback(self):
        pass

    def checktype(self):
        # force type change if a bifurcation diagram or solution don't exist
        ty = self.grapher.cget("type")
        bd = self.grapher.cget("bifurcation_diagram")
        sol = self.grapher.cget("solution")
        if ty == "bifurcation" and (len(bd)==0 or len(bd[0])==0) and len(sol):
            self.grapher.config(type="solution")
            self.labelEntry.invoke("solution")
        elif ty == "solution" and len(sol)==0 and len(bd) and len(bd[0]):
            self.grapher.config(type="bifurcation")
            self.labelEntry.invoke("bifurcation")

    def config(self,cnf=None,**kw):
        """
        Configure settings for a plot.
    
        The values for resources are specified as keyword
        arguments. See the manual and example autorc in $AUTO_DIR/autorc for
        the list of allowed keyword arguments.
        """
        rval = self.grapher.config(cnf,**kw)
        if isinstance(cnf, str) or (cnf is None and not kw):
            return rval
        self.checktype()
        dct = (cnf or {}).copy()
        dct.update(kw)
        for key,value in dct.items():
            if key == "type":
                self.labelEntry.invoke(value)
            if key in ["type","label","label_defaults",
                       "bifurcation_diagram_filename",
                       "solution_filename",
                       "bifurcation_column_defaults",
                       "bifurcation_diagram",
                       "bifurcation_x","bifurcation_y","bifurcation_z",
                       "bifurcation_coordnames",
                       "solution_column_defaults",
                       "solution_indepvarname","solution_coordnames",
                       "solution","solution_x","solution_y","solution_z"]:
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

    def destroy(self):
        self.typeEntry.destroy()
        Pmw.MegaToplevel.destroy(self)        

    def destroywindow(self):
        self.destroy()

class WindowPlotter2D(WindowPlotter):
    def __init__(self,parent=None,**kw):
        WindowPlotter.__init__(self,plotter.plotter,parent,**kw)

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

        if plotter.Axes3D is not None:
            self.zEntry = self.createcomponent('zEntry',
                                               (), None,
                                               Pmw.ComboBox,box,
                                               labelpos="w",
                                               label_text="Z")
            self.zEntry.grid(row=0,column=2)

        self.checktype()
        self.typeUpdateCallback()

    def destroy(self):
        self.xEntry.destroy()
        self.yEntry.destroy()
        if plotter.Axes3D is not None:
            self.zEntry.destroy()
        WindowPlotter.destroy(self)

    def typeUpdateCallback(self):
        if self.grapher.cget("type") == "bifurcation":
            ox = "bifurcation_x"
            oy = "bifurcation_y"
            oz = "bifurcation_z"
            ocd = "bifurcation_column_defaults"
            o = "bifurcation_diagram"
        else:
            ox = "solution_x"
            oy = "solution_y"
            oz = "solution_z"
            ocd = "solution_column_defaults"
            o = "solution"
        self.xEntry.configure(selectioncommand = lambda entry:
                              self._modifyOption(ox,entry))
        self.yEntry.configure(selectioncommand = lambda entry:
                              self._modifyOption(oy,entry))
        if plotter.Axes3D is not None:
            self.zEntry.configure(selectioncommand = lambda entry:
                                  self._modifyOption(oz,entry))

        lst = []
        if self.grapher.cget(ocd) is not None:
            lst = list(map(str,self.grapher.cget(ocd)))
        sol = self.grapher.cget(o)
        if self.grapher.cget("type") == "solution":
            indepvarname = None
            for s in sol:
                indepvarname = s.indepvarname
                break
            if self.grapher.cget(ox[:-1]+"indepvarname"):
                indepvarname = self.grapher.cget(ox[:-1]+"indepvarname")
            if indepvarname is not None:
                lst.append(indepvarname)
        coordnames = self.grapher._coordnames
        lst.extend(coordnames)
        xlist = self.grapher.cget(ox)
        ylist = self.grapher.cget(oy)
        zlist = self.grapher.cget(oz)
        if type(xlist) == type((0,)):
            xlist = list(xlist)
        if type(ylist) == type((0,)):
            ylist = list(ylist)
        if type(zlist) == type((0,)):
            zlist = list(zlist)
        if len(sol) > 0:
            for xylist in xlist,ylist,zlist:
                if xylist is None:
                    break
                for i in range(len(xylist)):
                    if type(xylist[i]) == type(1):
                        if xylist[i] == -1:    
                            xylist[i] = indepvarname
                        elif coordnames != []:
                            xylist[i] = coordnames[xylist[i]]
                        else:
                            xylist[i] = str(i) 
        xentry = self._shortstr(xlist)
        yentry = self._shortstr(ylist)
        zentry = None
        if plotter.Axes3D is not None:
            if zlist is None:
                zentry = " 2D Plot "
            else:
                zentry = self._shortstr(zlist)
        for entry in xentry, yentry, zentry:
            if entry is not None and entry != " 2D Plot " and entry not in lst:
                lst.append(entry)
        self.xEntry.setentry(xentry)
        self.xEntry.setlist(lst)
        self.yEntry.setentry(yentry)
        self.yEntry.setlist(lst)
        if plotter.Axes3D is not None:
            self.zEntry.setentry(zentry)
            self.zEntry.setlist([" 2D Plot "]+lst)
        labels = []
        if self.grapher.cget("label_defaults") is not None:
            labels = list(map(str,self.grapher.cget("label_defaults")))
        default_labels = self.grapher.cget("solution").getLabels()
        labels.extend(["%d"%d for d in default_labels])
        if len(default_labels) > 1 or len(labels) == 0:
            labels.append(self._shortstr(default_labels))
        if self.grapher.cget("label") == [0]:
            entry = labels[0]
        else:
            entry = self._shortstr(self.grapher.cget("label"))
        self.typeEntry.setentry(entry)
        if entry not in labels:
            labels.append(entry)
        self.typeEntry.setlist(labels)



