#! /usr/bin/env python

import AUTOCommands
import Tkinter
import Pmw
import runAUTO
import plotter
import os
import tkSimpleDialog
import sys
import AUTOclui
import AUTOutil

class AUTOMessageBar(Pmw.MessageBar):
    def printMessage(self,text):
        self.helpmessage(text)

class AUTOGUIComponent:
    def __init__(self,messageFunc=None,textFunc=None):
        self.messageFunc = messageFunc
        self.textFunc = textFunc

    def create(self,messageFunc=None,textFunc=None,runner=None):
        pass

    def _getArgs(self,command):
        args = tkSimpleDialog.askstring("Args for command","Args")
        self._queueCommand(args,command)
        
    def _queueCommand(self,args,command):
        name = command.__name__
        if args is not None:
            sys.stdout.write(name+"("+args+")\n")
        else:
            sys.stdout.write(name+"()\n")
        eval("command(%s)"%args)
        sys.stdout.write(sys.ps1)
        sys.stdout.flush()
        if self.textFunc is not None:
            self.textFunc(name+"("+args+")\n")
            self.textFunc(str(output))
            self.textFunc(sys.ps1)
        self.messageFunc(name+"("+args+")\n")
    

class AUTOSimpleGUIComponent(Tkinter.Frame,AUTOGUIComponent):
    def __init__(self,parent=None,messageFunc=None,textFunc=None,**kw):
        Tkinter.Frame.__init__(self,parent,**kw)
        AUTOGUIComponent.__init__(self,messageFunc,textFunc)
        self.default = None
        self.create()

    def create(self,messageFunc=None,textFunc=None,runner=None):
        keys = AUTOCommands.__dict__.keys()
        self.simple()

    def simple(self,keys):
        self.addSimpleCommands([AUTOCommands])
        
    def addSimpleCommands(self,moduleList):
        self.defaultEntry = Pmw.EntryField(self,
                                           labelpos = 'w',
                                           label_text = 'Default name:',
                                           validate = None,
                                           command = self.__setDefault)
        self.defaultEntry.grid(row=0,columnspan=2)
        for module in moduleList:
            keys = list(module.__dict__)
            keys.sort()
            i = 0
            for key in keys:
                # Check to see if it is a command
                cmd = getattr(module,key)
                if (hasattr(cmd,"fun") and hasattr(cmd,"type") and
                    cmd.type==AUTOCommands.SIMPLE):
                    button = Tkinter.Button(self,text=cmd.shortName,
                                            command=lambda c=cmd.fun:
                                                self._getArgs(c))
                    button.grid(row=i/2 + 1,column=i%2)
                    i = i + 1

    def __setDefault(self):
        self.default = self.defaultEntry.get()
                    
    def _getArgs(self,command):
        if self.default is None:
            self.default = tkSimpleDialog.askstring("Default Name","Name")
            self.defaultEntry.setentry(self.default)
        self._queueCommand(self.default,command)
        

class AUTOExpertGUIComponent(Pmw.MenuBar,AUTOGUIComponent):
    def __init__(self,parent=None,messageFunc=None,textFunc=None,**kw):
        Pmw.MenuBar.__init__(self,parent,**kw)
        AUTOGUIComponent.__init__(self,messageFunc,textFunc)
        self.create()

    def create(self):
        if self.textFunc is not None:
            self.textFunc(sys.ps1)
        keys = AUTOCommands.__dict__.keys()

        self.expert(keys)

    def expert(self,keys):
        baseList = []
        for key in keys:
            # Check to see if it is a command
            cmd = getattr(AUTOCommands,key)
            if hasattr(cmd,"fun"):
                bases = cmd.__bases__
                for base in bases:
                    if base not in baseList:
                        baseList.append(base)
        
        for base in baseList:
            if len(base.__name__) > 7 and base.__name__[:7] == "command":
                self.addmenu(base.__name__[7:],'Commands which inherit from %s'%base.__name__)
            else:
                self.addmenu(base.__name__,'Commands which inherit from %s'%base.__name__)
        self.addExpertCommands([AUTOCommands])
            
    def addExpertCommands(self,moduleList):
        for module in moduleList:
            keys = list(module.__dict__)
            keys.sort()
            for key in keys:
                # Check to see if it is a command
                cmd = getattr(module,key)
                if hasattr(cmd,"fun"):
                    f = cmd.fun
                    name = cmd.__bases__[0].__name__
                    if len(name) > 7:
                        name = name[7:]
                    self.addmenuitem(name, 'command', key, label=key,
                                     command=lambda c=f: self._getArgs(c))


class AUTOBody(Pmw.ScrolledText):
    def printOutput(self,text):
        self.insert("end",text)


class AUTOgui:
    def __init__(self,type="simple"):
        root=Tkinter.Toplevel()
        

        # Create the message bar        
        messagebar = AUTOMessageBar(root,
                                    entry_width = 40,
                                    entry_relief='groove',
                                    labelpos = 'w',
                                    label_text = 'Status:')
        messagebar.pack(fill = 'x', padx = 10, pady = 10,side="bottom")

        # Create the body
        #body   = AUTOBody(root)
        #body.pack(fill="both",expand=1)

        if type=="simple":
            self.interface = AUTOSimpleGUIComponent(root,messagebar.printMessage)
        else:
            balloon = Pmw.Balloon(root)
            self.interface = AUTOExpertGUIComponent(root,messagebar.printMessage,
                                                    hull_relief = 'raised',
                                                    hull_borderwidth = 1,
                                                    balloon = balloon)
        self.interface.pack(fill = 'x',side="top")


#    def addCommands(self,moduleList):
#        self.interface.addCommands(moduleList)








