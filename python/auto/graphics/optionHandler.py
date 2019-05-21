#! /usr/bin/env python

import AUTOutil
import AUTOExceptions

class OptionHandler:
    def __init__(self,baseClass=None):
        self.__options = {}
        self.__optionDefaults = {}
        self.__optionAliases = {}
        self.__optionCallbacks = {}
        self.__optionRC = {}
        self.__baseClass = baseClass

    def __applyOptionAliases(self,key):
        return self.__optionAliases.get(key,key)

    def __parseOptions(self,dct):
        for key in dct:
            newkey = self.__applyOptionAliases(key)
            if newkey in self.__options:
                if self.__options[newkey] != dct[key]:
                    self.__options[newkey] = dct[key]
                    newcb = self.__optionCallbacks[newkey]
                    if newcb is not None:
                        newcb(key,dct[key],self.__options)
                    
                
    # Options are of the form data=(default_value,callback=None)
    def addOptions(self,**kw):
        for key in kw:
            self.__optionDefaults[key] = kw[key][0]
            self.__options[key] = kw[key][0]
            if (self.__baseClass is not None and
                key in self.__baseClass.keys(self)):
                self.__baseClass.config(self,{key:kw[key][0]})
            self.__optionCallbacks[key] = kw[key][1]
            self.__optionRC[key] = 0

    def addRCOptions(self,**kw):
        self.__optionDefaults.update(kw)
        self.__options.update(kw)
        for key in kw:
            self.__optionRC[key] = 1
            if (self.__baseClass is not None and
                key in self.__baseClass.keys(self)):
                self.__baseClass.config(self, {key: kw[key]})

    # Aliases are of the form fg=foreground
    def addAliases(self,**kw):
        self.__optionAliases.update(kw)

    def _isInternalOption(self,key):
        return self.__applyOptionAliases(key) in self.__options

    def config(self,cnf=None,**kw):
        if cnf is None and not kw:
            dct={}
            for key in self.__optionAliases:
                dct[key] = [key,self.__optionAliases[key]]
            for key in self.__options:
                dct[key] = OptionHandler.config(self,key)
            if self.__baseClass is not None:
                dct.update(self.__baseClass.config(self))
            return dct
        if isinstance(cnf, str):
            cnf = self.__applyOptionAliases(cnf)
            if cnf in self.__options:
                return (cnf,cnf,cnf,
                        self.__optionDefaults[cnf],
                        self.__options[cnf],
                        self.__optionRC[cnf])
            if self.__baseClass is None:
                raise AUTOExceptions.AUTORuntimeError("Option %s not found"%cnf)
            return self.__baseClass.config(self,cnf)
        dct = (cnf or {}).copy()
        dct.update(kw)
        self.__parseOptions(dct)
        if self.__baseClass is not None:
            for k in list(dct):
                if k not in self.__baseClass.keys(self):
                    del dct[k]
            self.__baseClass.config(self,**dct)
        
    configure = config
    def cget(self,key):
        newkey = self.__applyOptionAliases(key)
        if newkey in self.__options:
            return self.__options[newkey]
        else:
            if self.__baseClass is None:
                raise AUTOExceptions.AUTORuntimeError("Option %s not found"%key)
            else:
                return self.__baseClass.cget(self,key)
    __getitem__ = cget


    def simpleOptionDictionary(self):
        options = self.configure()
        dct={}
        for key in options:
            dct[key] = self.cget(key)
        return dct
