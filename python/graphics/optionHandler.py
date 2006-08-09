#! /usr/bin/env python

import types
import AUTOutil

class OptionHandler:
    def __init__(self,baseClass=None):
        self.__options = {}
        self.__optionDefaults = {}
        self.__optionAliases = {}
        self.__optionCallbacks = {}
        self.__baseClass = baseClass

    def __applyOptionAliases(self,key):
        if key in self.__optionAliases.keys():
            return self.__optionAliases[key]
        else:
            return key

    def __parseOptions(self,dict):
        for key in dict.keys():
            newkey = self.__applyOptionAliases(key)
            if newkey in self.__options.keys():
                self.__options[newkey] = dict[key]
                if not(self.__optionCallbacks[newkey] is None):
                    self.__optionCallbacks[newkey](key,dict[key],self.__options)
                del dict[key]
                    
                
    # Options are of the form data=(default_value,callback=None)
    def addOptions(self,dict,**kw):
        dict = AUTOutil.cnfmerge((dict,kw))
        for key in dict.keys():
            self.__optionDefaults[key] = dict[key][0]
            self.__options[key] = dict[key][0]
            self.__optionCallbacks[key] = dict[key][1]

    # Aliases are of the form fg=foreground
    def addAliases(self,dict,**kw):
        dict = AUTOutil.cnfmerge((dict,kw))
        for key in dict.keys():
            self.__optionAliases[key] = dict[key]

    def _isInternalOption(self,key):
        key = self.__applyOptionAliases(key)
        if self.__options.has_key(key):
            return 1
        else:
            return 0

    def config(self,cnf=None,**kw):
        if cnf is None and len(kw)==0:
            dict={}
            for key in self.__optionAliases.keys():
                dict[key] = [key,self.__optionAliases[key]]
            for key in self.__options.keys():
                dict[key] = OptionHandler.config(self,key)
            if self.__baseClass is None:
                return dict
            else:
                return AUTOutil.cnfmerge((dict,self.__baseClass.config(self)))
        elif type(cnf) == types.StringType:
            cnf = self.__applyOptionAliases(cnf)
            if self.__options.has_key(cnf):
                return (cnf,cnf,cnf,
                        self.__optionDefaults[cnf],
                        self.__options[cnf])
            else:
                if self.__baseClass is None:
                    raise AUTORuntimeError("Option %s not found"%cnf)
                else:
                    return self.__baseClass.config(self,cnf)
        else:
            dict = AUTOutil.cnfmerge((cnf,kw))
            self.__parseOptions(dict)
            if not(self.__baseClass is None):
                self.__baseClass.config(self,dict)
        
    configure = config
    def cget(self,key):
        newkey = self.__applyOptionAliases(key)
        if self.__options.has_key(newkey):
            return self.__options[newkey]
        else:
            if self.__baseClass is None:
                raise AUTORuntimeError("Option %s not found"%key)
            else:
                return self.__baseClass.cget(self,key)
    __getitem__ = cget


    def simpleOptionDictionary(self):
        options = self.configure()
        dict={}
        for key in options.keys():
            dict[key] = self.cget(key)
        return dict
    














