#! /usr/bin/env python 
import cStringIO
import parseC
import parseB
import parseS
import parseBandS
import parseH
import bifDiag
import os
import AUTOutil
import sys
import types
import string
import glob
import runAUTO
import re
import shutil

SIMPLE=0
EXPERT=1


import AUTOExceptions

# Initialize a default runAUTO for those commands that use runAUTO object
_runner = runAUTO.runAUTO(verbose="yes",makefile="$AUTO_DIR/cmds/cmds.make")

#############################################
#  commands      
#############################################
class command:
    def __init__(self):
        pass
    # The call function must return something that you
    # can call the "print" function on
    def __call__(self):
        pass
    def undo(self):
        raise "Undo undefined for this command"

##############################################
#  Generic Commands
##############################################

class commandMacro(command):
    def __init__(self,command_list):
        self.command_list = command_list
    def __call__(self):
        text = ""
        for command in self.command_list:
            text = text + str(command())
        return valueString(text)

class commandReturnString(command):
    def __init__(self,text):
        self.text = text
    def __call__(self):
        return valueString(self.text)
        
##############################################
#  Script based commands from $AUTO_DIR/97/cmds
##############################################

class commandClean(command):
    """Clean the current directory.

    Type FUNC() to clean the current directory.  This command will
    delete all files of the form fort.*, *.*~, *.o, and *.exe.
    """
    
    def __init__(self):
        pass
    def __call__(self):
	rval=valueSystem()
        toclean = (glob.glob("fort.*") + glob.glob("*.o") + glob.glob("*.exe")+
                   glob.glob("*.*~"))
        for f in toclean:
            os.remove(f)
        rval.info("Deleting fort.* *.o *.exe *.*~ ... done\n")
        return rval

class commandCopyDemo(command):
    """Copy a demo into the current directory.

    Type FUNC('xxx') to copy all files from auto/07p/demos/xxx to the
    current user directory.  Here 'xxx' denotes a demo name; e.g.,
    'abc'.  To avoid the overwriting of existing
    files, always run demos in a clean work directory.
    """
    
    type=SIMPLE
    shortName="demo"
    def __init__(self,name1):
        self.demo = name1
    def __call__(self):
	rval=valueSystem()
        demofiles = glob.glob(os.path.expandvars(
                "$AUTO_DIR/demos/%s/*"%self.demo))
        for f in demofiles:
            try:
                shutil.copy(f, ".")
            except IOError:
                pass
        if os.path.exists("c.%s.1"%(self.demo)):
            shutil.copy("c.%s.1"%(self.demo),"c.%s"%(self.demo))
        rval.info("Copying demo %s ... done\n"%self.demo)
        return rval

class commandCopyAndLoadDemo(commandMacro):
    """Copy a demo into the current directory and load it.

    Type FUNC('xxx') to copy all files from auto/07p/demos/xxx to the
    current user directory.  Here 'xxx' denotes a demo name; e.g.,
    'abc'.  To avoid the overwriting of existing
    files, always run demos in a clean work directory.  NOTE: This
    command automatically performs the load command as well.
    """
    
    def __init__(self,name1,runner=None):
        lst=[]
        lst.append(commandCopyDemo(name1))
        lst.append(commandRunnerLoadName(name1,runner))
        commandMacro.__init__(self,lst)

class commandDeleteFortFiles(command):
    """Clear the current directory of fort files.

    Type FUNC() to clean the current directory.  This command will
    delete all files of the form fort.*.
    """
    
    def __init__(self):
        pass
    def __call__(self):
	rval=valueSystem()
        toclean = glob.glob("fort.*")
        for f in toclean:
            os.remove(f)
        rval.info("Deleting fort.* ... done\n")
        return rval

class commandUserData(command):
    """Convert user-supplied data files.

    Type FUNC('xxx') to convert a user-supplied data file 'xxx.dat' to
    AUTO format. The converted file is called 's.dat'.  The original
    file is left unchanged.  AUTO automatically sets the period in
    PAR(11).  Other parameter values must be set in 'STPNT'. (When
    necessary, PAR(11) may also be redefined there.)  The
    constants-file file 'c.xxx' must be present, as the AUTO-constants
    'NTST' and 'NCOL' are used to define the new mesh.  For examples
    of using the 'userData' command see demos 'lor' and 'pen' (where
    it has the old name 'fc').

    Note: this technique has been obsoleted by the 'dat' AUTO constant.
    """
    
    def __init__(self,name1):
        self.data = []
        self.data.append(name1)
    def __call__(self):
	rval=valueSystem()
        rval.info("NOTE: This command does not use filename templates\n")
        rval.info("Starting conversion of %s.dat : \n"%(self.data[0],))
        if glob.glob("%s.f90"%(self.data[0],)) == []:
            if glob.glob("%s.f"%(self.data[0],)) == []:
                equation_file="%s.c"%(self.data[0],)
            else:
                equation_file="%s.f"%(self.data[0],)
        else:
            equation_file="%s.f90"%(self.data[0],)
        cfile = "c.%s"%(self.data[0])
        datfile = "%s.dat"%(self.data[0])
        rval.info("(Required files : %s, %s, %s)\n"%(equation_file,cfile,
                                                     datfile))
        fconrun = runAUTO.runAUTO(verbose="no",
                                  makefile="$AUTO_DIR/cmds/cmds.make fcon")
        fconrun.config(e=self.data[0])
        fconrun.runMakefile(self.data[0])
        if os.path.exists(cfile):
            shutil.copy(cfile,"fort.2")
        if os.path.exists(datfile):
            shutil.copy(datfile,"fort.3")
        rval.interact("./fcon")
        if os.path.exists("fort.8"):
            if os.path.exists("s.dat"):
                os.remove("s.dat")
            os.rename("fort.8","s.dat")
            rval.info("Conversion done : converted file saved as s.dat\n")
        files = glob.glob("fcon*") + ["fort.2", "fort.3"]
        for f in files:
            os.remove(f)
        return rval
    
##############################################
#  Commands which use the filename templates
##############################################
class commandWithFilenameTemplate(command):
    def __init__(self,name1=None,name2=None,templates=None):
        if templates is None:
            self.templates = {}
            self.templates["equation"]           = "EQUATION_NAME=%s"
            self.templates["constants"]          = "c.%s"
            self.templates["bifurcationDiagram"] = "b.%s"
            self.templates["solution"]           = "s.%s"
            self.templates["diagnostics"]        = "d.%s"
            self.templates["homcont"]           = "h.%s"
        else:
            self.templates = templates

        self.name1={}
        self.name1["constants"] = self. _applyTemplate(name1,"constants")
        self.name1["bifurcationDiagram"] = self._applyTemplate(name1,"bifurcationDiagram")
        self.name1["solution"] = self._applyTemplate(name1,"solution")
        self.name1["diagnostics"] = self._applyTemplate(name1,"diagnostics")

        self.name2={}
        self.name2["constants"] = self._applyTemplate(name2,"constants")
        self.name2["bifurcationDiagram"] = self._applyTemplate(name2,"bifurcationDiagram")
        self.name2["solution"] = self._applyTemplate(name2,"solution")
        self.name2["diagnostics"] = self._applyTemplate(name2,"diagnostics")
        
    def _applyTemplate(self,text,template):
        if text is None:
            return None
        elif type(text) in [type(""), type(1), type(1.0)]:
            exec "rval = '%s'%%text"%self.templates[template]
            tmp = glob.glob(rval)
            if len(tmp) > 0:
                rval = ""
                for x in tmp:	
                   rval = rval + x + " "
            rval = string.strip(rval)
            return rval
        else:
            return text



class commandRelabel(commandWithFilenameTemplate):
    """Relabel data files.

    Type y=FUNC(x) to return the python object x, with the solution
    labels sequentially relabelled starting at 1, as a new object y.

    Type FUNC('xxx') to relabel s.xxx and b.xxx (if you are
    using the default filename templates).  Backups of the
    original files are saved.

    Type FUNC('xxx','yyy') to relabel the existing data-files s.xxx and b.xxx,
    and save them to s.yyy and b.yyy; d.xxx is copied to d.yyy (if you are using
    the default filename templates). 
    """

    type=SIMPLE
    shortName="relabel"
    def __init__(self,name1=None,name2=None,templates=None):
        self.type = type(name1)
        self.name1 = name1
        if type(name1) == type(""):
            commandWithFilenameTemplate.__init__(self,name1,name2,templates)
    def __call__(self):
	rval=valueSystem()
        if self.type != type("") and self.type != type(None):
            rval.data = self.name1.relabel()
            rval.info("Relabeling done\n")
            return rval
        n1b = self.name1["bifurcationDiagram"]
        n1s = self.name1["solution"]
        n1d = self.name1["diagnostics"]
        if n1b is None and n1s is None and n1d is None:
            n1b, n1s, n1d = "fort.7", "fort.8", "fort.9"
        if self.name2["bifurcationDiagram"] is None:
            n2b = n1b+'~~'
            n2s = n1s+'~~'
            n2d = n1d+'~~'
        else:
            n2b = self.name2["bifurcationDiagram"]
            n2s = self.name2["solution"]
            n2d = self.name2["diagnostics"]
        import relabel
        relabel.relabel(n1b,n1s,n2b,n2s)
        if os.access(n2b,os.F_OK):
            if self.name2["bifurcationDiagram"] is None:
                # Save backups
                if os.access(n1b+'~',os.F_OK):
                    os.remove(n1b+'~')
                os.rename(n1b,n1b+'~')
                os.rename(n2b,n1b)
                if os.access(n1s+'~',os.F_OK):
                    os.remove(n1s+'~')
                os.rename(n1s,n1s+'~')
                os.rename(n2s,n1s)
            elif os.path.exists(n1d):
                shutil.copy(n1d, n2d)
            rval.info("Relabeling succeeded\n")

        rval.info("Relabeling done\n")
        return rval

class commandMergeBranches(commandWithFilenameTemplate):
    """Merge branches in data files.

    Type y=FUNC(x) to return the python object x, with its branches
    merged into continuous curves, as a new object y.

    Type FUNC('xxx') to merge branches in s.xxx, b.xxx, and d.xxx (if you are
    using the default filename templates).  Backups of the
    original files are saved.

    Type FUNC('xxx','yyy') to merge branches in the existing data-files
    s.xxx, b.xxx, and d.xxx and save them to s.yyy, b.yyy, and d.yyy (if you
    are using the default filename templates). 
    """

    type=SIMPLE
    shortName="merge"
    def __init__(self,name1=None,name2=None,templates=None):
        self.type = type(name1)
        self.name1 = name1
        if type(name1) == type(""):
            commandWithFilenameTemplate.__init__(self,name1,name2,templates)
    def __call__(self):
	rval=valueSystem()
        if self.type != type("") and self.type != type(None):
            rval.data = self.name1.merge()
            rval.info("Merge done\n")
            return rval
        n1b = self.name1["bifurcationDiagram"]
        n1s = self.name1["solution"]
        n1d = self.name1["diagnostics"]
        if n1b is None and n1s is None and n1d is None:
            n1b, n1s, n1d = "fort.7", "fort.8", "fort.9"
        bd = bifDiag.bifDiag(n1b,n1s,n1d)
        bd = bd.merge()
        if self.name2["bifurcationDiagram"] is None:
            n2b = n1b+'~~'
            n2s = n1s+'~~'
            n2d = n1d+'~~'
        else:
            n2b = self.name2["bifurcationDiagram"]
            n2s = self.name2["solution"]
            n2d = self.name2["diagnostics"]
        bd.writeFilename(n2b,n2s,n2d)
        if os.access(n2b,os.F_OK):
            if self.name2["bifurcationDiagram"] is None:
                # Save backups
                for [n1,n2] in [[n1b,n2b],[n1s,n2s],[n1d,n2d]]:
                    if os.access(n1+'~',os.F_OK):
                        os.remove(n1+'~')
                    os.rename(n1,n1+'~')
                    os.rename(n2,n1)
            rval.info("Merging succeeded\n")

        rval.info("Merging done\n")
        return rval

class commandSubtractBranches(commandWithFilenameTemplate):
    """Subtract branches in data files.

    Type z=FUNC(x,y,ref) to return the python object x, where,
    using interpolation, the first branch in y is subtracted from all
    branches in x, as a new object z.
    Use 'ref' (e.g., 'PAR(1)')  as the reference column in y
    (only the first monotonically increasing or decreasing part is used).

    Type FUNC('xxx','yyy','ref') to subtract, using interpolation, the first
    branch in b.yyy from all branches in b.xxx, and save the result in b.xxx.
    A Backup of the original file is saved.

    Use optional arguments branch=m, and point=n, to denote the branch and
    first point on that branch within y or 'b.yyy', where m,n are in
    {1,2,3,...}.
    """

    type=SIMPLE
    shortName="subtract"
    def __init__(self,name1,name2,col,branch=1,point=1,templates=None):
        self.type = type(name1)
        self.name1 = name1
        self.name2 = name2
        self.col = col
        self.branch = branch
        self.point = point
        if type(name1) == type(""):
            commandWithFilenameTemplate.__init__(self,name1,name2,templates)
    def __call__(self):
	rval=valueSystem()
        if self.type != type(""):
            sub = self.name1.subtract(self.name2[self.branch-1],self.col,
                                      self.point)
        else:
            n1b = self.name1["bifurcationDiagram"]
            bd1 = bifDiag.bifDiag(n1b)
            n2b = self.name2["bifurcationDiagram"]
            if n1b == n2b:
                bd2 = bd1
            else:
                bd2 = bifDiag.bifDiag(n2b)
            sub = bd1.subtract(bd2[self.branch-1],self.col,self.point)
            shutil.copy(n1b,n1b+'~')
            sub.writeFilename(n1b,'')            
        rval.info("Subtracting done\n")
        rval.data = sub
        return rval

class commandAppend(commandWithFilenameTemplate):
    """Append data files.

    Type FUNC(x,'xxx') to append bifurcation diagram x
    to the data-files b.xxx, s.xxx, and d.xxx. This is equivalent to
    the command
    save(x+load('xxx'),'xxx')

    Type FUNC('xxx',x) to append existing data-files s.xxx, b.xxx,
    and d.xxx to bifurcation diagram x. This is equivalent to
    the command
    x=load('xxx')+x

    Type FUNC('xxx') to append the output-files fort.7, fort.8,
    fort.9, to existing data-files s.xxx, b.xxx, and d.xxx (if you are
    using the default filename templates).

    Type FUNC('xxx','yyy') to append existing data-files s.xxx, b.xxx,
    and d.xxx to data-files s.yyy, b.yyy, and d.yyy (if you are using
    the default filename templates).
    """

    type=SIMPLE
    shortName="append"
    def __init__(self,name1,name2=None,templates=None):
        self.parsed1=None
        self.parsed2=None
        if isinstance(name1, bifDiag.bifDiag):
            self.parsed1=name1
            name1=name2
            name2=None
        if isinstance(name1, bifDiag.bifDiag):
            self.parsed2=name1
        else:
            commandWithFilenameTemplate.__init__(self,name1,name2,templates)
    def __call__(self):
	rval=valueSystem()
        if self.parsed1 or self.parsed2:
            n = None
            if not self.parsed1 or not self.parsed2:
                nb = self.name1["bifurcationDiagram"]
                ns = self.name1["solution"]
                nd = self.name1["diagnostics"]
        if self.parsed2: #append to parsed2
            parsed1 = self.parsed1
            if not parsed1:
                parsed1 = bifDiag.bifDiag(nb,ns,nd)
                rval.info("Appending from %s, %s and %s ... done\n"%(nb,ns,nd))
            self.parsed2.extend(parsed1)
            return rval
        if self.parsed1: #append from parsed1 to file
            self.parsed1.writeFilename(nb,ns,nd,append=True)
            rval.info("Appending to %s, %s and %s ... done\n"%(nb,ns,nd))
            return rval
        i = 7
        for s in ["bifurcationDiagram","solution","diagnostics"]:
            n1 = self.name1[s]
            n2 = self.name2[s]
            if n2 is None:
                n2 = n1
                n1 = "fort."+str(i)
            i = i+1
            f1 = open(n1)
            f2 = open(n2,"ab")
            while 1:
                buf = f1.read(1024*1024)
                if buf == "":
                    break
                f2.write(buf)
            f1.close()
            f2.close()
            rval.info("Appending %s to %s ... done\n"%(n1,n2))
        return rval

class commandCopyDataFiles(commandWithFilenameTemplate):
    """Copy data files.

    Type FUNC('xxx','yyy') to copy the data-files c.xxx, d.xxx, b.xxx,
    and h.xxx to c.yyy, d.yyy, b.yyy, and h.yyy (if you are using the
    default filename templates).
    """
    
    def __init__(self,name1,name2,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,name2,templates)
    def __call__(self):
	rval=valueSystem()
        for s in ["bifurcationDiagram","solution","diagnostics","constants"]:
            n1 = self.name1[s]
            n2 = self.name2[s]
            if os.path.exists(n1):
                shutil.copy(n1,n2)
                rval.info("Copying %s to %s ... done\n"%(n1,n2))
        return rval
    
class commandCopyFortFiles(commandWithFilenameTemplate):
    """Save data files.

    Type FUNC(x,'xxx') to save bifurcation diagram x
    to the files b.xxx, s.xxx, d.xxx. 
    Existing files with these names will be overwritten.
    If x is a solution, a list of solutions, or does not contain any
    bifurcation diagram or diagnostics data, then only the file s.xxx
    is saved to.

    Type FUNC('xxx') to save the output-files fort.7, fort.8, fort.9,
    to b.xxx, s.xxx, d.xxx (if you are using the default filename
    templates).  Existing files with these names will be overwritten.
    """
    
    type=SIMPLE
    shortName="save"
    def __init__(self,name1,name2=None,templates=None):
        self.parsed = None
        if not name2 is None:
            self.parsed = name1
            name1 = name2
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
	rval=valueSystem()
        for s in ["bifurcationDiagram","solution","diagnostics"]:
            n1 = self.name1[s]
            if os.path.exists(n1):
                shutil.copy(n1,n1+'~')

        if self.parsed:
            n1b = self.name1["bifurcationDiagram"]
            n1s = self.name1["solution"]
            n1d = self.name1["diagnostics"]        
            if (isinstance(self.parsed,bifDiag.bifDiag) and
                len(self.parsed) > 0 and len(self.parsed[0]) > 0):
                self.parsed.writeFilename(n1b,n1s,n1d)
                msg = "Saving to %s, %s, and %s ... done\n"%(n1b,n1s,n1d)
            else:
                if (type(self.parsed) == type([]) and
                    isinstance(self.parsed[0], parseS.AUTOSolution)):
                    self.parsed = parseS.parseS(self.parsed)
                self.parsed.writeFilename(n1s)
                msg = "Saving to %s ... done\n"%(n1s)
            rval.info(msg)
            return rval
        
        i = 7
        for s in ["bifurcationDiagram","solution","diagnostics"]:
            n1 = self.name1[s]
            forti = "fort." + str(i)
            i = i + 1
            if os.path.exists(forti):
                shutil.copy(forti,n1)
                rval.info("Saving %s as %s ... done\n"%(forti,n1))
        return rval
        
class commandDeleteDataFiles(commandWithFilenameTemplate):
    """Delete data files.

    Type FUNC('xxx') to delete the data-files d.xxx, b.xxx, and s.xxx
    (if you are using the default filename templates).
    """
    
    def __init__(self,name1,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
	rval=valueSystem()
        n1b = self.name1["bifurcationDiagram"]
        n1s = self.name1["solution"]
        n1d = self.name1["diagnostics"]
        if os.path.exists(n1b):
            os.remove(n1b)
            rval.info("Deleting %s ... done\n"%n1b)
        if os.path.exists(n1s):
            os.remove(n1s)
            rval.info("Deleting %s ... done\n"%n1s)
        if os.path.exists(n1d):
            os.remove(n1d)
            rval.info("Deleting %s ... done\n"%n1d)
        return rval

class commandDeleteLabel(commandWithFilenameTemplate):
    def __init__(self,typenames=None,name1=None,templates=None,keepTY=0,keep=0):
        self.name = name1
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
        self.typenames=typenames
        self.keepTY=keepTY
        self.keep=keep
        
    def __call__(self):
        codes=self.typenames
        if hasattr(codes,'deleteLabel'):
            data = codes.deleteLabel(self.name,keepTY=self.keepTY,
                                     keep=self.keep,copy=1)
            return valueStringAndData("",data)
        if self.name1["solution"] is None:
            changedb='fort.7'
            changeds='fort.8'
        else:
            changedb=self.name1["bifurcationDiagram"]
            changeds=self.name1["solution"]
        bs=bifDiag.bifDiag(changedb,changeds)
        bs.deleteLabel(codes,keepTY=self.keepTY,keep=self.keep)
        origb=changedb+'~'
        origs=changeds+'~'
        try:
            os.remove(origb)
        except:
            pass
        try:
            os.remove(origs)
        except:
            pass
        os.rename(changedb,origb)
        os.rename(changeds,origs)
        bs.writeFilename(changedb,changeds)
        return valueString("")

class commandDeleteSpecialPoints(commandDeleteLabel):
    """Delete special points.

    Type FUNC(list,x) to delete the special points in list from
    the Python object x, which must be a solution list or a bifurcation diagram.
    Type FUNC(list,'xxx') to delete the special points in list from
    the data-files b.xxx, and s.xxx.
    (if you are using the default filename templates).
    Type FUNC(list) to delete the special points in list from
    the data-files fort.7 and fort.8.
    list is a label number or type name code, or a list of those,
    such as 1, or [2,3], or 'UZ' or ['BP','LP'], or it can be None or
    omitted to mean the special points ['BP','LP','HB','PD','TR','EP','MX']
    """
    
    def __init__(self,typenames=None,name1=None,templates=None):
        commandDeleteLabel.__init__(self,typenames,name1,templates)
        
class commandKeepSpecialPoints(commandDeleteLabel):
    """Keep special points.

    Type FUNC(list,x) to only keep the special points in list from
    the Python object x, which must be a solution list or a bifurcation diagram.
    Type FUNC(list,'xxx') to only keep the special points in list from
    the data-files b.xxx, and s.xxx.
    (if you are using the default filename templates).
    Type FUNC(list) to only keep the special points in list from
    the data-files fort.7 and fort.8.
    list is a label number or type name code, or a list of those,
    such as 1, or [2,3], or 'UZ' or ['BP','LP'], or it can be None or
    omitted to mean ['BP','LP','HB','PD','TR','EP','MX'], deleting 'UZ' and
    regular points.
    """
    
    def __init__(self,typenames=None,name1=None,templates=None):
        commandDeleteLabel.__init__(self,typenames,name1,templates,keep=1)

class commandDeleteLabels(commandDeleteLabel):
    """Delete special labels.

    Type FUNC(list,x) to delete the special points in list from
    the Python object x, which must be a solution list or a bifurcation diagram.
    Type FUNC(list,'xxx') to delete the special points in list from
    the data-files b.xxx, and s.xxx.
    (if you are using the default filename templates).
    Type FUNC(list) to delete the special points in list from
    the data-files fort.7 and fort.8.
    Type information is kept in the bifurcation diagram for plotting.
    list is a label number or type name code, or a list of those,
    such as 1, or [2,3], or 'UZ' or ['BP','LP'], or it can be None or
    omitted to mean the special points ['BP','LP','HB','PD','TR','EP','MX']
    """
    
    def __init__(self,typenames=None,name1=None,templates=None):
        commandDeleteLabel.__init__(self,typenames,name1,templates,keepTY=1)
        
class commandKeepLabels(commandDeleteLabel):
    """Keep special labels.

    Type FUNC(list,x) to only keep the special points in list from
    the Python object x, which must be a solution list or a bifurcation diagram.
    Type FUNC(list,'xxx') to only keep the special points in list from
    the data-files b.xxx, and s.xxx.
    (if you are using the default filename templates).
    Type FUNC(list) to only keep the special points in list from
    the data-files fort.7 and fort.8.
    Type information is kept in the bifurcation diagram for plotting.
    list is a label number or type name code, or a list of those,
    such as 1, or [2,3], or 'UZ' or ['BP','LP'], or it can be None or
    omitted to mean ['BP','LP','HB','PD','TR','EP','MX'], deleting 'UZ' and
    regular points.
    """
    
    def __init__(self,typenames=None,name1=None,templates=None):
        commandDeleteLabel.__init__(self,typenames,name1,templates,keepTY=1,keep=1)

class commandExpandData(commandWithFilenameTemplate):
    def __init__(self,name1=None,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
	rval=valueSystem()
        n1b = self.name1["bifurcationDiagram"]
        n1s = self.name1["solution"]
        if n1s is None:
            n1s = "fort.8"
            n1b = "fort.7"
        if os.path.exists(n1b):
            shutil.copy(n1b,n1b+'~')
        if os.path.exists(n1s):
            shutil.copy(n1s,"fort.28")
            if os.path.exists(n1s+'~'):
                os.remove(n1s+'~')
            os.rename(n1s,n1s+'~')
        rval.interact(os.path.expandvars("$AUTO_DIR/bin/%s"%self.command))
        os.rename("fort.38",n1s)
        if os.path.exists("fort.28"):
            os.remove("fort.28")
        rval.info("Solution doubling done.\n")
        return rval

class commandDouble(commandExpandData):
    """Double a solution.

    Type FUNC() to double the solution in 'fort.7' and 'fort.8'.

    Type FUNC('xxx') to double the solution in b.xxx and s.xxx (if you
    are using the default filename templates).
    """
    
    def __init__(self,name1=None,templates=None):
        commandExpandData.__init__(self,name1,templates)
        self.command = "double"
        
class commandMoveFiles(commandWithFilenameTemplate):
    """Move data-files to a new name.

    Type FUNC('xxx','yyy') to move the data-files b.xxx, s.xxx, d.xxx,
    and c.xxx to b.yyy, s.yyy, d.yyy, and c.yyy (if you are using the
    default filename templates).  """
    
    def __init__(self,name1,name2,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,name2,templates)
    def __call__(self):
	rval=valueSystem()
        for s in ["bifurcationDiagram","solution","diagnostics","constants"]:
            n1 = self.name1[s]
            n2 = self.name2[s]
            if os.path.exists(n1):
                if os.path.exists(n2):
                    os.remove(n2)
                os.rename(n1,n2)
                rval.info("Renaming %s as %s ... done\n"%(n1,n2))
        return rval

class commandParseConstantsFile(commandWithFilenameTemplate):
    """Get the current continuation constants.

    Type FUNC('xxx') to get a parsed version of the constants file
    c.xxx (if you are using the default filename templates).

    This is equivalent to the command
    loadbd('xxx').c
    """
    
    def __init__(self,name1,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
        data = parseC.parseC(self.name1["constants"])
        return valueStringAndData("Parsed file: %s\n"%self.name1["constants"],
                                  data)

class commandParseHomcontFile(commandWithFilenameTemplate):
    """Get the current continuation constants.

    Type FUNC('xxx') to get a parsed version of the HomCont file
    h.xxx (if you are using the default filename templates).
    """
    
    def __init__(self,name1,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
        data = parseH.parseH(self.name1["homcont"])
        return valueStringAndData("Parsed file: %s\n"%self.name1["homcont"],
                                  data)
        
class commandParseSolutionFile(commandWithFilenameTemplate):
    """Parse solution file:

    Type FUNC('xxx') to get a parsed version of the solution file
    s.xxx (if you are using the default filename templates).

    This is equivalent to the command
    loadbd('xxx')()
    """

    def __init__(self,name1=None,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
        n1s = self.name1["solution"]
        if n1s is None:
            n1s = "fort.8"
        data = parseS.parseS(n1s)
        return valueStringAndData("Parsed file: %s\n"%n1s,data)
        
class commandParseDiagramFile(commandWithFilenameTemplate):
    """Parse a bifurcation diagram.

    Type FUNC('xxx') to get a parsed version of the diagram file b.xxx
    (if you are using the default filename templates).

    This is equivalent to the command loadbd('xxx') but without the
    solutions in s.xxx and without the diagnostics in d.xxx.
    """

    def __init__(self,name1=None,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
        n1b = self.name1["bifurcationDiagram"]
        if n1b is None:
            n1b = "fort.7"
        data = parseB.parseB(n1b)
        return valueStringAndData("Parsed file: %s\n"%n1b,data)

class commandParseDiagramAndSolutionFile(commandWithFilenameTemplate):
    """Parse both bifurcation diagram and solution.

    Type FUNC('xxx') to get a parsed version of the diagram file b.xxx
    and solution file s.xxx (if you are using the default filename
    templates).

    This is equivalent to the command loadbd('xxx') but without the
    diagnostics in d.xxx.
    """
    
    def __init__(self,name1=None,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
        n1b = self.name1["bifurcationDiagram"]
        n1s = self.name1["solution"]
        if n1b is None:
            n1b = "fort.7"
            n1s = "fort.8"
        data = parseBandS.parseBandS(n1b,n1s)
        output_names = n1b + " and " + n1s
        return valueStringAndData("Parsed files: %s\n"%output_names,data)


class commandQueryDiagnostic(commandWithFilenameTemplate):
    def __init__(self,diagnostic,name1=None,templates=None):
        self.diagnostic = diagnostic
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
	rval=valueSystem()
        n1d = self.name1["diagnostics"]
        if n1d is None:
            n1d = "fort.9"
        try:
            f = open(n1d)
        except TypeError:
            for branch in n1d:
                if hasattr(branch,"diagnostics"):
                    for s in string.split(str(branch.diagnostics),"\n"):
                        if string.find(s,self.diagnostic) != -1:
                            rval.info(s+"\n")
            rval.info("\n")
            return rval
        try:
            for s in f:
                if self.diagnostic in s:
                    rval.info(s)
        except:
            while 1:
                s = f.readline()
                if s == "":
                    break
                if string.find(s,self.diagnostic) != -1:
                    rval.info(s)
        f.close()
        rval.info("\n")
        return rval

class commandQueryBranchPoint(commandQueryDiagnostic):
    """Print the ``branch-point function''.
    
    Type FUNC(x) to list the value of the ``branch-point function'' 
    in the diagnostics of the bifurcation diagram object x.
    This function vanishes at a branch point.

    Type FUNC() to list the value of the ``branch-point function'' 
    in the output-file fort.9.
    
    Type FUNC('xxx') to list the value of the ``branch-point function''
    in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"BP",name1,templates)
        
class commandQueryEigenvalue(commandQueryDiagnostic):
    """Print eigenvalues of Jacobian (algebraic case).

    Type FUNC(x) to list the eigenvalues of the Jacobian 
    in the diagnostics of the bifurcation diagram object x.
    (Algebraic problems.)

    Type FUNC() to list the eigenvalues of the Jacobian 
    in fort.9. 

    Type FUNC('xxx') to list the eigenvalues of the Jacobian 
    in the info file 'd.xxx'.
    """

    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"Eigenvalue",name1,templates)

class commandQueryFloquet(commandQueryDiagnostic):
    """Print the Floquet multipliers.

    Type FUNC(x) to list the Floquet multipliers
    in the diagnostics of the bifurcation diagram object x.
    (Differential equations.)

    Type FUNC() to list the Floquet multipliers
    in the output-file fort.9. 

    Type FUNC('xxx') to list the Floquet multipliers 
    in the info file 'd.xxx'.
    """

    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"Mult",name1,templates)

class commandQueryHopf(commandQueryDiagnostic):
    """Print the value of the ``Hopf function''.

    Type FUNC(x) to list the value of the ``Hopf function'' 
    in the diagnostics of the bifurcation diagram object x.
    This function vanishes at a Hopf bifurcation point.

    Type FUNC() to list the value of the ``Hopf function'' 
    in the output-file fort.9.

    Type FUNC('xxx') to list the value of the ``Hopf function''
    in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"Hopf",name1,templates)

class commandQueryIterations(commandQueryDiagnostic):
    """Print the number of Newton interations.

    Type FUNC(x) to list the number of Newton iterations per
    continuation step in the diagnostics of the bifurcation diagram
    object x.

    Type FUNC() to list the number of Newton iterations per
    continuation step in fort.9. 

    Type FUNC('xxx') to list the number of Newton iterations per
    continuation step in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"Iterations",name1,templates)

class commandQueryLimitpoint(commandQueryDiagnostic):
    """Print the value of the ``limit point function''.

    Type FUNC(x) to list the value of the ``limit point function'' 
    in the diagnostics of the bifurcation diagram object x.
    This function vanishes at a limit point (fold).

    Type FUNC() to list the value of the ``limit point function'' 
    in the output-file fort.9.

    Type FUNC('xxx') to list the value of the ``limit point function'' 
    in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"Fold",name1,templates)

class commandQueryNote(commandQueryDiagnostic):
    """Print notes in info file.

    Type FUNC(x) to show any notes 
    in the diagnostics of the bifurcation diagram
    object x.

    Type FUNC() to show any notes 
    in the output-file fort.9.

    Type FUNC('xxx') to show any notes 
    in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"NOTE",name1,templates)

class commandQuerySecondaryPeriod(commandQueryDiagnostic):
    """Print value of ``secondary-periodic bif. fcn''.

    Type FUNC(x) to list the value of the
    ``secondary-periodic bifurcation function'' 
    in the diagnostics of the bifurcation diagram object x.
    This function vanishes at period-doubling and torus bifurcations.

    Type FUNC()  to list the value of the 
    ``secondary-periodic bifurcation function'' 
    in the output-file 'fort.9.

    Type FUNC('xxx') to list the value of the
    ``secondary-periodic bifurcation function''
    in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"SPB",name1,templates)

class commandQueryStepsize(commandQueryDiagnostic):
    """Print continuation step sizes.

    Type FUNC(x) to list the continuation step size for each
    continuation step in the diagnostics of the bifurcation diagram
    object x.

    Type FUNC() to list the continuation step size for each
    continuation step in  'fort.9. 

    Type FUNC('xxx') to list the continuation step size for each
    continuation step in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"Step",name1,templates)

class commandTriple(commandExpandData):
    """Triple a solution.

    Type FUNC() to triple the solution in 'fort.8'.

    Type FUNC('xxx') to triple the solution in s.xxx (if you
    are using the default filename templates).
    """

    def __init__(self,name1=None,templates=None):
        commandExpandData.__init__(self,name1,templates)
        self.command = "triple"
        
############################################
#  System Commands
############################################

class commandLs(command):
    """List the current directory.
    
    Type 'FUNC' to run the system 'ls' command in the current directory.  This
    command will accept whatever arguments are accepted by the Unix command
    'ls'.
    """
    def __init__(self,dir=None):
        self.dir = dir
    def __call__(self):
        cmd = "ls"
        if os.name in ["nt", "dos"]:
            path = string.split(os.environ["PATH"],os.pathsep)
            cmd = "dir" 
            for s in path:
                if os.path.exists(os.path.join(s,"ls.exe")):
                    cmd = "ls"
                    break
        if self.dir is None:
            os.system(cmd)
        else:
            os.system("%s %s"%(cmd,self.dir,))
        return valueString("")
        
class commandQuit(command):
    def __init__(self):
        pass
    def __call__(self):
        try:
            quit()
        except:
            sys.exit()
        return valueString("")

class commandShell(command):
    """Run a shell command.
        
    Type FUNC('xxx') to run the command 'xxx' in the Unix shell and display
    the results in the AUTO command line user interface.
    """
    
    def __init__(self,command):
        self.command = command
    def __call__(self):
        os.system(self.command) 
        return valueString("")

class commandWait(command):
    """Wait for the user to enter a key.

    Type 'FUNC()' to have the AUTO interface wait
    until the user hits any key (mainly used in scripts).
    """

    def __call__(self):
        print "Hit <return> to continue"
        raw_input()
        return valueString("")
          
class commandCat(commandShell):
    """Print the contents of a file

    Type 'FUNC xxx' to list the contents of the file 'xxx'.  This calls the
    Unix function 'cat' for reading the file.  
    """
    
    def __init__(self,command=None):
        self.command = "cat"
        if not command is None:
            self.command = self.command + " " + command


############################################
#  Commands which use runAUTO
############################################       
class commandWithRunner(command):
    def __init__(self,runner=None):
        if runner is None:
            global _runner
            self.runner = _runner
        else:
            self.runner = runner

class commandCd(commandWithRunner):
    """Change directories.
    
    Type 'FUNC xxx' to change to the directory 'xxx'.  This command
    understands both shell variables and home directory expansion.
    """
    def __init__(self,dir=None,runner=None):
        self.dir = dir
        commandWithRunner.__init__(self,runner)
    def __call__(self):
        if self.dir is None or self.dir == '':
            self.dir = os.path.expanduser("~")
        try:
            self.dir = os.path.expanduser(self.dir)
            self.dir = os.path.expandvars(self.dir)
            os.chdir(self.dir)
        except:
            print "Directory '%s' not found"%(self.dir,)
        self.runner.config(dir=os.getcwd())      
        return valueString("")

class commandRunnerConfig(commandWithFilenameTemplate,commandWithRunner):
    """Load files into the AUTO runner or return modified solution data.

    Type result=FUNC([options]) to modify the AUTO runner.

    The type of the result is a solution object.

    There are many possible options:
    \\begin{verbatim}
    Long name   Short name    Description
    -------------------------------------------
    equation    e             The equations file
    constants   c             The AUTO constants file
    homcont     h             The Homcont parameter file
    solution    s             The restart solution file
                NDIM,IPS,etc  AUTO constants.
                BR,PT,TY,LAB  Solution constants.
    \\end{verbatim}
    Options which are not explicitly set retain their previous value.
    For example one may type: s=FUNC(e='ab',c='ab.1') to use 'ab.c' as
    the equations file and c.ab.1 as the constants file (if you are
    using the default filename templates).

    You can also specify AUTO Constants, e.g., DS=0.05, or IRS=2.
    Special values for DS are '+' (forwards) and '-' (backwards).
    Example: s = FUNC(s,DS='-') changes s.c['DS'] to -s.c['DS'].
    """
    def __init__(self,runner=None,templates=None,cnf={},**kw):
        commandWithFilenameTemplate.__init__(self,None,None,templates)
        commandWithRunner.__init__(self,runner)
        dict = AUTOutil.cnfmerge((cnf,kw))
        self.configDict = dict
    
    def __applyRunnerConfigResolveAbbreviation(self,kw={}):
        abbrev = {}
        for key in ["equation", "constants", "solution", "homcont"]:
            abbrev[key[0]] = key
            abbrev[key]    = key
        for key in kw.keys():
            # remove long duplicates
            if (abbrev.has_key(key) and key != abbrev[key] and
                kw.has_key(abbrev[key])):
                del kw[abbrev[key]]
        for key,value in kw.items():
            if abbrev.has_key(key):
                # change the abbreviation to the long version
                del kw[key]
                if type(value) in [type(""),type(1),type(1.0)]:
                    kw[abbrev[key]] = self._applyTemplate(value,abbrev[key])
                else:
                    kw[abbrev[key]] = value
        return kw

    def __applyRunnerConfigResolveFilenames(self,kw={}):
        doneread = False
        wantread = False
        if kw.has_key("constants"):
            if type(kw["constants"]) == types.StringType:
                wantread = True
                try:
                    kw["constants"] = parseC.parseC(kw["constants"])
                    doneread = True
                except IOError:
                    del kw["constants"]
        if kw.has_key("homcont"):
            if type(kw["homcont"]) == types.StringType:
                wantread = True
                object = parseH.parseH()
                try:
                    object.readFilename(kw["homcont"])
                    doneread = True
                except IOError:
                    #sys.stdout.write("Could not open file '%s', defaulting to empty file\n"%kw["homcont"])
                    object = None
                kw["homcont"] = object
        if kw.has_key("solution"):
            if type(kw["solution"]) == types.StringType:
                wantread = True
                try:
                    object = parseS.parseS()
                    apply(object.readFilename,(kw["solution"],),kw)
                    doneread = True
                except IOError:
                    #sys.stdout.write("Could not open file '%s', defaulting to empty file\n"%kw["solution"])
                    object = None
                kw["solution"] = object
        if wantread and not doneread:
            if kw.has_key("equation"):
                eq = kw["equation"][14:]
                for ext in [".f90",".f",".c"]:
                    if os.path.exists(eq+ext):
                        doneread = True
                        break
            if not doneread:
                raise IOError("No files found.")
        return kw

    def __call__(self):
        dict = self.__applyRunnerConfigResolveAbbreviation(self.configDict)
        dict = self.__applyRunnerConfigResolveFilenames(dict)
        if hasattr(self.runner,'load'):
            data = apply(self.runner.load,(),dict)
        else:
            self.runner.config(dict)
            options = self.runner.options
            if hasattr(options["solution"],'load'):
                data = apply(options["solution"].load,(),options)
            else:
                if dict.has_key('t'):
                    options = options.copy()
                    options['t'] = dict['t']
                data = apply(parseS.AUTOSolution,(options["solution"],),options)
        return valueStringAndData("Runner configured\n",data)

class commandRunnerLoadName(commandRunnerConfig):
    """Load files into the AUTO runner or return modified solution data.

    Type result=FUNC([options]) to modify the AUTO runner.
    Type result=FUNC(data,[options]) to return possibly
    modified solution data.

    The type of the result is a solution object.

    FUNC(data,[options]) returns a solution in the following way for
    different types of data:

    * A solution: load returns the solution data, with AUTO constants
      modified by options.

    * A bifurcation diagram or a solution list:
      returns the solution specified by
      the AUTO constant IRS, or if IRS is not specified, the last solution
      in s.

    * A string: AUTO uses the solution in the file 's.s' together with the
      constants in the files 'c.s', and 'h.s'. Not all of these
      files need to be present.

    * A Python list array or a numpy array representing a solution,
      returns a solution with the given contents. Such an array must be given
      column-wise, as [[t0, ..., tn], [x0, ..., xn], [y0, ..., yn], ...].

    There are many possible options:
    \\begin{verbatim}
    Long name   Short name    Description
    -------------------------------------------
    equation    e             The equations file
    constants   c             The AUTO constants file
    homcont     h             The Homcont parameter file
    solution    s             The restart solution file
                NDIM,IPS,etc  AUTO constants.
                BR,PT,TY,LAB  Solution constants.
    \\end{verbatim}
    If data is not specified or data is a string then options which
    are not explicitly set retain their previous value.
    For example one may type: s=FUNC(e='ab',c='ab.1') to use 'ab.c' as
    the equations file and c.ab.1 as the constants file (if you are
    using the default filename templates).

    Type s=FUNC('name') to load all files with base 'name'.
    This does the same thing as running
    s=FUNC(e='name',c='name,h='name',s='name').
 
    You can also specify AUTO Constants, e.g., DS=0.05, or IRS=2.
    Special values for DS are '+' (forwards) and '-' (backwards).
    Example: s = FUNC(s,DS='-') changes s.c['DS'] to -s.c['DS'].
    """
    type="simple"
    shortName="loadName"
    def __init__(self,name=None,runner=None,templates=None,cnf={},**kw):
        if runner is None and name is not None and type(name) not in [
                type(""),type(1),type(1.0)]:
            if isinstance(name, (runAUTO.runAUTO,bifDiag.bifDiag)):
                runner = name
            else:
                kw["s"] = name
            name = None 
        elif name is not None:
            for key in ["equation", "constants", "solution", "homcont"]:
                if not kw.has_key(key):
                    kw[key] = name
        commandRunnerConfig.__init__(self,runner,templates,
                                     AUTOutil.cnfmerge((kw,cnf)))
        

class commandParseOutputFiles(commandWithFilenameTemplate):
    """Load bifurcation diagram files.

    Type b=FUNC([options]) to load output files or output data.
    There are three possible options:
    \\begin{verbatim}
    Long name   Short name    Description
    -------------------------------------------
    bifurcationdiagram   b    The bifurcation diagram file
    solution    s             The solution file or list of solutions
    diagnostics d             The diagnostics file
    \\end{verbatim}

    Type FUNC('name') to load all files with base 'name'.
    This does the same thing as running
    FUNC(b='name',s='name,d='name').
    plot(b) will then plot the 'b' and 's' components.

    Returns a bifurcation diagram object representing the files in b.
    """
    type="simple"
    shortName="loadbd"
    def __init__(self,name=None,templates=None,cnf={},**kw):
        if name is not None:
            for key in ["bifurcationDiagram", "solution", "diagnostics"]:
                if not kw.has_key(key):
                    kw[key] = name
        commandWithFilenameTemplate.__init__(self,None,None,templates)
        dict = AUTOutil.cnfmerge((cnf,kw))
        self.configDict = dict
    
    def __applyBsdConfigResolveAbbreviation(self,kw={}):
        abbrev = {}
        for key in ["bifurcationDiagram", "solution", "diagnostics"]:
            abbrev[key[0]] = key
            abbrev[key]    = key
        for key in kw.keys():
            # remove long duplicates
            if (abbrev.has_key(key) and key != abbrev[key] and
                kw.has_key(abbrev[key])):
                del kw[abbrev[key]]
        for key,value in kw.items():
            if abbrev.has_key(key):
                # change the abbreviation to the long version
                del kw[key]
                if type(value) in [type(""),type(1),type(1.0)]:
                    kw[abbrev[key]] = self._applyTemplate(value,abbrev[key])
                else:
                    kw[abbrev[key]] = value
        return kw

    def __call__(self):
        dict = self.__applyBsdConfigResolveAbbreviation(self.configDict)
        bname = dict.get("bifurcationDiagram")
        sname = dict.get("solution")
        dname = dict.get("diagnostics")
        data = bifDiag.bifDiag(bname,sname,dname,
                               verbose = _runner.options["verbose"],
                               makefile = _runner.options["makefile"])
        return valueStringAndData("Parsed output data\n",data)

class commandRunnerPrintFort2(commandWithRunner):
    """Print continuation parameters.

    Type FUNC() to print all the parameters.
    Type FUNC('xxx') to return the parameter 'xxx'.
    These commands are equivalent to the commands
    print s.c
    print s.c['xxx']
    where s is a solution.
    """
    
    def __init__(self,parameter=None,runner=None):
        self.parameter = parameter
        commandWithRunner.__init__(self,runner)
    def __call__(self):
        if self.parameter is None:
            return valueString(str(self.runner.options["constants"]))
        else:
            return valueStringAndData("",
                                      self.runner.options["constants"][self.parameter])

class commandRunnerPrintFort12(commandWithRunner):
    """Print HomCont continuation parameters.

    Type FUNC() to print all the HomCont parameters.
    Type FUNC('xxx') to return the HomCont parameter 'xxx'.
    These commands are equivalent to the commands
    print s.c
    print s.c['xxx']
    where s is a solution.
    """
    
    def __init__(self,parameter=None,runner=None):
        self.parameter = parameter
        commandWithRunner.__init__(self,runner)
    def __call__(self):
        if self.parameter is None:
            return valueString(str(self.runner.options["homcont"]))
        else:
            return valueStringAndData("",
                                      self.runner.options["homcont"][self.parameter])

class commandRunnerConfigFort2(commandWithRunner):
    """Modify continuation constants.

    Type FUNC('xxx',yyy) to change the constant 'xxx' to have
    value yyy.
    This is equivalent to the command
    s=load(s,xxx=yyy)
    where s is a solution.
    """

    type=SIMPLE
    shortName="changeConstants"
    def __init__(self,entry=None,value=None,runner=None,**kw):
        self.entry = None
        if not(entry is None):
            self.entry = entry
        if not(value is None):
            self.value = value
        commandWithRunner.__init__(self,runner)            
        self.kw = kw
    def __call__(self):
        if not(self.entry is None):
            self.runner.options["constants"][self.entry] = self.value
            return valueString("%s changed to %s\n"%(self.entry,self.value))
        func=commandRunnerLoadName(None,None,None,self.kw)
        func()
        return valueString(str(self.kw)+'\n')

class commandRunnerConfigFort12(commandWithRunner):
    """Modify continuation constants.

    Type FUNC('xxx',yyy) to change the HomCont constant 'xxx' to have
    value yyy.
    This is equivalent to the command
    s=load(s,xxx=yyy)
    where s is a solution.
    """

    type=SIMPLE
    shortName="changeConstantsHomCont"
    def __init__(self,entry=None,value=None,runner=None,**kw):
        self.entry = None
        if not(entry is None):
            self.entry = entry
        if not(value is None):
            self.value = value
        commandWithRunner.__init__(self,runner)            
        self.kw = kw
    def __call__(self):
        if not(self.entry is None):
            self.runner.options["homcont"][self.entry] = self.value
            return valueString("%s changed to %s\n"%(self.entry,self.value))
        func=commandRunnerLoadName(None,None,None,self.kw)
        func()
        return valueString(str(self.kw)+'\n')

    
class commandSetDirectory(commandWithRunner):
    def __init__(self,directory,runner=None):
        commandWithRunner.__init__(self,runner)
        self.directory = directory
    def __call__(self):
        self.runner.config(dir=self.directory)
        return valueString("Directory set to %s\n"%self.directory)

class commandRun(commandWithRunner,commandWithFilenameTemplate):
    """Run AUTO.

    Type r=FUNC([s],[options]) to run AUTO from solution s with the given
    AUTO constants or file keyword options.
    
    The results are stored in the bifurcation diagram r which you can
    later print with ``print r'', obtain branches from via r[0], r[1], ...,
    and obtain solutions from via r(3), r(5), r('LP2'), where 1 and 5
    are label numbers, and 'LP2' refers to the second LP label.

    FUNC(s) runs AUTO in the following way for different types of s:

    * A solution: AUTO starts from solution s, with AUTO constants s.c.

    * A bifurcation diagram: AUTO start from the solution specified by
      the AUTO constant IRS, or if IRS is not specified, the last solution
      in s, s()[-1], with AUTO constants s()[-1].c.

    * A string: AUTO uses the solution in the file 's.s' together with the
      constants in the files 'c.s', and 'h.s'. Not all of these
      files need to be present.

    If no solution s is specified, then the global values from the
    'load' command are used instead, where
    options which are not explicitly set retain their previous value.

    Keyword argument options can be AUTO constants, such as DS=0.05,
    or ISW=-1, or specify a constant or solution file. These override
    the constants in s.c, where applicable. See ``load'':
    FUNC(s,options) is equivalent to FUNC(load(s,options))

    Example: given a bifurcation diagram bd, with a branch point
    solution, switch branches and stop at the first Hopf bifurcation:
    hb = FUNC(bd('BP1'),ISW=-1,SP='HB1')
    
    Special keyword arguments are 'sv' and 'ap'; 'sv' is also an AUTO
    constant:
    FUNC(bd('BP1'),ISW=-1,SP='HB1',sv='hb',ap='all')
    saves to the files b.hb, s.hb and d.hb, and appends to b.all,
    s.all, and d.all.
    """
    type=SIMPLE
    shortName="run"
    def __init__(self,name=None,sv=None,ap=None,runner=None,templates=None,**kw):
        self.name = name
        self.runner = runner
        self.templates = templates
        if sv is not None:
            kw = kw.copy()
            kw['sv'] = sv
        self.kw = kw
        self.ap = ap
        if (runner is None and name is not None and type(name) != type("")
            and type(name) != type(1)):
            if isinstance(name, (runAUTO.runAUTO,bifDiag.bifDiag)):
                self.runner = name
            elif not kw.has_key("s"):
                self.kw["s"] = name
            self.name = None

    def __call__(self):
        func=commandRunnerLoadName(self.name,self.runner,self.templates,self.kw)
        runner = func().data
        err = cStringIO.StringIO()
        sv = (runner.options.get("constants") or {}).get("sv")
        if sv == '':
            sv = None
        if runner.options["verbose"] == "no":
            log = cStringIO.StringIO()
            data = runner.run(log=log,err=err)
            log.seek(0)
            err.seek(0)
            ret = valueRun(log,err,data=data)
            log.close()
        else:
            # log was already written if the runner is verbose
            data = runner.run(err=err)
            err.seek(0)
            ret = valueRun(err,data=data)
        err.close()
        if sv is not None:
            commandWithFilenameTemplate.__init__(self,sv,None,
                                                 self.templates)
            bname = self.name1["bifurcationDiagram"]
            sname = self.name1["solution"]
            dname = self.name1["diagnostics"]
            ret.value = ret.value + "Saving to %s, %s, and %s ... done\n"%(
                bname,sname,dname)
        if self.ap is not None:
            if sv is None:
                func=commandAppend(self.ap)
            else:
                func=commandAppend(sv,self.ap)
            rval=func()
            ret.value = ret.value + rval.value
        if self.runner is None:
            # delete ["sv"] from the global runner
            global _runner
            c = _runner.options.get("constants") or {}
            if c.has_key("sv"):
                c["sv"] = None
        return ret

class commandRunDemo(commandWithRunner):
    def __init__(self,demo,equation="all",runner=None):
        self.demo = demo
        self.equation = equation
        commandWithRunner.__init__(self,runner)
    def __call__(self):
        self.runner.config(equation=self.equation)
        log,err,data = self.runner.runDemo(self.demo)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err,data=data)
        else:
            return valueRun(log,err,data=data)

class commandRunMakefileWithSetup(commandWithRunner):
    def __init__(self,equation=None,fort2=None,fort3=None,runner=None):
        commandWithRunner.__init__(self,runner)
        self.equation = equation
        self.fort2 = fort2
        self.fort3 = fort3
    def __call__(self):
        if not(self.fort2 is None):
            self.runner.config(fort2=self.fort2)
        if not(self.fort3 is None):
            self.runner.config(fort3=self.fort3)
        # Before this is called runner needs to have the fort2 and fort3
        # options set.  Otherwise this will raise an exception.
        log,err,data = self.runner.runMakefileWithSetup(self.equation)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err,data=data)
        else:
            return valueRun(log,err,data=data)

class commandRunMakefile(command):
    def __init__(self,equation=None,runner=None):
        commandWithRunner.__init__(self,runner)
        self.equation = equation
    def __call__(self):
        log,err,data = self.runner.runMakefile(self.equation)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err,data=data)
        else:
            return valueRun(log,err,data=data)

class commandRunExecutableWithSetup(command):
    def __init__(self,executable=None,fort2=None,fort3=None,runner=None):
        commandWithRunner.__init__(self,runner)
        self.executable = executable
        self.fort2 = fort2
        self.fort3 = fort3
    def __call__(self):
        if not(self.fort2 is None):
            self.runner.config(fort2=self.fort2)
        if not(self.fort3 is None):
            self.runner.config(fort3=self.fort3)
        # Before this is called runner needs to have the fort2 and fort3
        # options set.  Otherwise this will raise an exception.
        log,err,data = self.runner.runExecutableWithSetup(self.executable)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err,data=data)
        else:
            return valueRun(log,err,data=data)

class commandRunExecutable(command):
    def __init__(self,executable=None,fort2=None,fort3=None,runner=None):
        commandWithRunner.__init__(self,runner)
        self.executable = executable
        self.fort2 = fort2
        self.fort3 = fort3
    def __call__(self):
        log,err,data = self.runner.runExecutable(self.executable)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err,data=data)
        else:
            return valueRun(log,err,data=data)

class commandRunCommandWithSetup(command):
    def __init__(self,command=None,fort2=None,fort3=None,runner=None):
        commandWithRunner.__init__(self,runner)
        self.command = command
        self.fort2 = fort2
        self.fort3 = fort3
    def __call__(self):
        if not(self.fort2 is None):
            self.runner.config(fort2=self.fort2)
        if not(self.fort3 is None):
            self.runner.config(fort3=self.fort3)
        # Before this is called runner needs to have the fort2 and fort3
        # options set.  Otherwise this will raise an exception.
        log,err,data = self.runner.runCommandWithSetup(self.command)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err,data=data)
        else:
            return valueRun(log,err,data=data)

class commandRunCommand(command):
    def __init__(self,command=None,runner=None):
        commandWithRunner.__init__(self,runner)
        self.command = command
    def __call__(self):
        log,err,data = self.runner.runCommand(self.command)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err,data=data)
        else:
            return valueRun(log,err,data=data)


class commandPlotter3D(command):
    """3D plotting of data.

    Type FUNC(x) to run the graphics program PLAUT04 for the graphical
    inspection of bifurcation diagram or solution data in x.

    Type FUNC('xxx') to run the graphics program PLAUT04 for the graphical
    inspection of the data-files b.xxx and s.xxx (if you are using the
    default filename templates).

    Type FUNC() to run the graphics program PLAUT04 for the graphical
    inspection of the output-files 'fort.7' and 'fort.8'.

    Type FUNC(...,r3b=True) to run PLAUT04 in restricted three body
    problem mode.
    """

    def __init__(self,name1=None,r3b=False):
        self.data = []
        self.r3b = r3b
        if not name1 is None:
            self.data.append(name1)
    def __call__(self):
        cmd = os.path.join(os.path.expandvars("$AUTO_DIR"),"bin")
        if self.r3b:
            cmd = os.path.join(cmd, "r3bplaut04")
        else:
            cmd = os.path.join(cmd, "plaut04")
        arg = []
        if self.data != []:
            d = self.data[0]
            if type(d) == type(""):
                arg = [d]
            else:
                for f in ["fort.7","fort.8","fort.9"]:
                    if os.path.exists(f):
                        os.remove(f)
                if isinstance(d,bifDiag.bifDiag):
                    d.writeFilename("fort.7","fort.8","fort.9")
                elif isinstance(d,parseBandS.parseBandS):
                    d.writeFilename("fort.7","fort.8")
                elif isinstance(d,parseB.parseB):
                    d.writeFilename("fort.7")
                elif isinstance(d,parseS.parseS):
                    d.writeFilename("fort.8")
                elif isinstance(d,parseB.AUTOBranch):
                    d.writeFilename("fort.7")
                elif isinstance(d,parseS.AUTOSolution):
                    d.writeFilename("fort.8")
        if hasattr(os,"spawnv"):
            if not os.path.exists(cmd):
                cmd = cmd + '.exe'
            os.spawnv(os.P_NOWAIT,cmd,[os.path.basename(cmd)] + arg)
        else:
            os.system(string.join([cmd]+arg+["&"]))
        return valueString("")


try:
    import Tkinter
    plotterimported = False
    try:
        import readline
        import atexit
    except:
        pass
    import select

    # this polling loop is here so that Cygwin Python does not "hang" the
    # plot window while Python waits for a user input
    def handleevents():
        while select.select([sys.stdin],[],[],0.02) == ([], [], []):
            _root.dooneevent()

    #####################################################
    #  Plotting commands
    #####################################################

    class commandPlotter(commandWithFilenameTemplate):
        """2D plotting of data.

        Type FUNC(x) to run the graphics program PyPLAUT for the graphical
        inspection of bifurcation diagram or solution data in x.

        Type FUNC('xxx') to run the graphics program PyPLAUT for the graphical
        inspection of the data-files b.xxx and s.xxx (if you are using the
        default filename templates).

        Type FUNC() to run the graphics program for the graphical
        inspection of the output-files 'fort.7' and 'fort.8'.

        The return value will be the handle for the graphics window.
        """

        type=SIMPLE
        shortName="plot"
        def __init__(self,name=None,templates=None,options={},**kw):
            self.options = AUTOutil.cnfmerge((options,kw))
            if type(name) == type("") or name is None:
                commandWithFilenameTemplate.__init__(self,name,None,templates)
                self.parsed = None
            else:
                self.parsed = name
        def quit(self):
            self.handle.destroy()
        def __call__(self):
            # delay importing plotting modules until we actually plot...
            global plotterimported, windowPlotter
            if not plotterimported:
                from graphics import windowPlotter
                plotterimported = True

            # root has to be here since I am passing options in
            # a dictionary.  Otherwise the default agruements
            # get messed up
            # NOTE: options set here go to the MegaToplevel!, while
            # the return value of this function is the underlying
            # grapher.  So we add 'grapher_' to all options that don't
            # already do
            for k, v in self.options.items():
                if k[:8] != 'grapher_':
                    del self.options[k]
                    self.options['grapher_'+k] = v

            # Get rid of the initial window
            root=Tkinter.Tk()
            root.withdraw()
            if sys.platform == "cygwin":
                try:
                    readline.set_pre_input_hook(handleevents)
                    global _root
                    _root=root
                except:
                    pass
            if self.parsed:
                nb, ns = None, None
                if isinstance(self.parsed,bifDiag.bifDiag):
                    nb = self.parsed
                    ns = self.parsed()
                elif isinstance(self.parsed,parseBandS.parseBandS):
                    nb = self.parsed.diagram.branches
                    ns = self.parsed.solution
                elif isinstance(self.parsed,parseB.parseB):
                    nb = self.parsed.branches
                elif isinstance(self.parsed,parseS.parseS):
                    ns = self.parsed
                elif isinstance(self.parsed,parseB.AUTOBranch):
                    nb = parseB.parseBR([self.parsed])
                elif isinstance(self.parsed,parseS.AUTOSolution):
                    ns = parseS.parseS([self.parsed])
                if nb:
                    self.options["grapher_bifurcation_diagram"] = nb
                if ns:
                    self.options["grapher_solution"] = ns                    
            else:
                n1b = self.name1["bifurcationDiagram"]
                n1s = self.name1["solution"]
                if n1b is None:
                    n1b = "fort.7"
                    n1s = "fort.8"
                try:
                    n1b = parseB.parseBR(n1b)
                    options = {"constants": n1b[0].c}
                    n1b = apply(bifDiag.bifDiag,(n1b,n1s),options)
                except IOError:
                    n1b = bifDiag.bifDiag(b,s)
                self.options["grapher_bifurcation_diagram"] = n1b
                self.options["grapher_solution"] = n1b()
            self.handle = windowPlotter.WindowPlotter2D(root,self.options,
                          grapher_width=600,grapher_height=480)
            self.handle.update()
            try:
                atexit.register(self.quit)
            except:
                pass
            return valueStringAndData("Created plotter\n",self.handle)

except:
    print
    print "-------------------------------------------------------------"
    print "Could not import plotting modules, plotting will be disabled."
    print "This is probably because Tkinter is not enabled in your Python installation."
    print "-------------------------------------------------------------"
    print
    class commandPlotter(commandWithFilenameTemplate):
        """2D plotting of data.

        Plotting of data has been disabled in the AUTO-07P CLUI.
        This is probably because the Python interpretor cannot
        load the Tkinter module.
        """
        
        def __call__(self):
            return valueString("2D plotting has been disabled\n")



##################################################
#  CLUI commands
##################################################
class commandHelp(command):
    def __init__(self,command_string=""):
        self.command_string = command_string
        self.__outputString = ""
        # Read in the aliases.
        self._aliases = {}
        parser = AUTOutil.getAUTORC("AUTO_command_aliases")
        for option in parser.options("AUTO_command_aliases"):
            self._aliases[option] = parser.get("AUTO_command_aliases",option)

    def __print(self,text):
        self.__outputString = self.__outputString + text

    def __call__(self):
        command_list = []

        # Here we get a list of the names of all of the commands in AUTOCommands
        import AUTOCommands
        for key in AUTOCommands.__dict__.keys():
            if key in self._aliases.values():
                command_list.append(key)

        return_value = {}
        if type(self.command_string) != types.StringType:
            self.__print(self.command_string.__doc__+'\n')
            return valueStringAndData(self.__outputString,return_value)
        if len(self.command_string) == 0:
            # If we were created with the empty string return a formatted
            # quick reference of all commands as the string and a
            # dictionary of all commands as the data.  The dictionary
            # has an entry for each command which is a dictionary
            # with two entries:
            #   "aliases"  a list of the aliases of the command
            #   "description" a one line description of the command
            command_list.sort()
            self.__print(" ALIASES    DESCRIPTION\n") 
            for cmd in command_list:
                return_value[cmd] = {}
                return_value[cmd]["aliases"] = []
                aliases = ""
                for key in self._aliases.keys():
                    if self._aliases[key] == cmd:
                        aliases = aliases + key + " "
                        return_value[cmd]["aliases"].append(key)
                doc = getattr(AUTOCommands,cmd).__doc__
                if not(doc is None):
                    self.__print(" %-25s"%aliases)
                    doc = string.split(doc,"\n")
                    return_value[cmd]["description"] = doc[0]
                    self.__print(doc[0])
                    self.__print("\n")

            import interactiveBindings            
            execlist = [{'name' : 'auto', 'alias' : 'ex', 
                         'fn' : interactiveBindings.AUTOInteractiveConsole.ex},
                        {'name' : 'demofile', 'alias' : 'dmf',
                         'fn' : interactiveBindings.AUTOInteractiveConsole.dmf}]
            for cmdprop in execlist:
                cmd = cmdprop['name']
                return_value[cmd] = {}
                return_value[cmd]["aliases"] = [cmd,cmdprop['alias']]
                aliases = cmd + " " + cmdprop['alias']
                doc = cmdprop["fn"].__doc__
                self.__print(" %-25s"%aliases)
                doc = string.split(doc,"\n")
                return_value[cmd]["description"] = doc[0]
                self.__print(doc[0])
                self.__print("\n")
                
            self.__print("\n")
        else:
            # If we were created with the nonempty string return a formatted
            # reference for the given command as the string and a
            # dictionary containing information about the command as the data.
            # The dictionary has 3 entries:
            #   "name"  the full name of the command
            #   "aliases"  a list of all of the aliases of the command
            #   "description" a long description of the command
            try:
                doc = getattr(AUTOCommands,self.command_string).__doc__
                return_value["name"] = self.command_string
            except:
                doc = getattr(AUTOCommands,self._aliases[self.command_string]).__doc__
                return_value["name"] = self._aliases[self.command_string]
            doc = re.sub("FUNC",self.command_string,doc)
            return_value["short description"] = string.split(doc,"\n")[0]
            return_value["long description"]  = string.join(string.split(doc,"\n")[1:],"\n")
            # Get rid of the LaTeX stuff from the string that gets returned, but
            # NOT from the data portion
            doc = string.replace(doc,"\\begin{verbatim}","")
            doc = string.replace(doc,"\\end{verbatim}","")
            doc = doc + "\n"

            command_string = self.command_string
            if not self.command_string in command_list:
                # This means help was asked for an alias
                command_string = self._aliases[command_string]
                doc = doc + "Command name: "+command_string+"\n"
            return_value["aliases"] = []
            doc = doc + "Aliases: "
            for key in self._aliases.keys():
                if self._aliases[key] == command_string:
                    doc = doc + key + " "
                    return_value["aliases"].append(key)
            self.__print(doc+"\n")
        return valueStringAndData(self.__outputString,return_value)

# This is just a little wrapper around commandHelp which discards the
# data portion of the return.  This is because, for the
# interactive command line we don't want it to print out.
class commandInteractiveHelp(commandHelp):
    """Get help on the AUTO commands.
    
    Type 'FUNC' to list all commands with a online help.
    Type 'FUNC xxx' to get help for command 'xxx'.
    """
    
    def __init__(self,command_string=""):
        commandHelp.__init__(self,command_string)

    def __call__(self):
        val = commandHelp.__call__(self)
        return valueString(str(val))

##################################################
#  GUI commands
##################################################
class commandPrintFunc(command):
    def __init__(self,printFunc,text):
        self.text = text
        self.printFunc = printFunc
    def __call__(self):
        self.printFunc(self.text)
        return valueString(self.text)

# FIXME: This is not done!!
class commandCreateGUI(command):
    """Show AUTOs graphical user interface.

    Type FUNC() to start AUTOs graphical user interface.
    
    NOTE: This command is not implemented yet.
    """
    def __init__(self,type="simple"):
        self.type=type
        pass
    def __call__(self):
        import Tkinter
        import Pmw
        from graphics import AUTOgui
        # Get rid of the initial window
        root = Tkinter.Tk()
        root.withdraw()
        gui = AUTOgui.AUTOgui(self.type)
        return valueStringAndData("GUI created\n",gui)
    
# Not ready yet
##  class commandRunGeneralGUI(command):
##      def __init__(self,runner):
##          self.runner = runner
##      def __call__(self):
##          tkSimple
##          first = commandSetupGeneralRun(eq_name,saved_data,parameter_name)
##          second = commandRunnerConfig(self.runner,makefile="$AUTO_DIR/cmds/cmds.make")
##          third = commandRunMakefile(self.runner,"EQUATION_NAME=%s"%(eq_name))
##          return commandMacro((first,second,third))
        
############################################
#  High level functions
############################################
class commandSpecialPointLabels(command):
    """Return special labels

    Type FUNC('xxx',typename) to get a list of labels with the specified
    typename, where typename can be one of
    'EP', 'MX', 'BP', 'LP', 'UZ', 'HB', 'PD', 'TR', or 'RG'.
    This is equivalent to the command
    load('xxx')(typename)
    which gives a list of the solutions themselves;
    load('xxx')(typename).getLabels()
    returns the list of labels.

    Or use FUNC(s,typename) where s is a parsed solution from sl().
    This is equivalent to the command
    s(typename).getLabels()
    """
    def __init__(self,s,typename):
        self.s = s
        self.typename = typename
    def __call__(self):
        labels = []
        func = commandParseSolutionFile(self.s)
        try:
            s = func().data
        except:
            s = self.s
        for solution in s:
            if solution['Type name'] == self.typename:
                labels.append(solution['Label'])
        return valueStringAndData("", labels)

############################################
#  Return values
############################################

class valueStringAndData:
    def __init__(self,text,data):
        self.text = text
        self.data = data
    def __str__(self):
        return self.text

class valueString:
    def __init__(self,text):
        self.text = text
    def __str__(self):
        return self.text

class valueRun:
    def __init__(self,stream1=None,stream2=None,data=None):
        self.value = ""
        if not (stream1 is None):
            self.value = self.value + stream1.read()
        if not (stream2 is None):
            self.value = self.value + stream2.read()
        if not (data is None):
            self.data = data
    def __str__(self):
         return self.value

class valueSystem:
    def __init__(self):
        self.value = ""
    def __str__(self):
        return self.value
    def interact(self,command,*args):
        if hasattr(os,"spawnv"):
            def syscmd(command,args):
                if not os.path.exists(command):
                    command = command + '.exe'
                return os.spawnv(os.P_WAIT,command,
                                 (os.path.basename(command),) + args)
        else:
            def syscmd(command,args):
                fullcmd = string.join([command]+list(args)," ")
                return os.system(fullcmd)
        fullcmd = string.join([command]+list(args)," ")
        if syscmd(command,args) != 0:
            raise AUTOExceptions.AUTORuntimeError("Error running %s"%fullcmd)
        self.value = self.value + "Finished running: " + fullcmd + "\n"
    def info(self,text):
        self.value = self.value + text

############################################
#  Testing stuff
############################################
def print_test(text):
    print text

def test():
    import runAUTO
    import sys

    runner = runAUTO.runAUTO()
    
    clean      = commandRunDemo("wav","clean",runner)
    first      = commandRunDemo("wav","first",runner)
    second     = commandRunDemo("wav","second",runner)
    tmacro     = commandMacro((clean,first,first))
    printer    = commandPrintFunc(print_test,"Hello World")
    quiet      = commandRunnerConfig(runner,verbose="no")
    verbose    = commandRunnerConfig(runner,verbose="yes")

    verbose()
    clean()
    first()
    tmacro()
    quiet()
    print second()
    printer()

if __name__ == "__main__":
    test()
