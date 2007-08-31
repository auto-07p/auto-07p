#! /usr/bin/env python 
import cStringIO
import parseC
import parseB
import parseS
import parseBandS
import parseH
import os
import AUTOutil
import sys
import types
import string
import glob
import runAUTO
import ConfigParser
import re

SIMPLE=0
EXPERT=1


from AUTOExceptions import *

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
    delete all files of the form fort.*, *.o, and *.exe.
    """
    
    def __init__(self):
        pass
    def __call__(self):
	rval=valueSystem()
        rval.system("rm -f fort.* *.o *.exe *.*~")
        rval.info("Deleting fort.* *.o *.exe *.*~ ... done\n")
        return rval

class commandCopyDemo(command):
    """Copy a demo into the current directory.

    Type FUNC('xxx') to copy all files from auto/2000/demos/xxx to the
    current user directory.  Here 'xxx' denotes a demo name; e.g.,
    'abc'.  Note that the 'dm' command also copies a Makefile to the
    current user directory. To avoid the overwriting of existing
    files, always run demos in a clean work directory.
    """
    
    type=SIMPLE
    shortName="demo"
    def __init__(self,name1):
        self.demo = name1
    def __call__(self):
	rval=valueSystem()
        rval.system("cp $AUTO_DIR/demos/%s/* ."%self.demo)
        rval.system("cp c.%s.1 c.%s"%(self.demo,self.demo))
        rval.info("Copying demo %s ... done\n"%self.demo)
        return rval

class commandCopyAndLoadDemo(commandMacro):
    """Copy a demo into the current directory and load it.

    Type FUNC('xxx') to copy all files from auto/2000/demos/xxx to the
    current user directory.  Here 'xxx' denotes a demo name; e.g.,
    'abc'.  Note that the 'dm' command also copies a Makefile to the
    current user directory. To avoid the overwriting of existing
    files, always run demos in a clean work directory.  NOTE: This
    command automatically performs the commandRunnerLoadName command
    as well.
    """
    
    def __init__(self,name1,runner=None):
        list=[]
        list.append(commandCopyDemo(name1))
        list.append(commandRunnerLoadName(name1,runner))
        commandMacro.__init__(self,list)

class commandDeleteFortFiles(command):
    """Clear the current directory of fort files.

    Type FUNC() to clean the current directory.  This command will
    delete all files of the form fort.*.
    """
    
    def __init__(self):
        pass
    def __call__(self):
	rval=valueSystem()
        rval.system("rm -f fort.*")
        rval.info("Deleting fort.* ... done\n")
        return rval

class commandUserData(command):
    """Covert user-supplied data files.

    Type FUNC('xxx') to convert a user-supplied data file 'xxx.dat' to
    AUTO format. The converted file is called 's.dat'.  The original
    file is left unchanged.  AUTO automatically sets the period in
    PAR(11).  Other parameter values must be set in 'stpnt'. (When
    necessary, PAR(11) may also be redefined there.)  The
    constants-file file 'c.xxx' must be present, as the AUTO-constants
    'NTST' and 'NCOL' are used to define the new mesh.  For examples
    of using the 'userData' command see demos 'lor' and 'pen' (where
    it has the old name 'fc').
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
        rval.info("(Required files : %s, c.%s, %s.dat)\n"%(equation_file,
                                                           self.data[0],
                                                           self.data[0]))
        rval.system("make -f $AUTO_DIR/src/Makefile EQUATION_NAME=%s fcon"%(self.data[0],))
        rval.system("cp c.%s   fort.2"%(self.data[0],))
        rval.system("cp %s.dat fort.3"%(self.data[0],))
        rval.system("./fcon")
        rval.system("mv fort.8 s.dat")
        rval.system("rm fcon* fort.2 fort.3")
        rval.info("Conversion done : converted file saved as s.dat\n")
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
        else:
            exec "rval = '%s'%%text"%self.templates[template]
            tmp = glob.glob(rval)
            if len(tmp) > 0:
                rval = ""
                for x in tmp:	
                   rval = rval + x + " "
            rval = string.strip(rval)
            return rval



class commandRelabel(commandWithFilenameTemplate):
    """Relabel data files.

    Type FUNC('xxx') to relabel s.xxx, b.xxx, and d.xxx (if you are
    using the default filename templates).  Backups of the
    original files are saved.

    Type FUNC('xxx','yyy') to relabel the existing data-files s.xxx, b.xxx,
    and d.xxx and save then to s.yyy, b.yyy, and d.yyy (if you are using
    the default filename templates). 
    """

    type=SIMPLE
    shortName="relabel"
    def __init__(self,name1,name2=None,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,name2,templates)
    def __call__(self):
	rval=valueSystem()
        if self.name2["bifurcationDiagram"] is None:
            rval.system("cp %s fort.27"%self.name1["bifurcationDiagram"])
            rval.system("cp %s fort.28"%self.name1["solution"])

            # Save backups
            rval.system("cp %s %s~"%(self.name1["bifurcationDiagram"],
                                     self.name1["bifurcationDiagram"]))
            rval.system("cp %s %s~"%(self.name1["solution"],self.name1["solution"]))

            command = os.path.join(os.environ["AUTO_DIR"],"bin/relabel")
            rval.interact("%s"%command)

            if os.access("fort.37",os.F_OK):
                rval.system("mv fort.37 %s"%self.name1["bifurcationDiagram"])
                rval.system("cp -p fort.38 %s"%self.name1["solution"])
                rval.system("rm fort.38")
                rval.info("Relabeling succeeded\n")
            
            rval.info("Relabeling done\n")
        else:
            rval.system("cp %s fort.27"%self.name1["bifurcationDiagram"])
            rval.system("cp %s fort.28"%self.name1["solution"])

            command = os.path.join(os.environ["AUTO_DIR"],"bin/relabel")
            rval.interact("%s"%command)

            if os.access("fort.37",os.F_OK):
                rval.system("mv fort.37 %s"%self.name2["bifurcationDiagram"])
                rval.system("cp -p fort.38 %s"%self.name2["solution"])
                rval.system("rm fort.38")
                rval.system("cp %s  %s"%(self.name1["diagnostics"],self.name2["diagnostics"]))
                rval.info("Relabeling succeeded\n")
            
            rval.info("Relabeling done\n")

        return rval





class commandAppend(commandWithFilenameTemplate):
    """Append data files.

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
        commandWithFilenameTemplate.__init__(self,name1,name2,templates)
    def __call__(self):
	rval=valueSystem()
        if self.name2["bifurcationDiagram"] is None:
            rval.system("cat fort.7 >> %s"%self.name1["bifurcationDiagram"])
            rval.system("cat fort.8 >> %s"%self.name1["solution"])
            rval.system("cat fort.9 >> %s"%self.name1["diagnostics"])
            rval.info("Appending fort.7 to %s ... done\n"%self.name1["bifurcationDiagram"])
            rval.info("Appending fort.8 to %s ... done\n"%self.name1["solution"])
            rval.info("Appending fort.9 to %s ... done\n"%self.name1["diagnostics"])
        else:
            rval.system("cat %s >> %s"%(self.name1["bifurcationDiagram"],
                                        self.name2["bifurcationDiagram"]))
            rval.system("cat %s >> %s"%(self.name1["solution"],
                                        self.name2["solution"]))
            rval.system("cat %s >> %s"%(self.name1["diagnostics"],
                                        self.name2["diagnostics"]))
            rval.info("Appending %s to %s ... done\n"%(self.name1["bifurcationDiagram"],
                                                       self.name2["bifurcationDiagram"]))
            rval.info("Appending %s to %s ... done\n"%(self.name1["solution"],
                                                       self.name2["solution"]))
            rval.info("Appending %s to %s ... done\n"%(self.name1["diagnostics"],
                                                       self.name2["diagnostics"]))
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
        rval.system("cp %s %s"%(self.name1["bifurcationDiagram"],
                                self.name2["bifurcationDiagram"]))
        rval.system("cp %s %s"%(self.name1["solution"],
                                self.name2["solution"]))
        rval.system("cp %s %s"%(self.name1["diagnostics"],
                                self.name2["diagnostics"]))
        rval.system("cp %s %s"%(self.name1["constants"],
                                self.name2["constants"]))
        rval.info("Copying %s to %s ... done\n"%(self.name1["bifurcationDiagram"],
                                                 self.name2["bifurcationDiagram"]))
        rval.info("Copying %s to %s ... done\n"%(self.name1["solution"],
                                                 self.name2["solution"]))
        rval.info("Copying %s to %s ... done\n"%(self.name1["diagnostics"],
                                                 self.name2["diagnostics"]))
        rval.info("Copying %s to %s ... done\n"%(self.name1["constants"],
                                                 self.name2["constants"]))
        return rval
    
class commandCopyFortFiles(commandWithFilenameTemplate):
    """Save data files.

    Type FUNC('xxx') to save the output-files fort.7, fort.8, fort.9,
    to b.xxx, s.xxx, d.xxx (if you are using the default filename
    templates).  Existing files with these names will be overwritten.
    """
    
    type=SIMPLE
    shortName="save"
    def __init__(self,name1,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
	rval=valueSystem()
        if os.path.exists(self.name1["bifurcationDiagram"]):
            rval.system("cp %s %s~"%(self.name1["bifurcationDiagram"],self.name1["bifurcationDiagram"]))
        if os.path.exists(self.name1["solution"]):
            rval.system("cp %s %s~"%(self.name1["solution"],self.name1["solution"]))
        if os.path.exists(self.name1["diagnostics"]):
            rval.system("cp %s %s~"%(self.name1["diagnostics"],self.name1["diagnostics"]))
        rval.system("cat fort.7 > %s"%self.name1["bifurcationDiagram"])
        rval.system("cat fort.8 > %s"%self.name1["solution"])
        rval.system("cat fort.9 > %s"%self.name1["diagnostics"])
        rval.info("Saving fort.7 as %s ... done\n"%self.name1["bifurcationDiagram"])
        rval.info("Saving fort.8 as %s ... done\n"%self.name1["solution"])
        rval.info("Saving fort.9 as %s ... done\n"%self.name1["diagnostics"])
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
        rval.system("rm -f %s"%self.name1["bifurcationDiagram"])
        rval.system("rm -f %s"%self.name1["solution"])
        rval.system("rm -f %s"%self.name1["diagnostics"])
        rval.info("Deleting %s ... done\n"%self.name1["bifurcationDiagram"])
        rval.info("Deleting %s ... done\n"%self.name1["solution"])
        rval.info("Deleting %s ... done\n"%self.name1["diagnostics"])
        return rval

class commandExpandData(commandWithFilenameTemplate):
    def __init__(self,name1=None,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
	rval=valueSystem()
        if self.name1["solution"] is None:
            rval.system("cp fort.8 fort.28")
            rval.system("cp fort.7 fort.7~")
            rval.system("mv fort.8 fort.8~")
            rval.system("$AUTO_DIR/bin/%s"%self.command)
            rval.system("mv fort.38 fort.8")
            rval.system("rm fort.28")
        else:
            rval.system("cp %s fort.28"%self.name1["solution"])
            rval.system("cp %s %s~"%(self.name1["bifurcationDiagram"],self.name1["bifurcationDiagram"]))
            rval.system("mv %s %s~"%(self.name1["solution"],self.name1["solution"]))
            rval.system("$AUTO_DIR/bin/%s"%self.command)
            rval.system("mv fort.38 %s"%self.name1["solution"])
            rval.system("rm fort.28")
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
        rval.system("mv -f %s  %s"%(self.name1["bifurcationDiagram"],
                                    self.name2["bifurcationDiagram"]))
        rval.system("mv -f %s  %s"%(self.name1["solution"],
                                    self.name2["solution"]))
        rval.system("mv -f %s  %s"%(self.name1["diagnostics"],
                                    self.name2["diagnostics"]))
        rval.system("mv -f %s  %s"%(self.name1["constants"],
                                    self.name2["constants"]))
        rval.info("Renaming %s as %s ... done\n"%(self.name1["bifurcationDiagram"],
                                                  self.name2["bifurcationDiagram"]))
        rval.info("Renaming %s as %s ... done\n"%(self.name1["solution"],
                                                  self.name2["solution"]))
        rval.info("Renaming %s as %s ... done\n"%(self.name1["diagnostics"],
                                                  self.name2["diagnostics"]))
        rval.info("Renaming %s as %s ... done\n"%(self.name1["constants"],
                                                  self.name2["constants"]))
        return rval

class commandParseConstantsFile(commandWithFilenameTemplate):
    """Get the current continuation constants.

    Type FUNC('xxx') to get a parsed version of the constants file
    c.xxx (if you are using the default filename templates).
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
    """

    def __init__(self,name1=None,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
        if self.name1["solution"] is None:
            data = parseS.parseS("fort.8")
            output_name = "fort.8"
        else:
            data = parseS.parseS(self.name1["solution"])
            output_name = self.name1["solution"]
        return valueStringAndData("Parsed file: %s\n"%output_name,
                                  data)
        
class commandParseDiagramFile(commandWithFilenameTemplate):
    """Parse a bifurcation diagram.

    Type FUNC('xxx') to get a parsed version of the diagram file b.xxx
    (if you are using the default filename templates).
    """

    def __init__(self,name1=None,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
        if self.name1["bifurcationDiagram"] is None:
            data = parseB.parseB("fort.7")
            output_name = "fort.7"
        else:
            data = parseB.parseB(self.name1["bifurcationDiagram"])
            output_name = self.name1["bifurcationDiagram"] 
        return valueStringAndData("Parsed file: %s\n"%output_name,
                                  data)

class commandParseDiagramAndSolutionFile(commandWithFilenameTemplate):
    """Parse both bifurcation diagram and solution.

    Type FUNC('xxx') to get a parsed version of the diagram file b.xxx
    and solution file s.xxx (if you are using the default filename
    templates).
    """
    
    def __init__(self,name1=None,templates=None):
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
        if self.name1["bifurcationDiagram"] is None:
            data = parseBandS.parseBandS("fort.7","fort.8")
            output_names = "fort.7 and fort.8"
        else:
            data = parseBandS.parseBandS(self.name1["bifurcationDiagram"],
                                         self.name1["solution"])
            output_names = self.name1["bifurcationDiagram"] + " and " + self.name1["solution"]
        return valueStringAndData("Parsed files: %s\n"%output_names,data)


class commandQueryDiagnostic(commandWithFilenameTemplate):
    def __init__(self,diagnostic,name1=None,templates=None):
        self.diagnostic = diagnostic
        commandWithFilenameTemplate.__init__(self,name1,None,templates)
    def __call__(self):
	rval=valueSystem()
        if self.name1["diagnostics"] is None:
            rval.system("grep %s fort.9"%(self.diagnostic,))
        else:
            rval.system("grep %s %s"%(self.diagnostic,self.name1["diagnostics"]))
        rval.info("\n")
        return rval

class commandQueryBranchPoint(commandQueryDiagnostic):
    """Print the ``branch-point function''.
    
    Type FUNC() to list the value of the ``branch-point function'' 
    in the output-file fort.9. This function vanishes at a branch point.
    
    Type FUNC('xxx') to list the value of the ``branch-point function''
    in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"BP",name1,templates)
        
class commandQueryEigenvalue(commandQueryDiagnostic):
    """Print eigenvalues of Jacobian (algebraic case).

    Type FUNC() to list the eigenvalues of the Jacobian 
    in fort.9. 
    (Algebraic problems.)

    Type FUNC('xxx') to list the eigenvalues of the Jacobian 
    in the info file 'd.xxx'.
    """

    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"Eigenvalue",name1,templates)

class commandQueryFloquet(commandQueryDiagnostic):
    """Print the Floquet multipliers.

    Type FUNC() to list the Floquet multipliers
    in the output-file fort.9. 
    (Differential equations.)

    Type FUNC('xxx') to list the Floquet multipliers 
    in the info file 'd.xxx'.
    """

    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"Mult",name1,templates)

class commandQueryHopf(commandQueryDiagnostic):
    """Print the value of the ``Hopf function''.

    Type FUNC() to list the value of the ``Hopf function'' 
    in the output-file fort.9. This function
    vanishes at a Hopf bifurcation point.

    Type FUNC('xxx') to list the value of the ``Hopf function''
    in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"Hopf",name1,templates)

class commandQueryIterations(commandQueryDiagnostic):
    """Print the number of Newton interations.

    Type FUNC() to list the number of Newton iterations per
    continuation step in fort.9. 

    Type FUNC('xxx') to list the number of Newton iterations per
    continuation step in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"Iterations",name1,templates)

class commandQueryLimitpoint(commandQueryDiagnostic):
    """Print the value of the ``limit point function''.

    Type FUNC() to list the value of the ``limit point function'' 
    in the output-file fort.9. This function
    vanishes at a limit point (fold).

    Type FUNC('xxx') to list the value of the ``limit point function'' 
    in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"Fold",name1,templates)

class commandQueryNote(commandQueryDiagnostic):
    """Print notes in info file.

    Type FUNC() to show any notes 
    in the output-file fort.9.

    Type FUNC('xxx') to show any notes 
    in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"NOTE",name1,templates)

class commandQuerySecondaryPeriod(commandQueryDiagnostic):
    """Print value of ``secondary-periodic bif. fcn''.

    Type FUNC()  to list the value of the 
    ``secondary-periodic bifurcation function'' 
    in the output-file 'fort.9. This function
    vanishes at period-doubling and torus bifurcations.

    Type FUNC('xxx') to list the value of the
    ``secondary-periodic bifurcation function''
    in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"SPB",name1,templates)

class commandQueryStepsize(commandQueryDiagnostic):
    """Print continuation step sizes.

    Type FUNC() to list the continuation step size for each
    continuation step in  'fort.9. 

    Type FUNC('xxx') to list the continuation step size for each
    continuation step in the info file 'd.xxx'.
    """
    
    def __init__(self,name1=None,templates=None):
        commandQueryDiagnostic.__init__(self,"Step",name1,templates)

class commandTriple(commandExpandData):
    """Triple a solution.

    Type FUNC() to triple the solution in 'fort.7' and 'fort.8'.

    Type FUNC('xxx') to triple the solution in b.xxx and s.xxx (if you
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
    def __init__(self,dir):
        self.dir = dir
    def __call__(self):
        os.system("ls %s"%(self.dir,)) 
        return valueString("")
        
class commandQuit(command):
    def __init__(self):
        pass
    def __call__(self):
        sys.exit()
        return valueString("")

class commandShell(command):
    """Run a shell command.
        
    Type 'shell xxx' to run the command 'xxx' in the Unix shell and display
    the results in the AUTO command line user interface.
    """
    
    def __init__(self,command):
        self.command = command
    def __call__(self):
        os.system(self.command) 
        return valueString("")

class commandWait(command):
    """Wait for the user to enter a key.

    Type 'FUNC' to have the AUTO interface wait
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
    
    def __init__(self,command):
        self.command = "cat "+command


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
    def __init__(self,dir,runner=None):
        self.dir = dir
        commandWithRunner.__init__(self,runner)
    def __call__(self):
        try:
            self.dir = os.path.expanduser(self.dir)
            self.dir = os.path.expandvars(self.dir)
            os.chdir(self.dir)
        except:
            print "Directory '%s' not found"%(self.dir,)
        self.runner.config(dir=os.getcwd())      
        return valueString("")

class commandRunnerConfig(commandWithFilenameTemplate,commandWithRunner):
    """Load files into the AUTO runner.

    Type FUNC([options]) to modify AUTO runner.
    There are four possible options:
    \\begin{verbatim}
    Long name   Short name    Description
    -------------------------------------------
    equation    e             The equations file
    constants   c             The AUTO constants file
    solution    s             The restart solution file
    homcont     h             The Homcont parameter file
    \\end{verbatim}
    Options which are not explicitly set retain their previous value.
    For example one may type: FUNC(e='ab',c='ab.1') to use 'ab.c' as
    the equations file and c.ab.1 as the constants file (if you are
    using the default filename templates).
    """

    def __init__(self,runner=None,templates=None,cnf={},**kw):
        commandWithFilenameTemplate.__init__(self,None,None,templates)
        commandWithRunner.__init__(self,runner)
        dict = AUTOutil.cnfmerge((cnf,kw))
        self.configDict = dict
    
    def __applyRunnerConfigResolveAbbreviation(self,kw={}):
        abbrev = {}
        abbrev["e"]         = "equation"
        abbrev["equation"]  = "equation"
        abbrev["c"]         = "constants"
        abbrev["constants"] = "constants"
        abbrev["s"]         = "solution"
        abbrev["solution"]  = "solution"
        abbrev["h"]         = "homcont"
        abbrev["homcont"]   = "homcont"
        for key in kw.keys():
            if key in abbrev.keys():
                # change the abbreviation to the long version
                value = kw[key]
                del kw[key]
                if type(value) == types.StringType:
                    kw[abbrev[key]] = self._applyTemplate(value,abbrev[key])
                else:
                    kw[abbrev[key]] = value
        return kw

    def __applyRunnerConfigResolveFilenames(self,kw={}):
        if kw.has_key("constants"):
            if type(kw["constants"]) == types.StringType:
                object = parseC.parseC()
                object.readFilename(kw["constants"])
                kw["constants"] = object
        if kw.has_key("solution"):
            if type(kw["solution"]) == types.StringType:
                object = parseS.parseS()
                try:
                    object.readFilename(kw["solution"])
                except IOError:
                    #sys.stdout.write("Could not open file '%s', defaulting to empty file\n"%kw["solution"])
                    object = None
                kw["solution"] = object
        if kw.has_key("homcont"):
            if type(kw["homcont"]) == types.StringType:
                object = parseH.parseH()
                try:
                    object.readFilename(kw["homcont"])
                except IOError:
                    #sys.stdout.write("Could not open file '%s', defaulting to empty file\n"%kw["homcont"])
                    object = None
                kw["homcont"] = object
        return kw

    def __call__(self):
        dict = self.__applyRunnerConfigResolveAbbreviation(self.configDict)
        dict = self.__applyRunnerConfigResolveFilenames(dict)
        self.runner.config(dict)
        return valueString("Runner configured\n")

class commandRunnerLoadName(commandRunnerConfig):
    """Load files into the AUTO runner.

    Type FUNC([options]) to modify AUTO runner.
    There are four possible options:
    \\begin{verbatim}
    Long name   Short name    Description
    -------------------------------------------
    equation    e             The equations file
    constants   c             The AUTO constants file
    solution    s             The restart solution file
    homcont     h             The Homcont parameter file
    \\end{verbatim}
    Options which are not explicitly set retain their previous value.
    For example one may type: FUNC(e='ab',c='ab.1') to use 'ab.c' as
    the equations file and c.ab.1 as the constants file (if you are
    using the default filename templates).

    Type FUNC('name') load all files with base 'name'.
    This does the same thing as running
    FUNC(e='name',c='name,s='name',h='name').

    You can also specify AUTO Constants, e.g., DS=0.05, or IRS=2.
    Special values for DS are '+' (forwards) and '-' (backwards).
    """
    type="simple"
    shortName="loadName"
    def __init__(self,name=None,runner=None,templates=None,cnf={},**kw):
        if not(name is None):
            kw["equation"]   = name
            kw["constants"]  = name
            kw["solution"]   = name
            kw["homcont"]    = name
        commandRunnerConfig.__init__(self,runner,templates,
                                     AUTOutil.cnfmerge((kw,cnf)))

class commandRunnerPrintFort2(commandWithRunner):
    """Print continuation parameters.

    Type FUNC() to print all the parameters.
    Type FUNC('xxx') to return the parameter 'xxx'.
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
    """Print continuation parameters.

    Type FUNC() to print all the HomCont parameters.
    Type FUNC('xxx') to return the HomCont parameter 'xxx'.
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

class commandRun(commandWithRunner):
    """Run AUTO.

    Type FUNC([options]) to run AUTO with the given options.
    There are four possible basic options:
    \\begin{verbatim}
    Long name   Short name    Description
    -------------------------------------------
    equation    e             The equations file
    constants   c             The AUTO constants file
    solution    s             The restart solution file
    homcont     h             The Homcont parameter file
    \\end{verbatim}
    Options which are not explicitly set retain their previous value.
    For example one may type: FUNC(e='ab',c='ab.1') to use 'ab.c' as
    the equations file and c.ab.1 as the constants file (if you are
    using the default filename templates).

    You can also specify an sv='xxx' option to save to b.xxx, and so on,
    or ap to append, or AUTO Constants, e.g., DS=0.05, or IRS=2.
    Special values for DS are '+' (forwards) and '-' (backwards).

    Type FUNC('name') load all files with base 'name'.
    This does the same thing as running
    FUNC(e='name',c='name,s='name',h='name').

    FUNC('name','save') does the same thing as running
    FUNC(e='name',c='name,s='name',h='name',sv='save').
    """
    type=SIMPLE
    shortName="run"
    def __init__(self,name=None,sv=None,ap=None,runner=None,templates=None,**kw):
        self.name = name
        self.runner = runner
        self.templates = templates
        self.kw = kw
        self.sv = sv
        self.ap = ap

    def __call__(self):
        if not(self.name is None):
            func=commandRunnerLoadName(self.name,self.runner,self.templates)
            func()
        func=commandRunnerLoadName(None,self.runner,self.templates,self.kw)
        func()
        func=commandRunMakefileWithSetup(self.runner)
        ret=func()
        if not(self.sv is None):
            func=commandCopyFortFiles(self.sv)
            rval=func()
            ret.value = ret.value + rval.value
        if not(self.ap is None):
            func=commandAppend(self.ap)
            rval=func()
            ret.value = ret.value + rval.value
        return ret

class commandRunDemo(commandWithRunner):
    def __init__(self,demo,equation="all",runner=None):
        self.demo = demo
        self.equation = equation
        commandWithRunner.__init__(self,runner)
    def __call__(self):
        self.runner.config(equation=self.equation)
        log,err = self.runner.runDemo(self.demo)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err)
        else:
            return valueRun(log,err)

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
        # options set.  Otherwise this will raise an exeception.
        log,err = self.runner.runMakefileWithSetup(self.equation)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err)
        else:
            return valueRun(log,err)

class commandRunMakefile(command):
    def __init__(self,equation=None,runner=None):
        commandWithRunner.__init__(self,runner)
        self.equation = equation
    def __call__(self):
        log,err = self.runner.runMakefile(self.equation)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err)
        else:
            return valueRun(log,err)

class commandRunExecutableWithSetup(command):
    def __init__(self,executabl=None,fort2=None,fort3=None,runner=None):
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
        # options set.  Otherwise this will raise an exeception.
        log,err = self.runner.runExecutableWithSetup(self.executable)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err)
        else:
            return valueRun(log,err)

class commandRunExecutable(command):
    def __init__(self,executable=None,runner=None):
        commandWithRunner.__init__(self,runner)
        self.executable = executable
        self.fort2 = fort2
        self.fort3 = fort3
    def __call__(self):
        log,err = self.runner.runExecutable(self.executable)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err)
        else:
            return valueRun(log,err)

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
        # options set.  Otherwise this will raise an exeception.
        log,err = self.runner.runCommandWithSetup(self.command)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err)
        else:
            return valueRun(log,err)

class commandRunCommand(command):
    def __init__(self,command=None,runner=None):
        commandWithRunner.__init__(self,runner)
        self.command = command
    def __call__(self):
        log,err = self.runner.runCommand(self.command)
        # Only return the log if the runner is not verbose
        # since when the runner is verbose it prints to
        # to stdout anyway
        if self.runner.options["verbose"] == "yes":
            return valueRun(err)
        else:
            return valueRun(log,err)


class commandPlotter3D(command):
    """3D plotting of data.

    Type FUNC('xxx') to run the graphics program PLAUT04 for the graphical
    inspection of the data-files b.xxx and s.xxx (if you are using the
    default filename templates).

    Type FUNC() to run the graphics program PLAUT04 for the graphical
    inspection of the output-files 'fort.7' and 'fort.8'.
    """

    def __init__(self,name1=None):
        self.data = []
        if not name1 is None:
            self.data.append(name1)
    def __call__(self):
        if self.data == []:
            os.system("sh -c $AUTO_DIR/bin/plaut04 &")
        else:
            os.system("sh -c '$AUTO_DIR/bin/plaut04 %s' &"%(self.data[0],))
        return valueString("")


try:
    import windowPlotter
    try:
        import readline
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

        Type FUNC('xxx') to run the graphics program for the graphical
        inspection of the data-files b.xxx and s.xxx (if you are using the
        default filename templates).  The return value will be the handle
        for the graphics window.

        Type FUNC() to run the graphics program for the graphical
        inspection of the output-files 'fort.7' and 'fort.8'.  The return
        value will be the handle for the graphics window.
        """

        type=SIMPLE
        shortName="plot"
        def __init__(self,name=None,templates=None,options={},**kw):
            self.options = AUTOutil.cnfmerge((options,kw))
            commandWithFilenameTemplate.__init__(self,name,None,templates)
        def __call__(self):
            import Tkinter
            # root has to be here since I am passing options in
            # a dictionary.  Otherwise the default agruements
            # get messed up
            # NOTE: options set here go to the MegaToplevel!, while
            # the return value of this function is the underlying
            # grapher.  Accordingly:
            # foo = commandPlotter3D(foo="bar").data
            # is not the same as
            # foo.config(foo="bar")

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
            if self.name1["bifurcationDiagram"] is None:
                self.handle = windowPlotter.WindowPlotter2D(root,self.options,grapher_bifurcation_diagram_filename="fort.7",
                                                            grapher_solution_filename="fort.8",
                                                            grapher_width=520,grapher_height=480)
                self.handle.update()
                return valueStringAndData("Created plotter\n",self.handle.grapher)
            else:
                self.handle = windowPlotter.WindowPlotter2D(root,self.options,grapher_bifurcation_diagram_filename="%s"%self.name1["bifurcationDiagram"],
                                                            grapher_solution_filename="%s"%self.name1["solution"],
                                                            grapher_width=520,grapher_height=480)
                self.handle.update()
                return valueStringAndData("Created plotter\n",self.handle.grapher)

except:
    print
    print "-------------------------------------------------------------"
    print "Could not import plotting modules, plotting will be disabled."
    print "This is probably because Tkinter is not enabled in your Python installation."
    print "-------------------------------------------------------------"
    print
    class commandPlotter(commandWithFilenameTemplate):
        """2D plotting of data.

        Plotting of data has been disabled in the AUTO2000 CLUI.
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
        parser = ConfigParser.ConfigParser()
        parser.add_section("AUTO_command_aliases")
        if(os.path.exists(os.path.expandvars("$AUTO_DIR/.autorc"))):
            parser.read(os.path.expandvars("$AUTO_DIR/.autorc"))
        if(os.path.exists(os.path.expandvars("$HOME/.autorc"))):
            parser.read(os.path.expandvars("$HOME/.autorc"))
        if(os.path.exists("./.autorc")):
            parser.read("./.autorc")

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
            for i in range(len(command_list)):
                return_value[command_list[i]] = {}
                return_value[command_list[i]]["aliases"] = []
                aliases = ""
                for key in self._aliases.keys():
                    if self._aliases[key] == command_list[i]:
                        aliases = aliases + key + " "
                        return_value[command_list[i]]["aliases"].append(key)
                doc = eval("AUTOCommands.%s.__doc__"%command_list[i])
                if not(doc is None):
                    self.__print(" %-25s"%aliases)
                    doc = string.split(doc,"\n")
                    return_value[command_list[i]]["description"] = doc[0]
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
                doc = eval("AUTOCommands.%s.__doc__"%self.command_string)
                return_value["name"] = self.command_string
            except:
                doc = eval("AUTOCommands.%s.__doc__"%self._aliases[self.command_string])
                return_value["name"] = self._aliases[self.command_string]
            doc = re.sub("FUNC",self.command_string,doc)
            return_value["short description"] = string.split(doc,"\n")[0]
            return_value["long description"]  = string.join(string.split(doc,"\n")[1:],"\n")
            # Get rid of the LaTeX stuff from the string that gets returned, but
            # NOT from the data portion
            doc = string.replace(doc,"\\begin{verbatim}","")
            doc = string.replace(doc,"\\end{verbatim}","")
            doc = doc + "\n"

            return_value["aliases"] = []
            # This means help was asked for a true command
            if self.command_string in command_list:
                aliases = "Aliases: "
                for key in self._aliases.keys():
                    if self._aliases[key] == self.command_string:
                        aliases = aliases + key + " "
                        return_value["aliases"].append(key)
                aliases = aliases + "\n"
                self.__print(doc+aliases)
            # Otherwise help was asked for an alias
            else:
                name = "Commmand name: "+self._aliases[self.command_string]+"\n"
                aliases = "Aliases: "
                for key in self._aliases.keys():
                    if self._aliases[key] == self._aliases[self.command_string]:
                        aliases = aliases + key + " "
                        return_value["aliases"].append(key)
                aliases = aliases + "\n"
                self.__print(doc+name+aliases)
        return valueStringAndData(self.__outputString,return_value)

# This is just a little wrapper around commandHelp which discards the
# data portion of the return.  This is because, for the
# interactive command line we don't want it to print out.
class commandInteractiveHelp(commandHelp):
    """Get help on the AUTO commands.
    
    Type 'help' to list all commands with a online help.
    Type 'help xxx' to get help for command 'xxx'.
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
        import AUTOgui
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

    Or use FUNC(s,typename) where s is a parsed solution from sl().
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
    def __init__(self,stream1=None,stream2=None):
        self.value = ""
        if not (stream1 is None):
            self.value = self.value + stream1.read()
        if not (stream2 is None):
            self.value = self.value + stream2.read()
    def __str__(self):
         return self.value

class valueSystem:
    def __init__(self):
        self.value = ""
    def __str__(self):
        return self.value
    def system(self,command):
        import os
        if os.name == "posix":
            import commands
            self.value = self.value + commands.getoutput(command)
        else:
            pipe = os.popen('sh -c \'{ ' + command + '; } 2>&1\'', 'r')
            text = pipe.read()
            if text[-1:] == '\n': text = text[:-1]
            self.value = self.value + text
    def interact(self,command):
        import os
        os.system(command)
        self.value = self.value + "Finished running: " + command + "\n"
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
    
    clean      = commandRunDemo("ab","clean",runner)
    first      = commandRunDemo("ab","first",runner)
    second     = commandRunDemo("ab","second",runner)
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










