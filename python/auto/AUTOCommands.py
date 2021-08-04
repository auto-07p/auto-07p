#! /usr/bin/env python 
try:
    from cStringIO import StringIO
    import __builtin__
except ImportError: # Python 3
    from io import StringIO
    import builtins as __builtin__
from auto import parseC
from auto import parseB
from auto import parseS
from auto import parseBandS
from auto import parseH
from auto import bifDiag
import os
from auto import AUTOutil
import sys
import glob
import shutil

SIMPLE=0
EXPERT=1


from auto import AUTOExceptions

#############################################
#  commands      
#############################################
def command(f,*args,**kw):
    # This is a class factory that produces a class that can be used
    # to make macros of commands.
    class cmd(object):
        if len(args) == 2:
            type = args[0]
            shortName = args[1]
        alias = kw.get("alias",[])
        fun = staticmethod(f)
        __doc__ = f.__doc__
        def __init__(self,*args,**kw):
            self.args = args
            self.kw = kw
        # The call function must return something that you
        # can call the "print" function on
        def __call__(self):
            return self.fun(*self.args,**self.kw)
        def undo(self):
            raise Exception("Undo undefined for this command")
    return cmd


##############################################
#  Generic Commands
##############################################

def macro(command_list):
    for command in command_list:
        command()
commandMacro = command(macro)

# info messages: override this function or sys.stdout to redirect
def info(s):
    sys.stdout.write(s)

# interact with a .exe file
def interact(command,*args):
    if not os.path.exists(command):
        command = command + '.exe'
    fullcmd = " ".join([command]+list(args))
    if os.spawnv(os.P_WAIT,command, (os.path.basename(command),) + args) != 0:
        raise AUTOExceptions.AUTORuntimeError("Error running %s"%fullcmd)
    info("Finished running: " + fullcmd + "\n")

##############################################
#  Script based commands from $AUTO_DIR/97/cmds
##############################################

def clean():
    """Clean the current directory.

    Type FUNC() to clean the current directory.  This command will
    delete all files of the form fort.*, *.*~, *.o, and *.exe.
    """
    toclean = (glob.glob("fort.*") + glob.glob("*.o") + glob.glob("*.exe")+
               glob.glob("*.*~"))
    # remove duplicates
    files = []
    for f in toclean:
        if f not in files:
            files.append(f)
    for f in files:
        os.remove(f)
    info("Deleting fort.* *.o *.exe *.*~ ... done\n")
commandClean = command(clean,alias=['cl'])


def copydemo(name):
    """Copy a demo into the current directory.

    Type FUNC('xxx') to copy all files from auto/07p/demos/xxx to the
    current user directory.  Here 'xxx' denotes a demo name; e.g.,
    'abc'.  To avoid the overwriting of existing
    files, always run demos in a clean work directory.
    """
    demodir = os.path.join(os.environ["AUTO_DIR"],"demos",name)
    for f in glob.glob(os.path.join(demodir,"*")):
        if os.path.isdir(f):
            subdir = f[len(demodir)+len(os.sep):]
            try:
                os.remove(subdir)
            except OSError:
                pass
            try:
                os.mkdir(subdir)
            except OSError:
                pass
            for f2 in glob.glob(os.path.join(f,"*")):
                try:
                    shutil.copy(f2, subdir)
                except IOError:
                    pass
        try:
            shutil.copy(f, ".")
        except IOError:
            pass
    if (os.path.exists(os.path.join(demodir,"c.%s.1"%name)) and
        not os.path.exists(os.path.join(demodir,"c.%s"%name))):
        shutil.copy("c.%s.1"%name,"c.%s"%name)
    info("Copying demo %s ... done\n"%name)
commandCopyDemo = command(copydemo,SIMPLE,"demo")


def demo(name,runner=None):
    """Copy a demo into the current directory and load it.

    Type FUNC('xxx') to copy all files from auto/07p/demos/xxx to the
    current user directory.  Here 'xxx' denotes a demo name; e.g.,
    'abc'.  To avoid the overwriting of existing
    files, always run demos in a clean work directory.  NOTE: This
    command automatically performs the load command as well.
    """
    runner = withrunner(runner)
    lst = [commandCopyDemo(name)]
    slash = name.rfind("/")
    if slash != -1:
        name = name[slash+1:]
    lst.append(commandRunnerLoadName(name,runner))
    return macro(lst)
commandCopyAndLoadDemo = command(demo,alias=['dm'])


def df():
    """Clear the current directory of fort files.

    Type FUNC() to clean the current directory.  This command will
    delete all files of the form fort.*.
    """
    toclean = glob.glob("fort.*")
    for f in toclean:
        os.remove(f)
    info("Deleting fort.* ... done\n")
commandDeleteFortFiles = command(df,alias=['deletefort'])


def us(name,templates=None):
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
    info("Starting conversion of %s.dat : \n"%name)
    if glob.glob("%s.f90"%name) == []:
        if glob.glob("%s.f"%name) == []:
            equation_file="%s.c"%name
        else:
            equation_file="%s.f"%name
    else:
        equation_file="%s.f90"%name
    cfile = applyTemplate(name,"constants",templates)
    datfile = "%s.dat"%name
    info("(Required files : %s, %s, %s)\n"%(equation_file,cfile,
                                                 datfile))
    import runAUTO
    fconrun = runAUTO.runAUTO(makefile="$AUTO_DIR/cmds/cmds.make fcon")
    fconrun.config(e=name)
    fconrun.runMakefile(name)
    if os.path.exists(cfile):
        shutil.copy(cfile,"fort.2")
    if os.path.exists(datfile):
        shutil.copy(datfile,"fort.3")
    interact("./fcon")
    sfile = applyTemplate("dat","solution",templates)
    if os.path.exists("fort.8"):
        if os.path.exists(sfile):
            os.remove(sfile)
        os.rename("fort.8",sfile)
        info("Conversion done : converted file saved as %s\n"%sfile)
    files = glob.glob("fcon*") + ["fort.2", "fort.3"]
    for f in files:
        try:
            os.remove(f)
        except OSError:
            pass
commandUserData = command(us,alias=['userdata'])


##############################################
#  Commands which use the filename templates
##############################################
def applyTemplate(text,template,templates=None):
    if templates is None:
        templates = {}
        templates["equation"]           = "EQUATION_NAME=%s"
        templates["constants"]          = "c.%s"
        templates["bifurcationDiagram"] = "b.%s"
        templates["solution"]           = "s.%s"
        templates["diagnostics"]        = "d.%s"
        templates["homcont"]           = "h.%s"

    if text is None:
        return None
    elif type(text) in [type(""), type(1), type(1.0)]:
        rval = templates[template]%text
        tmp = glob.glob(rval)
        if len(tmp) > 0:
            rval = ""
            for x in tmp:   
                rval = rval + x + " "
        rval = rval.strip()
        return rval
    else:
        return text


def filenameTemplate(name=None,templates=None):
    name1={}
    name1["constants"] =  applyTemplate(name,"constants",templates)
    name1["bifurcationDiagram"] = applyTemplate(name,"bifurcationDiagram",templates)
    name1["solution"] = applyTemplate(name,"solution",templates)
    name1["diagnostics"] = applyTemplate(name,"diagnostics",templates)
    return name1


def relabel(name1=None,name2=None,templates=None):
    """Relabel data files.

    Type y=FUNC(x) to return the python object x, with the solution
    labels sequentially relabelled starting at 1, as a new object y.

    Type FUNC('xxx') to relabel s.xxx and b.xxx. Backups of the
    original files are saved.

    Type FUNC('xxx','yyy') to relabel the existing data-files s.xxx and b.xxx,
    and save them to s.yyy and b.yyy; d.xxx is copied to d.yyy.
    """

    typen = type(name1)
    if type(name1) == type(""):
        name1 = filenameTemplate(name1,templates)
        name2 = filenameTemplate(name2,templates)
    if typen != type("") and typen != type(None):
        data = name1.relabel()
        info("Relabeling done\n")
        return data
    n1b = name1["bifurcationDiagram"]
    n1s = name1["solution"]
    n1d = name1["diagnostics"]
    if n1b is None and n1s is None and n1d is None:
        n1b, n1s, n1d = "fort.7", "fort.8", "fort.9"
    if name2["bifurcationDiagram"] is None:
        n2b = n1b+'~~'
        n2s = n1s+'~~'
        n2d = n1d+'~~'
    else:
        n2b = name2["bifurcationDiagram"]
        n2s = name2["solution"]
        n2d = name2["diagnostics"]
    from auto.relabel import relabel
    relabel(n1b,n1s,n2b,n2s)
    if os.access(n2b,os.F_OK):
        if name2["bifurcationDiagram"] is None:
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
        info("Relabeling succeeded\n")
    info("Relabeling done\n")
commandRelabel = command(relabel,SIMPLE,"relabel",alias=['rl'])


def merge(name1=None,name2=None,templates=None):
    """Merge branches in data files.

    Type y=FUNC(x) to return the python object x, with its branches
    merged into continuous curves, as a new object y.

    Type FUNC('xxx') to merge branches in s.xxx, b.xxx, and d.xxx. Backups
    of the original files are saved.

    Type FUNC('xxx','yyy') to merge branches in the existing data-files
    s.xxx, b.xxx, and d.xxx and save them to s.yyy, b.yyy, and d.yyy.
    """

    ntype = type(name1)
    if type(name1) == type(""):
        name1 = filenameTemplate(name1,templates)
        name2 = filenameTemplate(name2,templates)
    if ntype != type("") and ntype != type(None):
        data = name1.merge()
        info("Merge done\n")
        return data
    n1b = name1["bifurcationDiagram"]
    n1s = name1["solution"]
    n1d = name1["diagnostics"]
    if n1b is None and n1s is None and n1d is None:
        n1b, n1s, n1d = "fort.7", "fort.8", "fort.9"
    bd = bifDiag.bifDiag(n1b,n1s,n1d)
    bd = bd.merge()
    if name2["bifurcationDiagram"] is None:
        n2b = n1b+'~~'
        n2s = n1s+'~~'
        n2d = n1d+'~~'
    else:
        n2b = name2["bifurcationDiagram"]
        n2s = name2["solution"]
        n2d = name2["diagnostics"]
    bd.writeFilename(n2b,n2s,n2d)
    if os.access(n2b,os.F_OK):
        if name2["bifurcationDiagram"] is None:
            # Save backups
            for [n1,n2] in [[n1b,n2b],[n1s,n2s],[n1d,n2d]]:
                if os.access(n1+'~',os.F_OK):
                    os.remove(n1+'~')
                os.rename(n1,n1+'~')
                os.rename(n2,n1)
        info("Merging succeeded\n")
    info("Merging done\n")
commandMergeBranches = command(merge,SIMPLE,"merge",alias=['mb'])


def subtract(name1,name2,col,branch=1,point=1,templates=None):
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
    ntype = type(name1)
    if type(name1) == type(""):
        name1 = filenameTemplate(name1,templates)
        name2 = filenameTemplate(name2,templates)
    if ntype != type(""):
        sub = name1.subtract(name2[branch-1],col,point)
        info("Subtracting done\n")
        return sub
    else:
        n1b = name1["bifurcationDiagram"]
        bd1 = bifDiag.bifDiag(n1b)
        n2b = name2["bifurcationDiagram"]
        if n1b == n2b:
            bd2 = bd1
        else:
            bd2 = bifDiag.bifDiag(n2b)
        sub = bd1.subtract(bd2[branch-1],col,point)
        shutil.copy(n1b,n1b+'~')
        sub.writeFilename(n1b,'')            
        info("Subtracting done\n")
commandSubtractBranches = command(subtract,SIMPLE,"subtract",alias=['sb'])


def append(name1,name2=None,templates=None):
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
    fort.9, to existing data-files s.xxx, b.xxx, and d.xxx.

    Type FUNC('xxx','yyy') to append existing data-files s.xxx, b.xxx,
    and d.xxx to data-files s.yyy, b.yyy, and d.yyy.
    """
    parsed1=None
    parsed2=None
    if isinstance(name1, bifDiag.bifDiag):
        parsed1=name1
        name1=name2
        name2=None
    if isinstance(name1, bifDiag.bifDiag):
        parsed2=name1
    else:
        name1 = filenameTemplate(name1,templates)
        name2 = filenameTemplate(name2,templates)
    if parsed1 or parsed2:
        n = None
        if not parsed1 or not parsed2:
            nb = name1["bifurcationDiagram"]
            ns = name1["solution"]
            nd = name1["diagnostics"]
    if parsed2: #append to parsed2
        if not parsed1:
            parsed1 = bifDiag.bifDiag(nb,ns,nd)
            info("Appending from %s, %s and %s ... done\n"%(nb,ns,nd))
        parsed2.extend(parsed1)
        return
    if parsed1: #append from parsed1 to file
        parsed1.writeFilename(nb,ns,nd,append=True)
        info("Appending to %s, %s and %s ... done\n"%(nb,ns,nd))
        return
    i = 7
    for s in ["bifurcationDiagram","solution","diagnostics"]:
        n1 = name1[s]
        n2 = name2[s]
        if n2 is None:
            n2 = n1
            n1 = "fort."+str(i)
        i = i+1
        try:
            f1 = open(n1,"rb")
            f2 = open(n2,"ab")
            while 1:
                buf = f1.read(1024*1024)
                if len(buf) == 0:
                    break
                f2.write(buf)
            f1.close()
            f2.close()
            info("Appending %s to %s ... done\n"%(n1,n2))
        except IOError:
            info("Appending %s to %s: %s\n"%(n1,n2,sys.exc_info()[1]))
commandAppend = command(append,SIMPLE,"append",alias=['ap'])


def dirfilenames(name1,name2,name3,name4):
    """Convert arguments to directories and names for copy() and move()"""
    dir1 = ""
    dir2 = ""
    if os.path.isdir(name1):
        dir1 = name1
        name1 = name2
        if name4 is not None:
            dir2 = name3
            name2 = name4
        elif name3 is not None:
            name2 = name3
    elif os.path.isdir(name2):
        dir2 = name2
        if name3 is not None:
            name2 = name3
        else:
            name2 = name1
    return dir1,name1,dir2,name2

def copy(name1,name2,name3=None,name4=None,templates=None):
    """Copy data files.

    Type FUNC(name1,name2) or
    FUNC(name1,name2,name3) or
    FUNC(name1,name2,name3,name4).

    Copy the data-files dir1/c.xxx, dir1/b.xxx, dir1/s.xxx, and dir1/d.xxx
    to dir2/c.yyy, dir2/b.yyy, dir2/s.yyy, and dir2/d.yyy.

    The values of dir1/?.xxx and dir2/?.yyy are as follows, depending on
    whether name1 is a directory or name2 is a directory:

    FUNC(name1,name2)
    no directory names: ./?.name1 and ./?.name2
    name1 is a directory: name1/?.name2 and ./?.name2
    name2 is a directory: ./?.name1 and name2/?.name1

    FUNC(name1,name2,name3)
    name1 is a directory: name1/?.name2 and ./?.name3
    name2 is a directory: ./?.name1 and name2/?.name3

    FUNC(name1,name2,name3,name4)
    name1/?.name2 and name3/?.name4
    """
    
    dir1, name1, dir2, name2 = dirfilenames(name1,name2,name3,name4)
    names1 = filenameTemplate(name1,templates)
    names2 = filenameTemplate(name2,templates)
    done = False
    for s in ["bifurcationDiagram","solution","diagnostics","constants"]:
        n1 = os.path.join(dir1,names1[s])
        n2 = os.path.join(dir2,names2[s])
        if os.path.exists(n1):
            shutil.copy(n1,n2)
            info("Copying %s to %s ... done\n"%(n1,n2))
            done = True
    if not done:
        raise AUTOExceptions.AUTORuntimeError(
            "Copying: no files found for %s and %s"%(
                os.path.join(dir1,"[bsdc]."+name1),
                os.path.join(dir2,"[bsdc]."+name2)))
commandCopyDataFiles = command(copy,alias=['cp'])


def save(name1,name2=None,templates=None):
    """Save data files.

    Type FUNC(x,'xxx') to save bifurcation diagram x
    to the files b.xxx, s.xxx, d.xxx. 
    Existing files with these names will be overwritten.
    If x is a solution, a list of solutions, or does not contain any
    bifurcation diagram or diagnostics data, then only the file s.xxx
    is saved to.

    Type FUNC('xxx') to save the output-files fort.7, fort.8, fort.9,
    to b.xxx, s.xxx, d.xxx.  Existing files with these names will be
    overwritten.
    """
    parsed = None
    if not name2 is None:
        parsed = name1
        name1 = name2
    name1 = filenameTemplate(name1,templates)
    for s in ["bifurcationDiagram","solution","diagnostics"]:
        n1 = name1[s]
        if os.path.exists(n1):
            shutil.copy(n1,n1+'~')

    if parsed:
        n1b = name1["bifurcationDiagram"]
        n1s = name1["solution"]
        n1d = name1["diagnostics"]        
        if (type(parsed) == type([]) and
            isinstance(parsed[0], parseB.AUTOBranch)):
            parsed = bifDiag.bifDiag(parsed)
        if (isinstance(parsed,bifDiag.bifDiag) and
            len(parsed) > 0 and len(parsed[0]) > 0):
            parsed.writeFilename(n1b,n1s,n1d)
            msg = "Saving to %s and %s ... done\n"%(n1b,n1s)
            for d in parsed:
                if hasattr(d,"diagnostics"):
                    msg = "Saving to %s, %s, and %s ... done\n"%(n1b,n1s,n1d)
                    break
        else:
            if (type(parsed) == type([]) and
                isinstance(parsed[0], parseS.AUTOSolution)):
                parsed = parseS.parseS(parsed)
            parsed.writeFilename(n1s)
            msg = "Saving to %s ... done\n"%(n1s)
        info(msg)
        return
        
    i = 7
    for s in ["bifurcationDiagram","solution","diagnostics"]:
        n1 = name1[s]
        forti = "fort." + str(i)
        i = i + 1
        if os.path.exists(forti):
            shutil.copy(forti,n1)
            info("Saving %s as %s ... done\n"%(forti,n1))
commandCopyFortFiles = command(save,SIMPLE,"save",alias=['sv'])


def delete(name,templates=None):
    """Delete data files.

    Type FUNC('xxx') to delete the data-files d.xxx, b.xxx, and s.xxx.
    """
    
    name = filenameTemplate(name,templates)
    n1b = name["bifurcationDiagram"]
    n1s = name["solution"]
    n1d = name["diagnostics"]
    if os.path.exists(n1b):
        os.remove(n1b)
        info("Deleting %s ... done\n"%n1b)
    if os.path.exists(n1s):
        os.remove(n1s)
        info("Deleting %s ... done\n"%n1s)
    if os.path.exists(n1d):
        os.remove(n1d)
        info("Deleting %s ... done\n"%n1d)
commandDeleteDataFiles = command(delete,alias=['dl'])


def deleteLabel(codes=None,name1=None,name2=None,templates=None,
                keepTY=0,keep=0):
    if hasattr(codes,'deleteLabel'):
        origlen=len(codes())
        new = codes.deleteLabel(name1,keepTY=keepTY,keep=keep,copy=1)
        newlen=len(new())
        info("Deleted %d labels, and kept %d.\n"%(origlen-newlen, newlen))
        return new
    name1 = filenameTemplate(name1,templates)
    if name1["solution"] is None:
        changedb='fort.7'
        changeds='fort.8'
    else:
        changedb=name1["bifurcationDiagram"]
        changeds=name1["solution"]
    bs=bifDiag.bifDiag(changedb,changeds)
    origlen=len(bs())
    bs.deleteLabel(codes,keepTY=keepTY,keep=keep)
    newlen=len(bs())
    if name2 is None:
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
    else:
        name2 = filenameTemplate(name2,templates)
        bs.writeFilename(name2["bifurcationDiagram"],name2["solution"])
    info("Deleted %d labels, and kept %d.\n"%(origlen-newlen, newlen))

def dsp(typenames=None,name1=None,name2=None,templates=None):
    """Delete special points.

    Type FUNC(x,list) to delete the special points in list from
    the Python object x, which must be a solution list or a bifurcation diagram.
    Type FUNC(list,'xxx') to delete from the data-files b.xxx, and s.xxx.
    Type FUNC(list,'xxx','yyy') to save to b.yyy and s.yyy instead of ?.xxx.
    Type FUNC(list) to delete from fort.7 and fort.8.
    list is a label number or type name code, or a list of those,
    such as 1, or [2,3], or 'UZ' or ['BP','LP'], or it can be None or
    omitted to mean the special points ['BP','LP','HB','PD','TR','EP','MX']
    Alternatively a boolean user-defined function f that takes a solution
    can be specified for list, such as
        def f(s):
            return s["PAR(9)"]<0
    where all solutions are deleted that satisfy the given condition, or
        def f(s1,s2):
            return abs(s1["L2-NORM"] - s2["L2-NORM"]) < 1e-4
    where all solutions are compared with each other and s2 is deleted if
    the given condition is satisfied, which causes pruning of solutions
    that are close to each other.

    Type information is NOT kept in the bifurcation diagram.
    """
    return deleteLabel(typenames,name1,name2,templates)
commandDeleteSpecialPoints = command(dsp)
        

def ksp(typenames=None,name1=None,name2=None,templates=None):
    """Keep special points.

    Type FUNC(x,list) to only keep the special points in list in
    the Python object x, which must be a solution list or a bifurcation diagram.
    Type FUNC(list,'xxx') to keep them in the data-files b.xxx and s.xxx.
    Type FUNC(list,'xxx','yyy') to save to b.yyy and s.yyy instead of ?.xxx.
    Type FUNC(list) to keep them in fort.7 and fort.8.
    list is a label number or type name code, or a list of those,
    such as 1, or [2,3], or 'UZ' or ['BP','LP'], or it can be None or
    omitted to mean ['BP','LP','HB','PD','TR','EP','MX'], deleting 'UZ' and
    regular points.
    Alternatively a boolean user-defined function f that takes a solution
    can be specified for list, such as
        def f(s):
            return s["PAR(9)"]<0
    where only solutions are kept that satisfy the given condition.

    Type information is NOT kept in the bifurcation diagram.
    """
    return deleteLabel(typenames,name1,name2,templates,keep=1)
commandKeepSpecialPoints = command(ksp)


def dlb(typenames=None,name1=None,name2=None,templates=None):
    """Delete special labels.

    Type FUNC(x,list) to delete the special points in list from
    the Python object x, which must be a solution list or a bifurcation diagram.
    Type FUNC(list,'xxx') to delete from the data-files b.xxx and s.xxx.
    Type FUNC(list,'xxx','yyy') to save to b.yyy and s.yyy instead of ?.xxx.
    Type FUNC(list) to delete from fort.7 and fort.8.
    list is a label number or type name code, or a list of those,
    such as 1, or [2,3], or 'UZ' or ['BP','LP'], or it can be None or
    omitted to mean the special points ['BP','LP','HB','PD','TR','EP','MX']
    Alternatively a boolean user-defined function f that takes a solution
    can be specified for list, such as
        def f(s):
            return s["PAR(9)"] < 0
    where all solutions are deleted that satisfy the given condition, or
        def f(s1,s2):
            return abs(s1["L2-NORM"] - s2["L2-NORM"]) < 1e-4
    where all solutions are compared with each other and s2 is deleted if
    the given condition is satisfied, which causes pruning of solutions
    that are close to each other.

    Type information is kept in the bifurcation diagram for plotting.
    """
    return deleteLabel(typenames,name1,name2,templates,keepTY=1)
commandDeleteLabels = command(dlb)
        

def klb(typenames=None,name1=None,name2=None,templates=None):
    """Keep special labels.

    Type FUNC(x,list) to only keep the special points in list in
    the Python object x, which must be a solution list or a bifurcation diagram.
    Type FUNC(list,'xxx') to keep them in the data-files b.xxx and s.xxx.
    Type FUNC(list,'xxx','yyy') to save to b.yyy and s.yyy instead of ?.xxx.
    Type FUNC(list) to keep them in fort.7 and fort.8.
    list is a label number or type name code, or a list of those,
    such as 1, or [2,3], or 'UZ' or ['BP','LP'], or it can be None or
    omitted to mean ['BP','LP','HB','PD','TR','EP','MX'], deleting 'UZ' and
    regular points.
    Alternatively a boolean user-defined function f that takes a solution
    can be specified for list, such as
        def f(s):
            return s["PAR(9)"]<0
    where only solutions are kept that satisfy the given condition.

    Type information is kept in the bifurcation diagram for plotting.
    """
    return deleteLabel(typenames,name1,name2,templates,keepTY=1,keep=1)
commandKeepLabels = command(klb)


def expandData(cmd,name=None,templates=None):
    name = filenameTemplate(name,templates)
    n1b = name["bifurcationDiagram"]
    n1s = name["solution"]
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
    interact(os.path.expandvars("$AUTO_DIR/bin/%s"%cmd))
    os.rename("fort.38",n1s)
    if os.path.exists("fort.28"):
        os.remove("fort.28")
    if cmd == "double":
        info("Solution doubling done.\n")
    else:
        info("Solution tripling done.\n")

def double(name=None,templates=None):
    """Double a solution.

    Type FUNC() to double the solution in 'fort.7' and 'fort.8'.

    Type FUNC('xxx') to double the solution in b.xxx and s.xxx.
    """
    expandData("double",name,templates)
commandDouble = command(double,alias=['db'])


def move(name1,name2,name3=None,name4=None,templates=None):
    """Move data-files to a new name.

    Type FUNC(name1,name2) or
    FUNC(name1,name2,name3) or
    FUNC(name1,name2,name3,name4)

    Move the data-files dir1/b.xxx, dir1/s.xxx, and dir1/d.xxx,
    to dir2/b.yyy, dir2/s.yyy, and dir2/d.yyy, and copy the constants
    file dir1/c.xxx to dir2/c.yyy.

    The values of dir1/?.xxx and dir2/?.yyy are as follows, depending on
    whether name1 is a directory or name2 is a directory:

    FUNC(name1,name2)
    no directory names: ./?.name1 and ./?.name2
    name1 is a directory: name1/?.name2 and ./?.name2
    name2 is a directory: ./?.name1 and name2/?.name1

    FUNC(name1,name2,name3)
    name1 is a directory: name1/?.name2 and ./?.name3
    name2 is a directory: ./?.name1 and name2/?.name3

    FUNC(name1,name2,name3,name4)
    name1/?.name2 and name3/?.name4
    """
    dir1, name1, dir2, name2 = dirfilenames(name1,name2,name3,name4)
    names1 = filenameTemplate(name1,templates)
    names2 = filenameTemplate(name2,templates)
    done = False
    for s in ["bifurcationDiagram","solution","diagnostics","constants"]:
        n1 = os.path.join(dir1,names1[s])
        n2 = os.path.join(dir2,names2[s])
        if s == "constants":
            try:
                shutil.copy(n1,n2)
                info("Copying %s to %s ... done\n"%(n1,n2))
                done = True
            except IOError:
                pass
            continue
        if os.path.exists(n1):
            if os.path.exists(n2):
                os.remove(n2)
            os.rename(n1,n2)
            info("Renaming %s as %s ... done\n"%(n1,n2))
            done = True
    if not done:
        raise AUTOExceptions.AUTORuntimeError(
            "Renaming: no files found for %s and %s"%(
                os.path.join(dir1,"[bsdc]."+name1),
                os.path.join(dir2,"[bsdc]."+name2)))
commandMoveFiles = command(move,alias=['mv'])


def cn(name,templates=None):
    """Get the current continuation constants.

    Type FUNC('xxx') to get a parsed version of the constants file
    c.xxx.

    This is equivalent to the command
    loadbd('xxx').c
    """
    name = filenameTemplate(name,templates)
    data = parseC.parseC(name["constants"])
    info("Parsed file: %s\n"%name["constants"])
    return data
commandParseConstantsFile = command(cn,alias=['constantsget'])


def hcn(name,templates=None):
    """Get the current HomCont continuation constants.

    Type FUNC('xxx') to get a parsed version of the HomCont file
    h.xxx.
    """
    name = filenameTemplate(name,templates)
    data = parseH.parseH(name["homcont"])
    info("Parsed file: %s\n"%name["homcont"])
    return data
commandParseHomcontFile = command(hcn)

        
def sl(name=None,templates=None):
    """Parse solution file:

    Type FUNC('xxx') to get a parsed version of the solution file
    s.xxx.

    This is equivalent to the command
    loadbd('xxx')()
    """
    name = filenameTemplate(name,templates)
    n1s = name["solution"] or "fort.8"
    try:
        data = parseS.parseS(n1s)
    except IOError:
        raise AUTOExceptions.AUTORuntimeError(sys.exc_info()[1])
    if isinstance(n1s, str):
        info("Parsed file: %s\n"%n1s)
    return data
commandParseSolutionFile = command(sl,alias=['solutionget'])


def dg(name=None,templates=None):
    """Parse a bifurcation diagram.

    Type FUNC('xxx') to get a parsed version of the diagram file b.xxx.

    This is equivalent to the command loadbd('xxx') but without the
    solutions in s.xxx and without the diagnostics in d.xxx.
    """
    name = filenameTemplate(name,templates)
    n1b = name["bifurcationDiagram"]
    if n1b is None:
        n1b = "fort.7"
    try:
        data = parseB.parseB(n1b)
    except IOError:
        raise AUTOExceptions.AUTORuntimeError(sys.exc_info()[1])
    info("Parsed file: %s\n"%n1b)
    return data
commandParseDiagramFile = command(dg,alias=['diagramget'])


def bt(name=None,templates=None):
    """Parse both bifurcation diagram and solution.

    Type FUNC('xxx') to get a parsed version of the diagram file b.xxx
    and solution file s.xxx.

    This is equivalent to the command loadbd('xxx') but without the
    diagnostics in d.xxx.
    """
    name = filenameTemplate(name,templates)
    n1b = name["bifurcationDiagram"]
    n1s = name["solution"]
    if n1b is None:
        n1b = "fort.7"
        n1s = "fort.8"
    data = parseBandS.parseBandS(n1b,n1s)
    output_names = n1b + " and " + n1s
    info("Parsed files: %s\n"%output_names)
    return data
commandParseDiagramAndSolutionFile = command(bt,alias=['diagramandsolutionget'])


def queryDiagnostic(diagnostic,name=None,templates=None):
    name = filenameTemplate(name,templates)
    n1d = name["diagnostics"]
    if n1d is None:
        n1d = "fort.9"
    try:
        f = open(n1d)
    except TypeError:
        for branch in n1d:
            if hasattr(branch,"diagnostics"):
                for s in str(branch.diagnostics).splitlines():
                    if diagnostic in s:
                        info(s+"\n")
        info("\n")
        return
    for s in f:
        if diagnostic in s:
            info(s)
    f.close()
    info("\n")
commandQueryDiagnostic = command(queryDiagnostic,alias=None)


def branchpoint(name=None,templates=None):
    """Print the ``branch-point function''.
    
    Type FUNC(x) to list the value of the ``branch-point function'' 
    in the diagnostics of the bifurcation diagram object x.
    This function vanishes at a branch point.

    Type FUNC() to list the value of the ``branch-point function'' 
    in the output-file fort.9.
    
    Type FUNC('xxx') to list the value of the ``branch-point function''
    in the info file 'd.xxx'.
    """
    queryDiagnostic("BP",name,templates)
commandQueryBranchPoint = command(branchpoint,alias=['bp','br'])

        
def eigenvalue(name=None,templates=None):
    """Print eigenvalues of Jacobian (algebraic case).

    Type FUNC(x) to list the eigenvalues of the Jacobian 
    in the diagnostics of the bifurcation diagram object x.
    (Algebraic problems.)

    Type FUNC() to list the eigenvalues of the Jacobian 
    in fort.9. 

    Type FUNC('xxx') to list the eigenvalues of the Jacobian 
    in the info file 'd.xxx'.
    """
    queryDiagnostic("Eigenvalue",name,templates)
commandQueryEigenvalue = command(eigenvalue,alias=['ev','eg'])
 

def floquet(name=None,templates=None):
    """Print the Floquet multipliers.

    Type FUNC(x) to list the Floquet multipliers
    in the diagnostics of the bifurcation diagram object x.
    (Differential equations.)

    Type FUNC() to list the Floquet multipliers
    in the output-file fort.9. 

    Type FUNC('xxx') to list the Floquet multipliers 
    in the info file 'd.xxx'.
    """
    queryDiagnostic("Mult",name,templates)
commandQueryFloquet = command(floquet,alias=['fl'])


def hopf(name=None,templates=None):
    """Print the value of the ``Hopf function''.

    Type FUNC(x) to list the value of the ``Hopf function'' 
    in the diagnostics of the bifurcation diagram object x.
    This function vanishes at a Hopf bifurcation point.

    Type FUNC() to list the value of the ``Hopf function'' 
    in the output-file fort.9.

    Type FUNC('xxx') to list the value of the ``Hopf function''
    in the info file 'd.xxx'.
    """
    queryDiagnostic("Hopf",name,templates)
commandQueryHopf = command(hopf,alias=['hp','hb'])


def iterations(name=None,templates=None):
    """Print the number of Newton interations.

    Type FUNC(x) to list the number of Newton iterations per
    continuation step in the diagnostics of the bifurcation diagram
    object x.

    Type FUNC() to list the number of Newton iterations per
    continuation step in fort.9. 

    Type FUNC('xxx') to list the number of Newton iterations per
    continuation step in the info file 'd.xxx'.
    """
    queryDiagnostic("Iterations",name,templates)
commandQueryIterations = command(iterations,alias=['it'])


def limitpoint(name=None,templates=None):
    """Print the value of the ``limit point function''.

    Type FUNC(x) to list the value of the ``limit point function'' 
    in the diagnostics of the bifurcation diagram object x.
    This function vanishes at a limit point (fold).

    Type FUNC() to list the value of the ``limit point function'' 
    in the output-file fort.9.

    Type FUNC('xxx') to list the value of the ``limit point function'' 
    in the info file 'd.xxx'.
    """
    queryDiagnostic("Fold",name,templates)
commandQueryLimitpoint = command(limitpoint,alias=['lm','lp'])


def note(name=None,templates=None):
    """Print notes in info file.

    Type FUNC(x) to show any notes 
    in the diagnostics of the bifurcation diagram
    object x.

    Type FUNC() to show any notes 
    in the output-file fort.9.

    Type FUNC('xxx') to show any notes 
    in the info file 'd.xxx'.
    """
    queryDiagnostic("NOTE",name,templates)
commandQueryNote = command(note,alias=['nt'])


def secondaryperiod(name=None,templates=None):
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
    queryDiagnostic("SPB",name,templates)
commandQuerySecondaryPeriod = command(secondaryperiod,alias=['sp','sc'])


def stepsize(name=None,templates=None):
    """Print continuation step sizes.

    Type FUNC(x) to list the continuation step size for each
    continuation step in the diagnostics of the bifurcation diagram
    object x.

    Type FUNC() to list the continuation step size for each
    continuation step in  'fort.9'.

    Type FUNC('xxx') to list the continuation step size for each
    continuation step in the info file 'd.xxx'.
    """
    queryDiagnostic("Step",name,templates)
commandQueryStepsize = command(stepsize,alias=['ss','st'])


def triple(name=None,templates=None):
    """Triple a solution.

    Type FUNC() to triple the solution in 'fort.8'.

    Type FUNC('xxx') to triple the solution in s.xxx.
    """
    return expandData("triple",name,templates)
commandTriple = command(triple,alias=['tr'])

        
############################################
#  System Commands
############################################

def ls(dir=None):
    """List the current directory.
    
    Type 'FUNC' to run the system 'ls' command in the current directory.  This
    command will accept whatever arguments are accepted by the Unix command
    'ls'.
    """
    cmd = "ls"
    if os.name in ["nt", "dos"]:
        path = os.environ["PATH"].split(os.pathsep)
        cmd = "dir" 
        for s in path:
            if os.path.exists(os.path.join(s,"ls.exe")):
                cmd = "ls"
                break
    if dir is not None:
        cmd = "%s %s"%(cmd,dir)
    if sys.stdout is sys.__stdout__:
        sys.stdout.flush()
        os.system(cmd)
    else:
        info(AUTOutil.getstatusoutput(cmd, shell=True)[1]+'\n')
commandLs = command(ls)


def quit():
    """Quit the AUTO CLUI."""
    if isinstance(__builtin__.quit,str):
        sys.exit()
    else:
        __builtin__.quit()
commandQuit = command(quit,alias=['q'])


def shell(cmd):
    """Run a shell command.
        
    Type FUNC('xxx') to run the command 'xxx' in the Unix shell and display
    the results in the AUTO command line user interface.
    """
    sys.stdout.flush()
    os.system(cmd) 
commandShell = command(shell)


def wait():
    """Wait for the user to enter a key.

    Type 'FUNC()' to have the AUTO interface wait
    until the user hits any key (mainly used in scripts).
    """
    print("Hit <return> to continue")
    raw_input()
commandWait = command(wait)
          

def cat(f=None):
    """Print the contents of a file

    Type 'FUNC xxx' to list the contents of the file 'xxx'.
    """
    if f is not None:
        f = open(f,"r")
        for line in f:
            info(line)
        f.close()
    else:
        line = sys.stdin.readline()
        while line != "":
            info(line)
            line = sys.stdin.readline()
commandCat = command(cat)


############################################
#  Commands which use runAUTO
############################################

# This function is overridden in AUTOclui.py, so the AUTOSimpleFunctions
# instance's runner can be used.
def withrunner(runner=None):
    return runner


def cd(dir=None,runner=None):
    """Change directories.
    
    Type 'FUNC xxx' to change to the directory 'xxx'.  This command
    understands both shell variables and home directory expansion.
    """
    runner = withrunner(runner)
    if dir is None or dir == '':
        dir = os.path.expanduser("~")
    try:
        dir = os.path.expanduser(dir)
        dir = os.path.expandvars(dir)
        os.chdir(dir)
    except:
        print("Directory '%s' not found"%(dir,))
    runner.config(dir=os.getcwd())
commandCd = command(cd)


def configure(runner=None,templates=None,data=None,**kw):
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
    the equations file and c.ab.1 as the constants file.

    You can also specify AUTO Constants, e.g., DS=0.05, or IRS=2.
    Special values for DS are '+' (forwards) and '-' (backwards).
    Example: s = FUNC(s,DS='-') changes s.c['DS'] to -s.c['DS'].
    """

    def applyRunnerConfigResolveAbbreviation(**kw):
        abbrev = {}
        for key in ["equation", "constants", "solution", "homcont"]:
            abbrev[key[0]] = key
            abbrev[key]    = key
        for key in list(kw):
            # remove long duplicates
            if (key in abbrev and key != abbrev[key] and
                abbrev[key] in kw):
                del kw[abbrev[key]]
        for key,value in list(kw.items()):
            if key in abbrev:
                # change the abbreviation to the long version
                del kw[key]
                if AUTOutil.isiterable(value):
                    kw[abbrev[key]] = value
                else:
                    if key[0] == 'e':
                        kw['e'] = value
                    kw[abbrev[key]] = applyTemplate(value,abbrev[key],templates)
        return kw

    def applyRunnerConfigResolveFilenames(**kw):
        exception = None
        objectdict = {"constants": parseC.parseC,
                      "homcont": parseH.parseH,
                      "solution": parseS.parseS}
        for key in ["constants", "homcont", "solution"]:
            if key in kw:
                value = kw[key]
            elif data is not None:
                value = applyTemplate(data,key,templates)
            else:
                value = None
            if value is not None and not AUTOutil.isiterable(value):
                try:
                    kw[key] = objectdict[key](value)
                except IOError:
                    if key in kw:
                        # for solution only raise exception later if IRS!=0
                        exception = sys.exc_info()[1]
                        if key != "solution":
                            raise AUTOExceptions.AUTORuntimeError(exception)
                    # ignore error, but erase runner data for load("xxx")
                    kw[key] = None
        if data is not None and "e" not in kw and not AUTOutil.isiterable(data):
            kw["e"] = data
            kw["equation"] = applyTemplate(data,"equation",templates)
        if "e" in kw:
            eq = kw["e"]
            for ext in [".f90",".f",".c"]:
                if os.path.exists(eq+ext):
                    return kw, exception
            raise AUTOExceptions.AUTORuntimeError(
                "No equations file found for: '%s'"%eq)
        return kw, exception

    runner = withrunner(runner)
    if "info" in kw:
        info = kw["info"]
        del kw["info"]
    else:
        info = globals()["info"]
    kw = applyRunnerConfigResolveAbbreviation(**kw)
    kw, exception = applyRunnerConfigResolveFilenames(**kw)
    if data is not None and AUTOutil.isiterable(data):
        if hasattr(data,"load"):
            # for load(object,...)
            if "equation" in kw:
                del kw["equation"]
            solution = data.load(**kw)
            c = solution.c
            kw = {"equation": applyTemplate(c.get("e", ""), "equation",
                                            templates),
                  "solution": solution, "constants": c,
                  "homcont": c.get("homcont")}
        else:
            # for load(array,...)
            kw["solution"] = data
    solution = runner.load(**kw)
    if exception is not None and runner.options["constants"]["IRS"]:
        raise AUTOExceptions.AUTORuntimeError(exception)
    info("Runner configured\n")
    return solution
commandRunnerConfig = command(configure,alias=None)


def load(data=None,runner=None,templates=None,**kw):
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
      column-wise, as [[t0, ..., tn], [x0, ..., xn], [y0, ..., yn], ...],
      or for a point solution as [x, y, z, ...].

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
    the equations file and c.ab.1 as the constants file.

    Type s=FUNC('name') to load all files with base 'name'.
    This does the same thing as running
    s=FUNC(e='name',c='name,h='name',s='name').
 
    You can also specify AUTO Constants, e.g., DS=0.05, or IRS=2.
    Special values for DS are '+' (forwards) and '-' (backwards).
    Example: s = FUNC(s,DS='-') changes s.c['DS'] to -s.c['DS'].
    """
    runner = withrunner(runner)
    return configure(runner,templates,data,**kw)
commandRunnerLoadName = command(load,SIMPLE,"loadname",alias=['ld'])


def loadbd(name=None,templates=None,**kw):
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
    def __applyBsdConfigResolveAbbreviation(**kw):
        abbrev = {}
        for key in ["bifurcationDiagram", "solution", "diagnostics"]:
            abbrev[key[0]] = key
            abbrev[key]    = key
        for key in list(kw):
            # remove long duplicates
            if (key in abbrev and key != abbrev[key] and
                abbrev[key] in kw):
                del kw[abbrev[key]]
        for key,value in list(kw.items()):
            if key in abbrev:
                # change the abbreviation to the long version
                del kw[key]
                if type(value) in [type(""),type(1),type(1.0)]:
                    kw[abbrev[key]] = applyTemplate(value,abbrev[key],templates)
                else:
                    kw[abbrev[key]] = value
        return kw

    if name is not None:
        if AUTOutil.isiterable(name):
            lst = ["bifurcationDiagram"]
        else:
            lst = ["bifurcationDiagram", "solution", "diagnostics"]
        for key in lst:
            if key not in kw:
                kw[key] = name
    if name is None and kw == {}:
        bname, sname, dname = "fort.7", "fort.8", "fort.9"
    else:
        dict = __applyBsdConfigResolveAbbreviation(**kw)
        bname = dict.get("bifurcationDiagram")
        sname = dict.get("solution")
        dname = dict.get("diagnostics")

    data = bifDiag.bifDiag(bname,sname,dname)
    info("Parsed output data\n")
    return data
commandParseOutputFiles = command(loadbd,SIMPLE,"loadbd",alias=['bd'])


def pr(parameter=None,runner=None):
    """Print continuation parameters.

    Type FUNC() to print all the parameters.
    Type FUNC('xxx') to return the parameter 'xxx'.
    These commands are equivalent to the commands
    print s.c
    print s.c['xxx']
    where s is a solution.
    """
    runner = withrunner(runner)
    if parameter is None:
        info(str(runner.options["constants"]))
    else:
        return runner.options["constants"][parameter]
commandRunnerPrintFort2 = command(pr,alias=['printconstant','pc'])


def hpr(parameter=None,runner=None):
    """Print HomCont continuation parameters.

    Type FUNC() to print all the HomCont parameters.
    Type FUNC('xxx') to return the HomCont parameter 'xxx'.
    These commands are equivalent to the commands
    print s.c
    print s.c['xxx']
    where s is a solution.
    """
    runner = withrunner(runner)
    if parameter is None:
        info(str(runner.options["homcont"]))
    else:
        return runner.options["homcont"][parameter]
commandRunnerPrintFort12 = command(hpr)


def ch(entry=None,value=None,runner=None,**kw):
    """Modify continuation constants.

    Type FUNC('xxx',yyy) to change the constant 'xxx' to have
    value yyy.
    This is equivalent to the command
    s=load(s,xxx=yyy)
    where s is a solution.
    """
    runner = withrunner(runner)            
    if entry is not None:
        runner.options["constants"][entry] = value
        info("%s changed to %s\n"%(entry,value))
    else:
        configure(runner,None,info=lambda s:None,**kw)
        info(str(kw)+'\n')
commandRunnerConfigFort2 = command(ch,SIMPLE,"changeConstants",
                                   alias=['changeconstant','cc'])


def hch(entry=None,value=None,runner=None,**kw):
    """Modify HomCont continuation constants.

    Type FUNC('xxx',yyy) to change the HomCont constant 'xxx' to have
    value yyy.
    This is equivalent to the command
    s=load(s,xxx=yyy)
    where s is a solution.
    """
    runner = withrunner(runner)
    if entry is not None:
        runner.options["homcont"][entry] = value
        info("%s changed to %s\n"%(entry,value))
    else:
        configure(runner,None,info=lambda s:None,**kw)
        info(str(kw)+'\n')
commandRunnerConfigFort12 = command(hch,SIMPLE,"changeConstantsHomCont")
    

def run(data=None,sv=None,ap=None,runner=None,templates=None,**kw):
    """Run AUTO.

    Type r=FUNC([data],[options]) to run AUTO from solution data with the given
    AUTO constants or file keyword options.
    
    The results are stored in the bifurcation diagram r which you can
    later print with ``print r'', obtain branches from via r[0], r[1], ...,
    and obtain solutions from via r(3), r(5), r('LP2'), where 3 and 5
    are label numbers, and 'LP2' refers to the second LP label.

    FUNC(data) runs AUTO in the following way for different types of data:

    * A solution: AUTO starts from solution data, with AUTO constants data.c.

    * A bifurcation diagram: AUTO start from the solution specified by
      the AUTO constant IRS, or if IRS is not specified, the last solution
      in data, data()[-1], with AUTO constants data()[-1].c.

    * A string: AUTO uses the solution in the file 's.data' together with the
      constants in the files 'c.data', and 'h.data'. Not all of these
      files need to be present.

    If no solution data is specified, then the global values from the
    'load' command are used instead, where
    options which are not explicitly set retain their previous value.

    Keyword argument options can be AUTO constants, such as DS=0.05,
    or ISW=-1, or specify a constant or solution file. These override
    the constants in s.c, where applicable. See ``load'':
    FUNC(s,options) is equivalent to FUNC(load(s,options))

    Example: given a bifurcation diagram bd, with a branch point
    solution, switch branches and stop at the first Hopf bifurcation:
    hb = FUNC(bd('BP1'),ISW=-1,STOP='HB1')
    
    Special keyword arguments are 'sv' and 'ap'; 'sv' is also an AUTO
    constant:
    FUNC(bd('BP1'),ISW=-1,STOP='HB1',sv='hb',ap='all')
    saves to the files b.hb, s.hb and d.hb, and appends to b.all,
    s.all, and d.all.
    """
    runner = withrunner(runner)
    if sv is not None:
        kw['sv'] = sv
    load(data,runner,templates,info=lambda msg:None,**kw)
    res = runner.run()
    sv = runner.options["constants"].get("sv")
    runner.options["constants"]['sv'] = None
    if sv is not None and sv != '':
        name = filenameTemplate(sv,templates)
        bname = name["bifurcationDiagram"]
        sname = name["solution"]
        dname = name["diagnostics"]
        info("Saving to %s, %s, and %s ... done\n"%(bname,sname,dname))
        if ap is not None:
            append(sv,ap)
    elif ap is not None:
        append(ap)
    return res
commandRun = command(run,SIMPLE,"run",alias=['r','rn'])


def rundemo(demo,equation="all",runner=None):
    runner = withrunner(runner)
    runner.config(equation=equation)
    runner.runDemo(demo)
commandRunDemo = command(rundemo,alias=None)


def runMakefileWithSetup(equation=None,fort2=None,fort3=None,runner=None):
    runner = withrunner(runner)
    if fort2 is not None:
        runner.config(fort2=fort2)
    if fort3 is not None:
        runner.config(fort3=fort3)
    # Before this is called runner needs to have the fort2 and fort3
    # options set.  Otherwise this will raise an exception.
    runner.runMakefileWithSetup(equation)
commandRunMakefileWithSetup = command(runMakefileWithSetup,alias=None)


def runMakefile(equation=None,runner=None):
    runner = withrunner(runner)
    runner.runMakefile(equation)
commandRunMakefile = command(runMakefile,alias=None)


def runExecutableWithSetup(executable=None,fort2=None,fort3=None,runner=None):
    runner = withrunner(runner)
    if fort2 is not None:
        runner.config(fort2=fort2)
    if fort3 is not None:
        runner.config(fort3=fort3)
    # Before this is called runner needs to have the fort2 and fort3
    # options set.  Otherwise this will raise an exception.
    runner.runExecutableWithSetup(executable)
commandRunExecutableWithSetup = command(runExecutableWithSetup,alias=None)


def runExecutable(executable=None,fort2=None,fort3=None,runner=None):
    runner = withrunner(runner)
    runner.runExecutable(executable)
commandRunExecutable = command(runExecutable,alias=None)


def runCommandWithSetup(command=None,fort2=None,fort3=None,runner=None):
    runner = withrunner(runner)
    if fort2 is not None:
        runner.config(fort2=fort2)
    if fort3 is not None:
        runner.config(fort3=fort3)
    # Before this is called runner needs to have the fort2 and fort3
    # options set.  Otherwise this will raise an exception.
    runner.runCommandWithSetup(command)
commandRunCommandWithSetup = command(runCommandWithSetup,alias=None)


def runCommand(command=None,runner=None):
    runner = withRunner(runner)
    runner.runCommand(command)
commandRunCommand = command(runCommand,alias=None)


def plot3(name=None,r3b=False):
    """3D plotting of data.

    Type FUNC(x) to run the graphics program PLAUT04 for the graphical
    inspection of bifurcation diagram or solution data in x.

    Type FUNC('xxx') to run the graphics program PLAUT04 for the graphical
    inspection of the data-files b.xxx and s.xxx.

    Type FUNC() to run the graphics program PLAUT04 for the graphical
    inspection of the output-files 'fort.7' and 'fort.8'.

    Type FUNC(...,r3b=True) to run PLAUT04 in restricted three body
    problem mode.
    """
    cmd = os.path.join(os.path.expandvars("$AUTO_DIR"),"bin")
    cmd = os.path.join(cmd, "plaut04")
    arg = []
    if r3b:
        arg = ["-r3b"]
    if name is not None:
        if type(name) == type(""):
            arg.append(name)
        else:
            d = name
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
    sys.stdout.flush()
    if not os.path.exists(cmd):
        cmd = cmd + '.exe'
    if sys.stdout is sys.__stdout__:
        os.spawnv(os.P_NOWAIT,cmd,[os.path.basename(cmd)] + arg)
    else:
        # when testing, change directories so plaut04 does not keep
        # cwd open on Windows and it can be deleted
        cwd = os.getcwd()
        os.chdir(os.path.dirname(cmd))
        os.spawnv(os.P_NOWAIT,cmd,[os.path.basename(cmd), cwd] + arg)
        # and  wait a little bit
        os.chdir(cwd)
        import time
        time.sleep(2)
        
commandPlotter3D = command(plot3,alias=['p3'])


try:
    try:
        from Tkinter import Tk, TclError
    except ImportError:
        from tkinter import Tk, TclError # Python 3
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

    def plot(name=None,templates=None,**kw):
        """Plotting of data.

        Type FUNC(x) to run the graphics program PyPLAUT for the graphical
        inspection of bifurcation diagram or solution data in x.

        Type FUNC('xxx') to run the graphics program PyPLAUT for the graphical
        inspection of the data-files b.xxx and s.xxx.

        Type FUNC() to run the graphics program for the graphical
        inspection of the output-files 'fort.7' and 'fort.8'.

        Values also present in the file autorc, such as
        color_list="black green red blue orange" can be provided as
        keyword arguments, as well as hide=True which hides the
        on-screen plot.

        The return value, for instance, p for p=plot(x) will be the handle
        for the graphics window.
        It has p.config() and p.savefig() methods that allow you to configure
        and save the plot. When plotting, see help(p.config) and help(p.savefig)
        for details.
        """

        options = kw
        if type(name) == type("") or name is None:
            name = filenameTemplate(name,templates)
            parsed = None
        else:
            parsed = name
        # delay importing plotting modules until we actually plot...
        global plotterimported, windowPlotter
        if not plotterimported:
            from auto.graphics import windowPlotter
            plotterimported = True

        # root has to be here since I am passing options in
        # a dictionary.  Otherwise the default agruements
        # get messed up
        # NOTE: options set here go to the MegaToplevel!, while
        # the return value of this function is the underlying
        # grapher.  So we add 'grapher_' to all options that don't
        # already do
        for k in list(options):
            if k[:8] != 'grapher_':
                v = options[k]
                del options[k]
                options['grapher_'+k] = v

        # Get rid of the initial window
        root = None
        if not options.get('grapher_hide'):
            try:
                root=Tk()
                root.withdraw()
            except TclError:
                sys.stderr.write(str(sys.exc_info()[1])+"\n")
                options['grapher_hide'] = True
                if 'auto.graphics.grapher_mpl' in sys.modules:
                    sys.stderr.write("Disabling on-screen plots, but you can still use savefig\n")
                else:
                    sys.stderr.write("Disabling plots, off-screen plots require Matplotlib\n")
                    return windowPlotter.WindowPlotter2D(root,disabled=True,**options)
        if sys.platform == "cygwin":
            try:
                readline.set_pre_input_hook(handleevents)
                global _root
                _root=root
            except:
                pass
        if parsed:
            nb, ns = None, None
            if isinstance(parsed,bifDiag.bifDiag):
                nb = parsed
                ns = parsed()
            elif isinstance(parsed,parseBandS.parseBandS):
                nb = parsed.diagram.branches
                ns = parsed.solution
            elif isinstance(parsed,parseB.parseB):
                nb = parsed.branches
            elif isinstance(parsed,parseS.parseS):
                ns = parsed
            elif isinstance(parsed,parseB.AUTOBranch):
                nb = parseB.parseBR([parsed])
            elif isinstance(parsed,parseS.AUTOSolution):
                ns = parseS.parseS([parsed])
            if nb:
                options["grapher_bifurcation_diagram"] = nb
            if ns:
                options["grapher_solution"] = ns                    
        else:
            n1b = name["bifurcationDiagram"]
            n1s = name["solution"]
            if n1b is None:
                n1b = "fort.7"
                n1s = "fort.8"
            try:
                n1b = parseB.parseBR(n1b)
                n1b = bifDiag.bifDiag(n1b,n1s,constants=n1b[0].c)
            except IOError:
                n1b = bifDiag.bifDiag(n1b,n1s)
            options["grapher_bifurcation_diagram"] = n1b
            options["grapher_solution"] = n1b()
        handle = windowPlotter.WindowPlotter2D(root,**options)
        if (not options.get('grapher_hide') or
            'auto.graphics.grapher_mpl' not in sys.modules):
            handle.update()
            try:
                def plotterquit():
                    try:
                        handle.destroy()
                    except KeyError:
                        pass
                atexit.register(plotterquit)
            except:
                pass
        info("Created plot\n")
        return handle

except:
    print("\n-------------------------------------------------------------")
    print("Could not import plotting modules, plotting will be disabled.")
    print("This is probably because Tkinter is not enabled in your Python installation.")
    print("-------------------------------------------------------------\n")
    def plot(name=None,templates=None,**kw):
        """2D plotting of data.

        Plotting of data has been disabled in the AUTO-07P CLUI.
        This is probably because the Python interpretor cannot
        load the Tkinter module.
        """
        info("2D plotting has been disabled\n")
commandPlotter = command(plot,SIMPLE,"plot",alias=['pl','p2'])


##################################################
#  CLUI commands
##################################################
def autohelp(command_string=""):
    outputString = ""
    # Read in the aliases.
    _aliases = {}
    parser = AUTOutil.getAUTORC()
    if parser.has_section("AUTO_command_aliases"):
        for option in parser.options("AUTO_command_aliases"):
            cmd = parser.get("AUTO_command_aliases",option)
            if cmd not in _aliases:
                _aliases[cmd] = []
            _aliases[cmd].append(option)
    from auto import AUTOCommands
    if _aliases == {}:
        # Now we copy the commands from the module
        for key in AUTOCommands.__dict__:
            cmd = getattr(AUTOCommands,key)
            # Check to see if it is a command
            if hasattr(cmd,"fun") and cmd.alias is not None:
                _aliases[key] = [cmd.fun.__name__] + cmd.alias
        
    command_list = []

    # Here we get a list of the names of all of the commands in AUTOCommands
    for key in AUTOCommands.__dict__:
        if key in _aliases:
            command_list.append(key)

    return_value = {}
    if not isinstance(command_string, str):
        try:
            outputString += command_string.__doc__+'\n'
        except TypeError:
            pass
        info(outputString)
        return return_value
    if len(command_string) == 0:
        # If we were created with the empty string return a formatted
        # quick reference of all commands as the string and a
        # dictionary of all commands as the data.  The dictionary
        # has an entry for each command which is a dictionary
        # with two entries:
        #   "aliases"  a list of the aliases of the command
        #   "description" a one line description of the command
        command_list.sort()
        outputString += " ALIASES                  DESCRIPTION\n"
        for cmd in command_list:
            return_value[cmd] = {}
            return_value[cmd]["aliases"] = []
            aliases = ""
            for key in _aliases[cmd]:
                aliases = aliases + key + " "
                return_value[cmd]["aliases"].append(key)
            doc = getattr(AUTOCommands,cmd).__doc__
            if doc is not None:
                outputString += " %-25s"%aliases
                doc = doc.splitlines()
                return_value[cmd]["description"] = doc[0]
                outputString += doc[0]
                outputString += "\n"

        from auto import interactiveBindings            
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
            outputString += " %-25s"%aliases
            doc = doc.splitlines()
            return_value[cmd]["description"] = doc[0]
            outputString += doc[0]
            outputString += "\n"

        outputString += "\n"
    else:
        # If we were created with the nonempty string return a formatted
        # reference for the given command as the string and a
        # dictionary containing information about the command as the data.
        # The dictionary has 3 entries:
        #   "name"  the full name of the command
        #   "aliases"  a list of all of the aliases of the command
        #   "description" a long description of the command
        try:
            doc = getattr(AUTOCommands,command_string).__doc__
            return_value["name"] = command_string
        except:
            doc = getattr(AUTOCommands,_aliases[command_string]).__doc__
            return_value["name"] = _aliases[command_string]
        doc = doc.replace("FUNC",command_string)
        return_value["short description"] = doc.splitlines()[0]
        return_value["long description"]  = "\n".join(doc.split("\n")[1:])
        # Get rid of the LaTeX stuff from the string that gets returned, but
        # NOT from the data portion
        doc = doc.replace("\\begin{verbatim}","")
        doc = doc.replace("\\end{verbatim}","")
        doc = doc + "\n"

        if not command_string in command_list:
            # This means help was asked for an alias
            for cmd in _aliases:
                if command_string in _aliases[cmd]:
                    command_string = cmd
                    break
            doc = doc + "Command name: "+command_string+"\n"
        return_value["aliases"] = []
        doc = doc + "Aliases: "
        if command_string in _aliases:
            for key in _aliases[command_string]:
                doc = doc + key + " "
                return_value["aliases"].append(key)
        outputString += doc+"\n"
    info(outputString)
    return return_value
commandHelp = command(autohelp)


# This is just a little wrapper around commandHelp which discards the
# data portion of the return.  This is because, for the
# interactive command line we don't want it to print out.
def man(command_string=""):
    """Get help on the AUTO commands.
    
    Type 'FUNC' to list all commands with a online help.
    Type 'FUNC xxx' to get help for command 'xxx'.
    """
    autohelp(command_string)
commandInteractiveHelp = command(man)


##################################################
#  GUI commands
##################################################
def printFunc(printFnc,text):
    printFnc(text)
    info(text)
commandPrintFunc = command(printFunc)


# FIXME: This is not done!!
def gui(type="simple"):
    """Show AUTOs graphical user interface.

    Type FUNC() to start AUTOs graphical user interface.
    
    NOTE: This command is not implemented yet.
    """
    try:
        from Tkinter import Tk
    except ImportError:
        from tkinter import Tk # Python 3
    from auto.graphics import AUTOgui, Pmw
    # Get rid of the initial window
    root = Tk()
    root.withdraw()
    guic = AUTOgui.AUTOgui(type)
    info("GUI created\n")
    return guic
commandCreateGUI = command(gui)


# Not ready yet
##  def commandRunGeneralGUI(runner):
##      tkSimple
##      first = commandSetupGeneralRun(eq_name,saved_data,parameter_name)
##      second = commandRunnerConfig(runner,makefile="$AUTO_DIR/cmds/cmds.make")
##      third = commandRunMakefile(runner,"EQUATION_NAME=%s"%(eq_name))
##      return commandMacro((first,second,third))
##  commandRunGeneralGUI = command(generalGUI)
        

############################################
#  High level functions
############################################
def splabs(s,typename,templates=None):
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
    labels = []
    for solution in sl(s,templates=templates):
        if solution['Type name'] == typename:
            labels.append(solution['Label'])
    return labels
commandSpecialPointLabels = command(splabs)


############################################
#  Testing stuff
############################################
def test():
    from auto import runAUTO
    import sys
    
    def printfunc(text):
        stdout.write(text+"\n")

    stdout = sys.stdout
    f = StringIO()
    def getinfo(s):
        f.write(s)
    def noinfo(s):
        pass
    global info

    runner = runAUTO.runAUTO(
        makefile="",
        demos_dir=os.path.join(os.environ["AUTO_DIR"],"python"))
    
    clean      = commandRunDemo("wav","clean",runner)
    first      = commandRunDemo("wav","first",runner)
    second     = commandRunDemo("wav","second",runner)
    tmacro     = commandMacro((clean,first,first))
    printer    = commandPrintFunc(printfunc,"Hello World")
    quiet      = commandRunnerConfig(runner,log=f)
    verbose    = commandRunnerConfig(runner,log=None)
    changedir  = commandCd("wav",runner)
    constants  = commandParseConstantsFile("wav")
    changeup   = commandCd("..",runner)

    verbose()
    clean()
    first()
    tmacro()
    quiet()
    second()
    stdout.write(f.getvalue()+"\n")
    printer()
    verbose()
    clean()
    changedir()
    constants()
    changeup()

if __name__ == "__main__":
    test()
