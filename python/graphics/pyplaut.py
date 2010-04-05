#!/usr/bin/env autox
import bifDiag, parseB, AUTOExceptions
from graphics import windowPlotter
import code
import sys
try:
    import Tkinter
except ImportError:
    import tkinter as Tkinter
import select
try:
    import termios
except ImportError:
    pass
try:
    import readline
except ImportError:
    pass

class PyPlautInteractiveConsole(code.InteractiveConsole):
    def __init__(self,locals=None,b=None,s=None):
        code.InteractiveConsole.__init__(self,locals)
        if locals is None:
            return
        
        root=Tkinter.Tk()
        root.withdraw()
        self.handle = windowPlotter.WindowPlotter2D(root)
        self.handle.protocol("WM_DELETE_WINDOW", self.destroy)
        self.orig_destroy = self.handle.destroy
        self.handle.destroy = self.destroy
        try:
            self.tcattr = termios.tcgetattr(sys.stdin.fileno())
        except:
            pass

        if sys.platform == "cygwin":
            try:
                self.root = root
                readline.set_pre_input_hook(self.handleevents)
            except:
                pass

        #handle default options
        config = self.handle.configure()
        doptions = {"grid":       ["no", "no", "no", "yes", "yes"],
                    "use_labels": [   0,    1,    0,     1,     0],
                    "use_symbols":[   1,    1,    1,     1,     1],
                    "stability":  [   0,    1,    1,     1,     1]}
        dict = {}
        for i in range(5):
            if "d"+str(i) not in config:
                di = {}
                for k,v in doptions.items():
                    di[k] = v[i]
                dict["d"+str(i)] = (di,None)
        if "default_option" not in config:
            dict["default_option"] = ("d1",None)
        self.handle.grapher.addOptions(**dict)

        try:
            b = parseB.parseBR(b)
            bd = bifDiag.bifDiag(b,s,constants=b[0].c)
        except (IOError, AUTOExceptions.AUTORuntimeError):
            try:
                bd = bifDiag.bifDiag(b,s)
            except AUTOExceptions.AUTORuntimeError:
                sys.stderr.write(str(sys.exc_info()[1])+"\n")
                self.orig_destroy()
                sys.exit(1)
        dict = {
            "bifurcation_diagram": bd,
            "solution": bd(),
            "letter_symbols": False }
        for k,v in self[self["default_option"]].items():
            dict[k] = v
        for key in list(dict):
            # check if key was set in .autorc
            config = self.handle.config(key)
            if len(config) > 5 and config[5]:
                del dict[key]
        self.handle.config(**dict)
        self.defaults = {}
        for key in ["xlabel", "ylabel", "zlabel", "top_title"]:
            self.defaults[key] = self.handle.config(key)[3]
        self.plotdefaults = {}
        for key in ["top_title", "grid", "stability", "use_symbols",
                    "use_labels"]:
            self.plotdefaults[key] = self[key]
        self.normal_usage()
        self.b3d = False

    # this polling loop is here so that Cygwin Python does not "hang" the
    # plot window while Python waits for a user input
    def handleevents(self):
        while select.select([sys.stdin],[],[],0.02) == ([], [], []):
            self.root.dooneevent()

    def raw_input(self, prompt=None):
        line = ""
        while 1:
            if line == "":
                if self.b3d:
                    print(" ENTER <B3D> COMMAND\n")
                else:
                    print(" ENTER COMMAND\n")
                line = raw_input(prompt)
            line = self.process_input(line)

    def destroy(self):
        self.orig_destroy()
        try:
            termios.tcsetattr(sys.stdin.fileno(), termios.TCSADRAIN, self.tcattr)
        except:
            pass
        sys.exit()

    def process_input(self, line):
        lower = 0
        upper = 0
        its = 0
        for c in line:
            if c.islower():
                lower = 1
            if c.isupper():
                upper = 1
        if upper and not lower:
            line=line.lower()
        if line in ["stop","exit","quit","end"]:
            if self.b3d:
                self.b3d = False
                self["bifurcation_z"] = None
                return ""
            self.orig_destroy()
            sys.exit()
        elif line in ["scr","screen"]:
            return ""
        elif line in ["ss","set sym"]:
            return ""
        elif line == "sd":
            return ""
        elif line in ["lab","label"]:
            self.listlabels()
            return ""
        elif line in ["c","cl","clr","clear"]:
            self.handle.grapher.clear()
            self.handle.grapher.update()
            return ""
        elif line in ["save","sav","sa","s"]:
            self.savefile()
            return ""
        elif line == "help":
            return self.help()
        elif line in ["sy","sym","symbol"]:
            self["use_symbols"] = 1            
            return ""
        opts = []
        for j in range(6):
            if len(line) < j*2+2:
                break
            opts.append(line[j*2:j*2+2])
        if "sy" in opts:
            self["use_symbols"] = 1            
            its = 1
        if "dp" in opts:
            self["stability"] = 1
            its = 1
        if "ax" in opts:
            its = 1
            try:
                if self.b3d:
                    print(' ENTER THE NEW THREE AXES FOR (X,Y,Z) <B3D>')
                    line = raw_input().replace(","," ")
                    [xaxis,yaxis,zaxis] = map(int,line.split())
                    self["bifurcation_x"] = [xaxis-1]
                    self["bifurcation_y"] = [yaxis-1]
                    self["bifurcation_z"] = [zaxis-1]
                else:
                    print(' ENTER HORIZONTAL AND VERTICAL AXIS NUMBER (1,2,...) :')
                    line = raw_input().replace(","," ")
                    [xaxis,yaxis] = map(int,line.split())
                    self["bifurcation_x"] = [xaxis-1]
                    self["bifurcation_y"] = [yaxis-1]
            except:
                pass
        if "st" in opts:
            self.settitles(1,1,1,1)
            its = 1
        for i in range(5):
            dopt = "d"+str(i)
            if dopt in opts:
                self.dset = 1
                self.expert = 0
                self.handle.config(self[dopt])
                its = 1
        if "nu" in opts:
            its = 1
            self.handle.config(**self.defaults)
            self.handle.grapher.addRCOptions(self.defaults)
            self.normal_usage()
            self.handle.config(use_symbols=0,use_labels=0,
                               grid="no",stability=0)
            self.handle.grapher.plot()
        if "xp" in opts:
            its = 1
            self.expert = 1
        if "bd" in opts:
            index = opts.index("bd")
            if 2*index + 2 < len(line):
                if line[2*index+2] == "0":
                    self.plotbif(0)
                    return ""
            self.plotbif()
            return ""
        elif line[:2] == "2d":
            self.plotsol()
            return ""
        elif line[:2] == "3d":
            self.plotsol(use3d=True)
            return ""
        elif line == "b3d":
            self.b3d = True
            return ""
        elif line == "sda":
            return ""
        elif line == "sdo":
            return ""
        elif line == "sci":
            return ""
        elif line == "ssy":
            return ""
        elif line == "pa":
            return ""
        elif line == "sdd":
            return ""
        elif line == "sls":
            return ""
        elif line == "lda":
            return ""
        elif line == "us":
            return ""
        elif line == "lls":
            return ""
        elif line == "rss":
            return ""
        elif line == "rcs":
            return ""
        elif line == "res":
            return ""
        if its == 0:
            print(' ILLEGAL COMMAND - REENTER')
        return ""

    def normal_usage(self):
        self.dset = 1
        self.expert = 0
        self.icl = 0
        self.ict = 0
        for key in ["xlabel", "ylabel", "zlabel", "top_title"]:
            setattr(self, key, self.handle.config(key)[3])

    def savefile(self):
        print(' ENTER FILE NAME:')
        flname = raw_input().strip()
        if flname == '':
            flname = 'fig.1'
        self.handle.grapher.postscript(flname)
    
    def settitles(self,tit,rtit,axlb,raxlb):
        if self.b3d:
            labels = ["xlabel","ylabel","zlabel"]
        else:
            labels = ["xlabel","ylabel"]
        for key in labels:
            if axlb:
                if raxlb:
                    if not self.expert:
                        print (' ENTER '+ key[0].upper() +
                               ' AXIS LABEL BETWEEN THE QUOTES')
                    print(' "                              "')
                    setattr(self, key, raw_input().strip())
                label = getattr(self, key)
            else:
                label = ""
            if label is not None:
                self[key] = label
            self.handle.grapher.addRCOptions({key:label})
            if label is None:
                self.handle.grapher.plot()

        if rtit:
            if not self.expert:
                print(' ENTER TOP TITLE BETWEEN THE QUOTES')
            print(' "                                                            "')
            self.top_title = raw_input().strip()

            #if not self.expert:
            #    print ' ENTER BOTTOM TITLE BETWEEN THE QUOTES'
            #print ' "                                                            "'
        if tit:
            self["top_title"] = self.top_title
        else:
            self["top_title"] = ""
        self.handle.grapher.update()
        #ax.text(0.5,0,raw_input().strip(),transform=ax.transAxes)
        #self.handle.grapher.canvas.show()

    def listlabels(self):
        s = self["solution"]
        i = 0
        str='\n  THE LABELS ARE :    '
        for label in s.getLabels():
            str=str+"%5d"%(label)
            i=i+1
            if i==10:
                print(str)
                str='                      '
                i=0
        if len(str) > 22:
            print(str)

    def getopts(self):
        if self.expert:
            print(' LAB')
        else:
            print(' SOLUTION LABELS ?  ( <Y> OR <N> ) ')
        line = raw_input().strip()
        self["use_labels"] = line[0].lower() == 'y'
        if self.expert:
            print(' GL')
        else:
            print(' GRID LINES ?  (Y OR N)')
        line = raw_input().strip()
        if line[0].lower() == 'y':
            self["grid"] = "yes"
        else:
            self["grid"] = "no"
        tit = 0
        rtit = 0
        axlb = 0
        raxlb = 0
        if self.expert:
            print(' T - C')
            line = raw_input().strip()
            if len(line) > 0 and line[0].lower() == 'y':
                tit = 1
                if len(line) > 1 and line[1].lower() == 'y':
                    rtit   = 1
            print(' A - C')
            line = raw_input().strip()
            if len(line) > 0 and line[0].lower() == 'y':
                axlb = 1
                if len(line) > 1 and line[1].lower() == 'y':
                    raxlb = 1
        else:
            print(' TITLE ?  (Y OR N)')
            line = raw_input().strip()
            if len(line) > 0 and line[0].lower() == 'y':
                tit = 1
                if not self.ict:
                    self.ict = 1
                    rtit = 1
                else:
                    print(' CHANGE TITLES ?  (Y OR N)')
                    line = raw_input().strip()
                    if len(line) > 0 and line[0].lower() == 'y':
                        rtit = 1
            print(' AXES LABELS ?  (Y OR N)')
            line = raw_input().strip()
            if len(line) > 0 and line[0].lower() == 'y':
                axlb = 1
                if not self.icl:
                    self.icl = 1
                    raxlb  = 1
                else:
                    print(' CHANGE AXES LABELS ?  (Y OR N)')
                    line = raw_input().strip()
                    if len(line) > 0 and line[0].lower() == 'y':
                        raxlb = 1
        self.settitles(tit,rtit,axlb,raxlb)
        
    def enterlabels(self):
        self.listlabels()
        print('\n ENTER LABELS, OR <A> (ALL)\n')
        line = raw_input().lower()
        s = self["solution"]
        if line[0] == 'a':
            self["label"] = s.getLabels()
        else:
            line=line.replace(',',' ')
            line=line.replace(':','-')
            lablist = line.replace(',',' ').split()
            label = []
            for lab in lablist:
                try:
                    lrange = map(int,lab.split('-'))
                except:
                    print("    INVALID COMMAND,  REENTER")
                    return 0
                if len(lrange) == 2:
                    lrange = range(lrange[0],lrange[1]+1)
                label = label + lrange
            self["label"] = label
        return 1
        
    def plotsol(self,use3d=False):
        xaxsn = [1]
        yaxsn = [2]
        zaxsn = [3]
        
        self.handle.config(type = "solution")
        if use3d:
            self["solution_z"] = [1]
        if not self.enterlabels():
            return
        s = self["solution"]
        self.ndim=len(s[0]["data"][0]['u'])+1
        while 1:
            its = 0
            print('  NUMBER OF COMPONENTS :%5d'%(self.ndim))
            if use3d:
                axisstr = "%s %s %s"%(xaxsn,yaxsn,zaxsn)
            else:
                axisstr = "%s %s"%(xaxsn,yaxsn)
            axisstr = axisstr.replace("[","")
            axisstr = axisstr.replace("]","")
            axisstr = axisstr.replace(", ",",")
            print ('  ENTER AXES  (DEFAULT %s),'\
                   ' <D> (DISPLAY), OR <EX> (EXIT)'%axisstr)
            line = raw_input()
            lower = 0
            upper = 0
            for c in line:
                if c.islower():
                    lower = 1
                if c.isupper():
                    upper = 1
            if upper and not lower:
                line=line.lower()
            if line in ['end','ex','e']:
                return
            elif line == 'd':
                if not self.dset:
                    self.getopts()
            elif line in ["save","sav","sa","s"]:
                self.savefile()
            elif (line == '2d' and not use3d) or (line == '3d' and use3d):
                self.enterlabels()
            else:
                try:
                    axislist = map(int,line.replace(","," ").split())
                    if use3d:
                        xaxsn = axislist[:len(axislist)/3]
                        yaxsn = axislist[len(axislist)/3:len(axislist)/3*2]
                        zaxsn = axislist[len(axislist)/3*2:]
                    else:
                        xaxsn = axislist[:len(axislist)/2]
                        yaxsn = axislist[len(axislist)/2:]
                    xaxs = []
                    for x in xaxsn:
                        if x == 1:
                            xaxs.append("t")
                        else:
                            xaxs.append(x-2)
                    yaxs = []
                    for y in yaxsn:
                        if y == 1:
                            yaxs.append("t")
                        else:
                            yaxs.append(y-2)
                    self["solution_x"] = xaxs
                    self["solution_y"] = yaxs
                    self["solution_z"] = None
                    if use3d:
                        zaxs = []
                        for z in zaxsn:
                            if z == 1:
                                zaxs.append("t")
                            else:
                                zaxs.append(z-2)
                        self["solution_z"] = zaxs
                except:
                    print("    INVALID COMMAND,  REENTER")
                

    def plotbif(self,opt=None):
        if opt is None:
            while 1:
                if self.expert:
                    print(' LIMITS')
                elif self.b3d:
                    print('  ENTER XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX')
                else:
                    print('  ENTER XMIN,XMAX,YMIN,YMAX')
                line = raw_input().replace(","," ")
                try:
                    if self.b3d:
                        [xmin,xmax,ymin,ymax,
                         zmin,zmax] = map(float,line.split())
                    else:
                        [xmin,xmax,ymin,ymax] = map(float,line.split())
                    break
                except:
                    pass
            self.handle.config(type = "bifurcation")
            if self.b3d:
                self.handle.config(minx = xmin, maxx = xmax,
                                   miny = ymin, maxy = ymax,
                                   minz = zmin, maxz = zmax)
            else:
                self.handle.config(minx = xmin, maxx = xmax,
                                   miny = ymin, maxy = ymax)
        else:
            self.handle.config(type = "bifurcation")
            self.handle.grapher.plot()
        if not self.dset:
            self.getopts()
        
    def __setitem__(self,key,value):
        self.handle[key] = value

    def __getitem__(self,key):
        return self.handle[key]

    def help(self):
        print("""
         Principal PLAUT Commands :

  <BD0>   Bifurcation diagram with default limits
  <BD>    Bifurcation diagram with user-limits
  <AX>    To select bifurcation diagram axes
  <2D>    2D plot of labeled solutions
  <SAV>   To save the current plot in a file
  <CL>    To clear the graphics window
  <LAB>   List all labeled solutions in Unit 8
  <END>   To End PLAUT

        PLAUT Default Options :

  <D0>    Use solid curves, symbols, but no labels
  <D1>    As <D0>, showing stability and labels
  <D2>    As <D1>, without labels
  <D3>    As <D1>, with grid lines
  <D4>    AS <D2>, with grid lines

        Individual Options :

  <SY>    Use symbols for special points
  <DP>    Differential Plot (show stability)
  <ST>    Set up titles and axes labels
  <NU>    Normal usage (Reset special options)

  Press RETURN for more or Enter Command ...
  """)
        line = raw_input()
        if line != "":
            return line
        print("""
        Additional PLAUT Commands :

  <SCR>   To change the plot size
  <SS>    To define symbols
  <RSS>   Set symbol size
  <XP>    ""Expert"" (Abbreviated prompts)
  <SD>    To change curve type

  <SDA>   Set dash spacing
  <LDA>   Set dash size
  <SDO>   Set dot spacing
  <SCI>   Set circle spacing
  <SSY>   Set curve-symbol spacing
  <RCS>   Set curve-symbol size
  <SDD>   Set dash-dot spacing
  <SLS>   Set long-short dash spacing
  <LLS>   Set long-short dash size
  <PA>    Set plotting accuracy
  <RES>   Reset curves and symbols

  <3D>    3D plot of labeled solutions
  <B3D>   3D bifurcation diagram

         --- End of Help ---
  """)
        return ""

# Export the functions inside AUTOSimpleFunctions in a dictionary
# This also allows the setting of the log
def exportFunctions(log=None):
    PyPlautFunctionsInstance = PyPlautInteractiveConsole()
    dict = {}
    for name in PyPlautInteractiveConsole.__dict__:
        if name[0] != '_':
            dict[name] = getattr(PyPlautFunctionsInstance, name)
    return dict

# This is the Python syntax for making a script runable    
if __name__ == '__main__':
    sys.ps1=""
    if len(sys.argv) < 2:
        b='fort.7'
        s='fort.8'
    else:
        b='b.'+sys.argv[1]
        s='s.'+sys.argv[1]
    runner = PyPlautInteractiveConsole(exportFunctions(), b, s)
    runner.interact(" ENTER <HELP> IN CASE OF DIFFICULTY\n")
