#!/usr/bin/env autox
import windowPlotter
import parseB
import parseS
import matplotlib
import code
import re
import sys
import string
import Tkinter

class PyPlautInteractiveConsole(code.InteractiveConsole):
    def __init__(self,locals=None,b=None,s=None):
        code.InteractiveConsole.__init__(self,locals)
        if locals is None:
            return
        
        self.line_split = re.compile(r'^(\s*[,;/]?\s*)'
                                     r'([\?\w\.]+\w*\s*)'
                                     r'(\(?.*$)')
        self.re_exclude_auto = re.compile(r'^[<>,&^\|\*/\+-]'
                                          '|^is |^not |^in |^and |^or ')
        self.xaxis = 1
        self.yaxis = 2
        self.expert = 0
        self.dset = 0
        self.ict = 0
        self.icl = 0
        self.title = ""

        root=Tkinter.Tk()
        root.withdraw()
        self.handle = windowPlotter.WindowPlotter2D(root,
            {},grapher_bifurcation_diagram=b,
            grapher_solution=s,
            grapher_width=600,grapher_height=480,
            grapher_use_symbols=0,
            grapher_bifurcation_symbol="square",
            grapher_limit_point_symbol=None,
            grapher_hopf_symbol="fillsquare",
            grapher_period_doubling_symbol="doubletriangle",
            grapher_torus_symbol="filldiamond",
            grapher_user_point_symbol=None,
            grapher_error_symbol=None)
        self.xlabel = self.handle.config("xlabel")[3]
        if self.xlabel is None:
            self.xlabel = ""
        self.ylabel = self.handle.config("ylabel")[3]
        if self.ylabel is None:
            self.ylabel = ""
        self.handle.config(xlabel=self.xlabel,ylabel=self.ylabel)

    def raw_input(self, prompt=None):
        line = ""
        while 1:
            if line == "":
                print " ENTER COMMAND\n"
                line = raw_input(prompt)
            line = self.process_input(line)

    def process_input(self, line):
        lower = 0
        upper = 0
        its = 0
        for c in line:
            if c in string.lowercase:
                lower = 1
            if c in string.uppercase:
                upper = 1
        if upper and not lower:
            line=string.lower(line)
        if line in ["stop","exit","quit","end"]:
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
            self.handle.config(xlabel=self.xlabel,ylabel=self.ylabel)
            its = 1
        if "ax" in opts:
            its = 1
            print ' ENTER HORIZONTAL AND VERTICAL AXIS NUMBER (1,2,...) :'
            try:
                [self.xaxis,self.yaxis] = map(int,string.split(raw_input()))
                self["bifurcation_x"] = [self.xaxis-1]
                self["bifurcation_y"] = [self.yaxis-1]
            except:
                pass
        if "st" in opts:
            self.settitles(1,1,1,1)
            its = 1
        if "d0" in opts:
            self.dset = 1
            self.expert = 0
            self.handle.config(grid = "no", use_labels = 0, use_symbols = 1,
                               stability = 0)
            self.handle.config(xlabel=self.xlabel,ylabel=self.ylabel)
            its = 1
        if "d1" in opts:
            self.dset = 1
            self.expert = 0
            self.handle.config(grid = "no", use_labels = 1, use_symbols = 1,
                               stability = 1)
            self.handle.config(xlabel=self.xlabel,ylabel=self.ylabel)
            its = 1
        if "d2" in opts:
            self.dset = 1
            self.expert = 0
            self.handle.config(grid = "no", use_labels = 0, use_symbols = 1,
                               stability = 1)
            self.handle.config(xlabel=self.xlabel,ylabel=self.ylabel)
            #show stability
            its = 1
        if "d3" in opts:
            self.dset = 1
            self.expert = 0
            self.handle.config(grid = "yes", use_labels = 1, use_symbols = 1,
                               stability = 1)
            self.handle.config(xlabel=self.xlabel,ylabel=self.ylabel)
            its = 1
        if "d4" in opts:
            self.dset = 1
            self.expert = 0
            self.handle.config(grid = "yes", use_labels = 0, use_symbols = 1,
                               stability = 1)
            self.handle.config(xlabel=self.xlabel,ylabel=self.ylabel)
            its = 1
        if "nu" in opts:
            its = 1
            self.dset = 0
            self.expert = 0
            self.icl = 0
            self.ict = 0
            self.xlabel = ""
            self.ylabel = ""
            self.title = ""
            self.handle.config(use_symbols = 0, stability = 0)
            self.handle.config(xlabel=self.xlabel,ylabel=self.ylabel)
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
            print ' ILLEGAL COMMAND - REENTER'
        return ""

    def savefile(self):
        print ' ENTER FILE NAME:'
        flname = string.strip(raw_input())
        if flname == '':
            flname = 'fig.1'
        self.handle.grapher.postscript(flname)
    
    def settitles(self,tit,rtit,axlb,raxlb):
        if raxlb:
            if not self.expert:
                print ' ENTER X AXIS LABEL BETWEEN THE QUOTES'
            print ' "                              "'
            self.xlabel = string.strip(raw_input())
        if axlb:
            self["xlabel"] = self.xlabel
        else:
            self["xlabel"] = ""

        if raxlb:
            if not self.expert:
                print ' ENTER Y AXIS LABEL BETWEEN THE QUOTES'
            print ' "                              "'
            self.ylabel = string.strip(raw_input())
        if axlb:
            self["ylabel"] = self.ylabel
        else:
            self["xlabel"] = ""

        if rtit:
            if not self.expert:
                print ' ENTER TOP TITLE BETWEEN THE QUOTES'
            print ' "                                                            "'
            self.title = string.strip(raw_input())

            #if not self.expert:
            #    print ' ENTER BOTTOM TITLE BETWEEN THE QUOTES'
            #print ' "                                                            "'
        if tit:
            self["top_title"] = self.title
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
                print str
                str='                      '
                i=0
        if len(str) > 22:
            print str

    def getopts(self):
        if self.expert:
            print ' LAB'
        else:
            print ' SOLUTION LABELS ?  ( <Y> OR <N> ) '
        line = string.strip(raw_input())
        self["use_labels"] = string.lower(line[0]) == 'y'
        if self.expert:
            print ' GL'
        else:
            print ' GRID LINES ?  (Y OR N)'
        line = string.strip(raw_input())
        if string.lower(line[0]) == 'y':
            self["grid"] = "yes"
        else:
            self["grid"] = "no"
        tit = 0
        rtit = 0
        axlb = 0
        raxlb = 0
        if self.expert:
            print ' T - C'
            line = string.strip(raw_input())
            if len(line) > 0 and string.lower(line[0]) == 'y':
                tit = 1
                if len(line) > 1 and string.lower(line[1]) == 'y':
                    rtit   = 1
            print ' A - C'
            line = string.strip(raw_input())
            if len(line) > 0 and string.lower(line[0]) == 'y':
                axlb = 1
                if len(line) > 1 and string.lower(line[1]) == 'y':
                    raxlb = 1
        else:
            print ' TITLE ?  (Y OR N)'
            line = string.strip(raw_input())
            if len(line) > 0 and string.lower(line[0]) == 'y':
                tit = 1
                if not self.ict:
                    self.ict = 1
                    rtit = 1
                else:
                    print  ' CHANGE TITLES ?  (Y OR N)'
                    line = string.strip(raw_input())
                    if len(line) > 0 and string.lower(line[0]) == 'y':
                        rtit = 1
            print ' AXES LABELS ?  (Y OR N)'
            line = string.strip(raw_input())
            if len(line) > 0 and string.lower(line[0]) == 'y':
                axlb = 1
                if not self.icl:
                    self.icl = 1
                    raxlb  = 1
                else:
                    print ' CHANGE AXES LABELS ?  (Y OR N)'
                    line = string.strip(raw_input())
                    if len(line) > 0 and string.lower(line[0]) == 'y':
                        raxlb = 1
        self.settitles(tit,rtit,axlb,raxlb)
        
    def enterlabels(self):
        self.listlabels()
        print '\n ENTER LABELS, OR <A> (ALL)\n'
        line = string.lower(raw_input())
        s = self["solution"]
        if line[0] == 'a':
            self["label"] = s.getLabels()
        else:
            line=string.replace(line,',',' ')
            line=string.replace(line,':','-')
            lablist = string.split(string.replace(line,',',' '))
            label = []
            for lab in lablist:
                try:
                    lrange = map(int,string.split(lab,'-'))
                except:
                    print "    INVALID COMMAND,  REENTER"
                    return 0
                if len(lrange) == 2:
                    lrange = range(lrange[0],lrange[1]+1)
                label = label + lrange
            self["label"] = label
        return 1
        
    def plotsol(self):
        self.xaxs = 1
        self.yaxs = 2
        
        self["type"] = "solution"
        if not self.enterlabels():
            return
        s = self["solution"]
        self.ndim=len(s[0]["data"][0]['u'])+1
	while 1:
            its = 0
            print '  NUMBER OF COMPONENTS :%5d'%(self.ndim)
            print ('  ENTER AXES  (DEFAULT %3d%3d),'\
                   ' <D> (DISPLAY), OR <EX> (EXIT)'%(self.xaxs,self.yaxs))
            line = raw_input()
	    lower = 0
            upper = 0
            for c in line:
                if c in string.lowercase:
                    lower = 1
                if c in string.uppercase:
                    upper = 1
            if upper and not lower:
                line=string.lower(line)
            if line in ['end','ex','e']:
                return
            elif line == 'd':
                if not self.dset:
                    self.getopts()
            elif line == '2d':
                self.enterlabels()
            else:
                try:
                    [self.xaxs,self.yaxs] = map(int,string.split(line))
                    if self.xaxs == 1:
                        xaxs = "t"
                    else:
                        xaxs = self.xaxs - 2
                    if self.yaxs == 1:
                        yaxs = "t"
                    else:
                        yaxs = self.yaxs - 2
                    self["solution_x"] = [xaxs]
                    self["solution_y"] = [yaxs]
                except:
                    print "    INVALID COMMAND,  REENTER"
                

    def plotbif(self,opt=None):
        if opt is None:
            while 1:
                if self.expert:
                    print ' LIMITS'
                else:
                    print '  ENTER XMIN,XMAX,YMIN,YMAX'
                line = raw_input()
                try:
                    [xmin,xmax,ymin,ymax] = map(float,string.split(line))
                    break
                except:
                    try:
                        [xmin,xmax,ymin,ymax] = map(float,
                                                    string.split(line,","))
                        break
                    except:
                        pass
            self.handle.config(type = "bifurcation")
            self.handle.config(xlabel = self.xlabel, ylabel = self.ylabel,
                               xticks = None, yticks = None,
                               minx = xmin, maxx = xmax,
                               miny = ymin, maxy = ymax)
        else:
            self.handle.config(type = "bifurcation")
            self.handle.config(xlabel = self.xlabel, ylabel = self.ylabel)
        if not self.dset:
            self.getopts()
        
    def __setitem__(self,key,value):
        self.handle[key] = value

    def __getitem__(self,key):
        return self.handle[key]

    def help(self):
        print """
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

  <D0>    Use solid curves, labels, symbols
  <D1>    As <D0>, showing stability
  <D2>    As <D1>, without labels
  <D3>    As <D1>, with grid lines
  <D4>    AS <D2>, with grid lines

        Individual Options :

  <SY>    Use symbols for special points
  <DP>    Differential Plot (show stability)
  <ST>    Set up titles and axes labels
  <NU>    Normal usage (Reset special options)

  Press RETURN for more or Enter Command ...
  """
        line = raw_input()
        if line != "":
            return line
        print """
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

         --- End of Help ---
  """
        return ""

# Export the functions inside AUTOSimpleFunctions in a dictionary
# This also allows the setting of the log
def exportFunctions(log=None):
    PyPlautFunctionsInstance = PyPlautInteractiveConsole()
    dict = {}
    for name in PyPlautInteractiveConsole.__dict__.keys():
        if name[0] != '_':
            dict[name] = getattr(PyPlautFunctionsInstance, name)
    return dict

# This is the Python syntax for making a script runable    
if __name__ == '__main__':
    try:
        import readline
    except:
        pass

    sys.ps1=""
    if len(sys.argv) < 2:
        b='fort.7'
        s='fort.8'
    else:
        b='b.'+sys.argv[1]
        s='s.'+sys.argv[1]
    b = parseB.parseB(b)
    s = parseS.parseS(s)
    runner = PyPlautInteractiveConsole(exportFunctions(), b, s)
    runner.interact(" ENTER <HELP> IN CASE OF DIFFICULTY\n")
