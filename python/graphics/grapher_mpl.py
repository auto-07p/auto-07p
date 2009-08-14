#!/usr/bin/env python
import matplotlib
matplotlib.use('TkAgg')

from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg
from matplotlib.figure import Figure
import Points
if not Points.numpyimported:
    Points.importnumpy()
transpose = Points.N.transpose

try:
    import Tkinter
    import tkFileDialog
except ImportError:
    import tkinter as Tkinter # Python 3
    from tkinter import filedialog as tkFileDialog
from graphics import Pmw
import AUTOutil
from graphics import optionHandler
import math
from graphics import grapher

GrapherError="GrapherError"

class FigureCanvasTkAggRedraw(FigureCanvasTkAgg):
    def __init__(self,grapher,parent):
        if parent is None:
            parent = Tkinter.Tk()
        parent.wm_title("PyPLAUT")
        FigureCanvasTkAgg.__init__(self,grapher.ax.get_figure(),master=parent)
        
        tkwidget = self.get_tk_widget()

        toolbar = NavigationToolbar2TkAgg( self, parent )

        self.grapher = grapher

    def redraw(self):
        # recalculate label positions
        self.grapher.plotlabels()
        FigureCanvasTkAgg.draw(self)
        
    def draw(self):
        ax = self.grapher.ax
        d = {}
        [d["minx"],d["maxx"]] = ax.get_xlim()
        [d["miny"],d["maxy"]] = ax.get_ylim()
        for k in d:
            if d[k] != self.grapher.cget(k):
                d.update({"xticks": None, "yticks": None})
                self.grapher._configNoDraw(**d)
                self.redraw()
                return
        FigureCanvasTkAgg.draw(self)

    def show(self):
        fig = self.grapher.ax.get_figure() 
        dpi = fig.get_dpi()
        self.grapher._configNoDraw(
            realwidth=fig.get_figwidth()*dpi,
            realheight=fig.get_figheight()*dpi)
        self.redraw()

class BasicGrapher(grapher.BasicGrapher):
    """Documentation string for Basic Grapher

    A simple graphing widget
    By Randy P. and Bart O."""
    def __init__(self,parent=None,**kw):
        self.ax = Figure(figsize=(4.3,3.0)).gca()
        self.ax2d = None
        self.ax.set_autoscale_on(0)
        self.canvas = FigureCanvasTkAggRedraw(self,parent)
        tk_widget = self.canvas.get_tk_widget()
        self.quit = tk_widget.quit
        self.bind = tk_widget.bind
        self.unbind = tk_widget.unbind
        self.postscript = self.canvas.print_figure
        self.winfo_rootx = tk_widget.winfo_rootx
        self.winfo_rooty = tk_widget.winfo_rooty
        self.redrawlabels = 0

        callback = self.__optionCallback
        grapher.BasicGrapher.__init__(self,parent,callback,**kw)
        optionDefaults={}
        optionDefaults["xticks"] = (None,callback)
        optionDefaults["yticks"] = (None,callback)
        optionDefaults["background"] = ("white",callback)
        optionDefaults["width"] = (1,callback)
        optionDefaults["height"] = (1,callback)
        optionAliases = {}
        optionAliases["bg"] = "background"

        self.addOptions(**optionDefaults)
        self.addAliases(**optionAliases)

        for key in ["grid","decorations","xlabel","ylabel","minx","maxx",
                    "miny","maxy"]:
            self.__optionCallback(key,self.cget(key),[])
        matplotlib.rcParams["axes.edgecolor"]=self.cget("foreground")

    def pack(self,**kw):
        self.canvas.get_tk_widget().pack(kw)

    def update(self):
        self.canvas.get_tk_widget().update()
        FigureCanvasTkAgg.draw(self.canvas)

    def __optionCallback(self,key,value,options):
        if key in ["minx","maxx","miny","maxy","realwidth","realheight"]:
            self.redrawlabels = 1
            if key[:3] in ["min", "max"]:
                func = getattr(self.ax, "set_"+key[3]+"lim")
                func(**{key[3]+key[:3]:value})
            elif key == "realwidth":
                lm = self.cget("left_margin")
                rm = self.cget("right_margin")
                self.ax.get_figure().subplots_adjust(left=lm/value,
                                                     right=1-rm/value)
            elif key == "realheight":
                tm = self.cget("top_margin")
                bm = self.cget("bottom_margin")
                self.ax.get_figure().subplots_adjust(top=1-tm/value,
                                                     bottom=bm/value)
        elif key == "grid":
            if value == "yes":
                self.ax.grid(color = self.cget("foreground"))
            else:
                self.ax.grid(False)
        elif key in ["width", "height"]:
            self.canvas.get_tk_widget()[key] = value
        elif key == "top_title":
            fontsize = self.cget("top_title_fontsize")
            if fontsize is None:
                self.ax.set_title(value)
            else:
                self.ax.set_title(value,fontsize=fontsize)
        elif key == "top_title_fontsize":
            title = self.cget("top_title")
            if title is not None:
                self.ax.set_title(title,fontsize=value)
        elif key in ["background", "bg"]:
            self.ax.set_axis_bgcolor(value)
        elif key in ["foreground", "fg"]:
            matplotlib.rcParams["axes.edgecolor"]=self.cget("foreground")
            self.redrawlabels = 1
            if self.cget("grid") == "yes":
                self.ax.grid(color=value)
        elif key == "color_list":
            color_list = value.split()
            i = 0
            for d in self.data:
                if d["newsect"] is None or d["newsect"]:
                    i = i+1
                d["mpline"].set_color(color_list[i%len(color_list)])
        elif key == "decorations":
            if value:
                self.ax.set_axis_on()
            else:
                self.ax.set_axis_off()
        elif key == "use_symbols":
            self.plotsymbols()
        elif key == "use_labels":
            self.plotlabels()
        elif key == "xlabel":
            if value is None:
                value = ""
            fontsize = self.cget("xlabel_fontsize")
            if fontsize is None:
                self.ax.set_xlabel(value)
            else:
                self.ax.set_xlabel(value,fontsize=fontsize)
        elif key == "ylabel":
            if value is None:
                value = ""
            fontsize = self.cget("ylabel_fontsize")
            if fontsize is None:
                self.ax.set_ylabel(value)
            else:
                self.ax.set_ylabel(value,fontsize=fontsize)
        elif key in ["xticks","yticks"]:
            if value is None:
                if key == "xticks":
                    self.ax.set_xscale('linear')
                else:
                    self.ax.set_yscale('linear')
            else:
                ticks = []
                min = self.cget("min"+key[0])
                max = self.cget("max"+key[0])
                ticks = [min + ((max - min) * i) / (value-1) 
                         for i in range(value)]
                if key == "xticks":
                    self.ax.set_xticks(ticks)
                else:
                    self.ax.set_yticks(ticks)

    def _delAllData(self):
        self._configNoDraw(xticks=None,yticks=None)
        for d in self.data:
            if "mpline" in d:
                self.ax.lines.remove(d["mpline"])
        self.data=[]

    def _delData(self,index):
        if "mpline" in data[index]:
            self.ax.lines.remove(data[index]["mpline"])
        del self.data[index]

    def clear(self):
        self.ax.get_figure().axes = []

    def draw(self):
        if self.redrawlabels:
            self.plotlabels()
        self.ax.get_figure().axes = [self.ax]
        FigureCanvasTkAgg.draw(self.canvas)

    def plot(self):
        color_list = self.cget("color_list").split()
            
        # data
        line_width=self.cget("line_width")
        dashes=self.cget("dashes")
        i=-1
        for d in self.data:
            if d["newsect"] is None or d["newsect"]:
                i = i+1
            curve="curve:%d"%(i,)
            fill=color_list[i%len(color_list)]
            v = d["x"],d["y"]
            if len(v[0]) == 1:
                # If we only have one point we draw a small circle or a pixel
                if self.cget("type") == "solution":
                    marker = 'o'
                else:
                    marker = ','
                kw = {'marker':marker,'color':fill}
                #tags=("data_point:%d"%(0,),curve,"data")
            else:
                stable = d["stable"]
                #tags=(curve,"data")
                kw = {'color':fill,'lw':line_width}
                if stable is not None and not stable:
                    kw.update({'ls':'--','dashes':dashes})
            self.ax.plot(*v,**kw)
            d["mpline"] = self.ax.lines[-1]
        self.ax.get_figure().axes = [self.ax]
            
    def __setitem__(self,key,value):
        self.configure(**{key: value})

    def __getitem__(self,key):
        return self.cget(key)

class LabeledGrapher(BasicGrapher,grapher.LabeledGrapher):
    def __init__(self,parent=None,**kw):
        self.labels=[]
        BasicGrapher.__init__(self,parent,**kw)

    def _delData(self,i):
        del self.labels[i]
        BasicGrapher._delData(self,i)

    def _delAllData(self):
        for l in self.labels:
            for label in l:
                if "mpline" in label:
                    self.ax.lines.remove(label["mpline"])
                if "mptext" in label:
                    self.ax.texts.remove(label["mptext"])
                if "mpsymline" in label:
                    self.ax.lines.remove(label["mpsymline"])
                if "mpsymtext" in label:
                    self.ax.texts.remove(label["mpsymtext"])
        self.labels=[]
        BasicGrapher._delAllData(self)
        # set type for next data
        if self.ax is not self.ax2d:
            if self.ax2d is None:
                self.canvas.toolbar.zoom(self)
                self.ax2d = self.ax
            self.ax = self.ax2d

    def _addData(self,data,newsect=None,stable=None):
        self.labels.append([])
        BasicGrapher._addData(self,data,newsect,stable)

    def plotlabels(self):
        if self.cget("realwidth") == 1 or self.cget("realheight") == 1:
            return
        self.redrawlabels = 0

        for i in range(len(self.labels)):
            for label in self.labels[i]:
                if "mpline" in label:
                    self.ax.lines.remove(label["mpline"])
                    del label["mpline"]
                if "mptext" in label:
                    self.ax.texts.remove(label["mptext"])
                    del label["mptext"]

        if not self.cget("use_labels"):
            return

        if hasattr(self.ax.transData,'transform'):
            trans = self.ax.transData.transform
            def transseq(x,y):
                return trans(transpose([x,y]))
            inv_trans = self.ax.transData.inverted().transform
        else:
            trans = self.ax.transData.xy_tup
            def transseq(x,y):
                return transpose(self.ax.transData.numerix_x_y(x,y))
            inv_trans = self.ax.transData.inverse_xy_tup
        if self.cget("smart_label"):
            mp = self.inarrs()
            sp1 = self.cget("left_margin")
            sp2 = 5 #fontsize
            sp3 = self.cget("bottom_margin")
            sp4 = 5
            for i in range(len(self.data)):
                seq = transseq(self.data[i]["x"],self.data[i]["y"])
                seq[:,0] = (seq[:,0] - sp1) / sp2
                seq[:,1] = (seq[:,1] - sp3) / sp4
                self.map_curve(mp,seq)
                                                      
        for i in range(len(self.labels)):
            for label in self.labels[i]:
                if len(label["text"]) == 0:
                    continue
                [x,y] = label["xy"][:2]
                if (x < self["minx"] or x > self["maxx"] or
                    y < self["miny"] or y > self["maxy"]):
                    continue
                data = trans((x,y))
                if not(data is None):
                    [x,y] = data
                    if self.cget("smart_label"):
                        [xoffd1,yoffd1,xoffd2,yoffd2,
                         xofft,yofft,pos] = self.findsp(x,y,mp)
                    else:
                        [xoffd1,yoffd1,xoffd2,yoffd2,
                         xofft,yofft,pos] = self.dumblabel(i,j,x,y)
                    [ha,va] = self.getpos(pos)
                    [xd1,yd1] = inv_trans((x+xoffd1,y+yoffd1))
                    [xd2,yd2] = inv_trans((x+xoffd2,y+yoffd2))
                    [xt,yt] = inv_trans((x+xofft,y+yofft))
                    self.ax.plot([xd1,xd2],[yd1,yd2],linewidth=0.5,
                                 color=self.cget("foreground"))
                    self.ax.text(xt,yt,label["text"],ha=ha,va=va,
                                 color=self.cget("foreground"))
                    label["mpline"] = self.ax.lines[-1]
                    label["mptext"] = self.ax.texts[-1]

    def getpos(self,pos):
        has = [  "left", "center", "right", "right", "right", "center",
                 "left", "left", "left" ]
        vas = [  "bottom","bottom","bottom","center", "top", "top",
                 "top", "center", "bottom" ]
        ha = has[pos]
        va = vas[pos]
        return [ha,va]

    def plot(self):
        self.plotlabels()
        BasicGrapher.plot(self)
        self.plotsymbols()

    def plotsymbols(self):
        for i in range(len(self.labels)):
            for label in self.labels[i]:
                v = label["xy"]
                l = label["symbol"]
                if l is None:
                    continue
                if "mpsymline" in label:
                    self.ax.lines.remove(label["mpsymline"])
                    del label["mpsymline"]
                if "mpsymtext" in label:
                    self.ax.texts.remove(label["mpsymtext"])
                    del label["mpsymtext"]
                if not self.cget("use_symbols"):
                    continue
                c=self.cget("symbol_color")
                if len(l) == 1:
                    #font=self.cget("symbol_font"),
                    self.ax.text(*(v+[l]),
                                  **{'ha':"center",'va':"center",'color':c})
                    label["mpsymtext"] = self.ax.texts[-1]
                    continue
                v = [[coord] for coord in v]
                markerdict = {"fillcircle" : "o", "circle": "o",
                              "square": "s", "crossquare": "x",
                              "fillsquare": "s", "diamond": "D",
                              "filldiamond": "D", "triangle": "^",
                              "doubletriangle": "^"}
                if l not in markerdict:
                    continue
                kw = {'marker': markerdict[l], 'color': c}
                if l in ["circle", "square", "diamond", "triangle"]:
                    kw['mfc'] = self.ax.get_axis_bgcolor()
                if l in ["diamond","filldiamond","triangle","doubletriangle"]:
                    kw['ms'] = 8
                self.ax.plot(*v,**kw)
                label["mpsymline"] = self.ax.lines[-1]

# FIXME:  No regression tester
class InteractiveGrapher(LabeledGrapher,grapher.InteractiveGrapher):
    def __init__(self,parent=None,**kw):
        LabeledGrapher.__init__(self,parent,**kw)

class GUIGrapher(InteractiveGrapher,grapher.GUIGrapher):
    def __init__(self,parent=None,**kw):
        InteractiveGrapher.__init__(self,parent,**kw)
        #self.bind("<ButtonPress-3>",self.popupMenuWrapper)
        self.menu=Tkinter.Menu()
#        self.menu.add_radiobutton(label="print tag",command=self.printTagBindings)
#        self.menu.add_radiobutton(label="label point",command=self.labelPointBindings)
        #self.menu.add_radiobutton(label="zoom",command=self.zoomBindings)
        #self.menu.invoke('zoom')
        self.menu.add_command(label="Unzoom",command=self.unzoom)
        self.menu.add_command(label="Save",command=self.generatePostscript)
        self.menu.add_command(label="Configure...",command=self.__interactiveConfigureDialog)

    def __interactiveConfigureDialog(self):
        widget=self.canvas.get_tk_widget()
        diag = Pmw.Dialog(widget,buttons=("Ok","Cancel"))
        options = []
        for key in self.configure():
            if self._isInternalOption(key):
                options.append(self.configure(key)[0])

        options.sort()
        self.optionList = Pmw.ScrolledListBox(diag.interior(),
                                              items=options,
                                              dblclickcommand=self.__updateInteractiveConfigureDialog)
        self.optionList.pack(side="left")

        frame = Tkinter.Frame(diag.interior())
        frame.pack(side="right")
        self.optionLabel = Pmw.EntryField(frame,
                                         labelpos="w",
                                         label_text="Option Name",
                                         entry_state=Tkinter.DISABLED)
        self.optionLabel.pack(side="top")

        self.valueLabel = Pmw.EntryField(frame,
                                         labelpos="w",
                                         label_text="Old Value",
                                         entry_state=Tkinter.DISABLED)
        self.valueLabel.pack(side="top")

        self.valueEntry = Pmw.EntryField(frame,
                                         labelpos="w",
                                         label_text="New Value",
                                         command=self.__modifyOption)
        self.valueEntry.pack(side="top")
        
    def generatePostscript(self,filename=None,pscolormode=None):
        if pscolormode is None:
            pscolormode=self.cget("ps_colormode")
        if filename is None:
            filename = tkFileDialog.asksaveasfilename(defaultextension=".eps",title="Save the figure")
        self.update()
        #self.postscript(filename,colormode=pscolormode)
        self.postscript(filename)

if __name__=='__main__':
    grapher.test(GUIGrapher())






