#!/usr/bin/env python
import matplotlib
matplotlib.use('TkAgg')

from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg
from matplotlib.backends.backend_agg import FigureCanvasAgg
from matplotlib.figure import Figure
from matplotlib.lines import Line2D
from matplotlib.axes import Axes
from matplotlib.ticker import AutoLocator, FixedLocator
import Points
if not Points.numpyimported:
    Points.importnumpy()
transpose = Points.N.transpose

try: # mpl >= 0.99
    from mpl_toolkits.mplot3d import Axes3D
    from mpl_toolkits.mplot3d.proj3d import proj_transform, inv_transform
except ImportError:
    try: # mpl < 0.98
        from matplotlib.axes3d import Axes3D, Axes3DI
        from matplotlib.proj3d import proj_transform, inv_transform
        # mpl 0.87.7 bug workaround and compat
        def autoscale_view(self, scalex=True, scaley=True):
            self.set_top_view()
        Axes3DI.autoscale_view = autoscale_view
        Axes3DI.set_xlim3d = Axes3DI.set_w_xlim
        Axes3DI.set_ylim3d = Axes3DI.set_w_ylim
        Axes3DI.set_zlim3d = Axes3DI.set_w_zlim
        Axes3DI.get_xlim3d = Axes3DI.get_w_xlim
        Axes3DI.get_ylim3d = Axes3DI.get_w_ylim
        Axes3DI.get_zlim3d = Axes3DI.get_w_zlim
    except (ImportError, NotImplementedError, NameError):
        Axes3D = None

try:
    import Tkinter
    import tkFileDialog
except ImportError:
    import tkinter as Tkinter # Python 3
    from tkinter import filedialog as tkFileDialog
from graphics import Pmw
import AUTOutil
import math
import string
from graphics import grapher

GrapherError="GrapherError"

class FigureCanvasTkAggRedraw(FigureCanvasTkAgg):
    def __init__(self,grapher,parent):
        if parent is None:
            parent = Tkinter.Tk()
        parent.wm_title("PyPLAUT")
        FigureCanvasTkAgg.__init__(self,grapher.ax.get_figure(),master=parent)
        
        grapher.toolbar = NavigationToolbar2TkAgg( self, parent )
        grapher.toolbar.zoom()

        self.grapher = grapher

    def redraw(self):
        # recalculate label positions
        self.grapher.plotlabels()
        FigureCanvasTkAgg.draw(self)
        
    def draw(self):
        ax = self.grapher.ax
        d = {}
        if ax is self.grapher.ax3d:
            [d["minx"],d["maxx"]] = ax.get_xlim3d()
            [d["miny"],d["maxy"]] = ax.get_ylim3d()
            [d["minz"],d["maxz"]] = ax.get_zlim3d()
            d["azimuth"] = ax.azim
            d["elevation"] = ax.elev
            d["cur_lims"] = Axes.get_xlim(ax), Axes.get_ylim(ax)
        else:
            [d["minx"],d["maxx"]] = ax.get_xlim()
            [d["miny"],d["maxy"]] = ax.get_ylim()
        for k in list(d):
            # don't adjust any unchanged settings
            if k == "cur_lims":
                if map(list, d[k]) == map(list, self.grapher._cur_lims):
                    del d[k]
            elif d[k] == self.grapher.cget(k):
                del d[k]
        if d != {}:
            if "cur_lims" in d:
                del d["cur_lims"]
            if d != {}:
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
        self.ax2d = self.ax
        self.ax3d = None
        if kw.get("hide"):
            self.canvas = FigureCanvasAgg(self.ax.get_figure())
        else:
            self.canvas = FigureCanvasTkAggRedraw(self,parent)
            tk_widget = self.canvas.get_tk_widget()
            self.quit = tk_widget.quit
            self.bind = tk_widget.bind
            self.unbind = tk_widget.unbind
            self.winfo_rootx = tk_widget.winfo_rootx
            self.winfo_rooty = tk_widget.winfo_rooty
        self.ax.set_autoscale_on(0)
        self.postscript = self.canvas.print_figure
        self.savefig = self.ax.get_figure().savefig

        self.redrawlabels = 0

        callback = self.__optionCallback
        grapher.BasicGrapher.__init__(self,parent,callback,**kw)
        optionDefaults={}
        optionDefaults["xticks"] = (None,callback)
        optionDefaults["yticks"] = (None,callback)
        optionDefaults["zticks"] = (None,callback)
        optionDefaults["background"] = ("white",callback)
        optionAliases = {}
        optionAliases["bg"] = "background"

        self.addOptions(**optionDefaults)
        self.addAliases(**optionAliases)

        for key in ["grid","decorations","xlabel","ylabel","zlabel",
                    "xlabel_fontsize", "ylabel_fontsize", "zlabel_fontsize",
                    "minx","maxx","miny","maxy","minz","maxz", "width",
                    "height", "left_margin", "right_margin", "top_margin",
                    "bottom_margin"]:
            self.__optionCallback(key,self.cget(key),[])
        matplotlib.rcParams["axes.edgecolor"]=self.cget("foreground")

    def pack(self,**kw):
        self.canvas.get_tk_widget().pack(kw)

    def update(self):
        if isinstance(self.canvas,FigureCanvasTkAggRedraw):
            self.canvas.get_tk_widget().update()
            FigureCanvasTkAgg.draw(self.canvas)
        else:
            self.canvas.draw()

    def __optionCallback(self,key,value,options):
        if key in ["minx","maxx","miny","maxy","minz","maxz",
                   "realwidth","realheight","azimuth","elevation",
                   "left_margin","right_margin","top_margin","bottom_margin"]:
            self.redrawlabels = 1
            if key[:3] in ["min", "max"]:
                minc = self.cget("min"+key[3])
                maxc = self.cget("max"+key[3])
                if minc < maxc:
                    func = None
                    if self.ax is self.ax3d:
                        func = getattr(self.ax, "set_"+key[3]+"lim3d")
                        self._cur_lims = (
                            Axes.get_xlim(self.ax), Axes.get_ylim(self.ax))
                    elif key[3] != 'z':
                        func = getattr(self.ax, "set_"+key[3]+"lim")
                    if func is not None:
                        func(minc,maxc)
                        tickskey = key[3]+"ticks"
                        ticksval = self.cget(tickskey)
                        if ticksval is not None:
                            self.__optionCallback(tickskey,ticksval,options)
            elif key == "realwidth":
                lm = float(self.cget("left_margin"))
                rm = float(self.cget("right_margin"))
                self.ax.get_figure().subplots_adjust(left=lm/value,
                                                     right=1-rm/value)
            elif key == "realheight":
                tm = float(self.cget("top_margin"))
                bm = float(self.cget("bottom_margin"))
                self.ax.get_figure().subplots_adjust(top=1-tm/value,
                                                     bottom=bm/value)
            elif key == "left_margin":
                fig = self.ax.get_figure()
                width = fig.get_figwidth()*fig.get_dpi()
                fig.subplots_adjust(left=value/width)
            elif key == "right_margin":
                fig = self.ax.get_figure()
                width = fig.get_figwidth()*fig.get_dpi()
                fig.subplots_adjust(right=1-value/width)
            elif key == "top_margin":
                fig = self.ax.get_figure()
                height = fig.get_figheight()*fig.get_dpi()
                fig.subplots_adjust(top=1-value/height)
            elif key == "bottom_margin":
                fig = self.ax.get_figure()
                height = fig.get_figheight()*fig.get_dpi()
                fig.subplots_adjust(bottom=value/height)
            elif self.ax is self.ax3d:
                elev = self.cget("elevation")
                azim = self.cget("azimuth")
                if elev is not None or azim is not None:
                    self.ax.view_init(elev, azim)
        elif key == "grid":
            if value in ["yes", True]:
                self.ax.grid(color = self.cget("foreground"))
            else:
                self.ax.grid(False)
        elif key in ["width", "height"]:
            if isinstance(self.canvas,FigureCanvasTkAggRedraw):
                self.canvas.get_tk_widget()[key] = value
            else:
                fig = self.ax.get_figure()
                if key == "width":
                    fig.set_figwidth(float(value)/fig.get_dpi())
                    self._configNoDraw(realwidth=value)
                else:
                    fig.set_figheight(float(value)/fig.get_dpi())
                    self._configNoDraw(realheight=value)
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
            if self.cget("grid") in ["yes", True]:
                self.ax.grid(color=value)
        elif key == "color_list":
            color_list = value.split()
            i = 0
            for d in self.data:
                if d["newsect"] is None or d["newsect"]:
                    i = i+1
                if d["color"] is None:
                    color = i
                else:
                    color = d["color"]
                d["mpline"].set_color(color_list[color%len(color_list)])
        elif key == "decorations":
            if value:
                self.ax.set_axis_on()
            else:
                self.ax.set_axis_off()
        elif key == "use_symbols":
            self.plotsymbols()
        elif key == "use_labels":
            self.plotlabels()
        elif key in ["xlabel","ylabel","zlabel"]:
            if value is None:
                value = ""
            fontsize = self.cget(key+"_fontsize")
            if hasattr(self.ax,"set_"+key):
                func = getattr(self.ax,"set_"+key)
                if fontsize is None:
                    func(value)
                else:
                    func(value,fontsize=fontsize)
        elif key in ["xlabel_fontsize","ylabel_fontsize","zlabel_fontsize"]:
            label = self.cget(key[:6])
            if hasattr(self.ax,"set_"+key[:6]):
                func = getattr(self.ax,"set_"+key[:6])
                if value is None:
                    func(label)
                else:
                    func(label,fontsize=value)
        elif key in ["xticks","yticks","zticks"]:
            if value is None:
                if self.ax is self.ax3d:
                    axis = getattr(self.ax, "w_"+key[0]+"axis")
                    axis.set_major_locator(AutoLocator())
                elif key == "xticks":
                    self.ax.set_xscale('linear')
                else:
                    self.ax.set_yscale('linear')
            else:
                min = self.cget("min"+key[0])
                max = self.cget("max"+key[0])
                ticks = [min + ((max - min) * i) / float(value-1)
                         for i in range(value)]
                if self.ax is self.ax3d:
                    axis = getattr(self.ax, "w_"+key[0]+"axis")
                    axis.set_major_locator( FixedLocator(ticks) )
                elif key == "xticks":
                    self.ax.set_xticks(ticks)
                elif key == "yticks":
                    self.ax.set_yticks(ticks)

    def _delAllData(self):
        for d in self.data:
            if "mpline" in d:
                self.ax.lines.remove(d["mpline"])
        self.data=[]

        # set type for next data
        try:
            zcolumn = self.cget(self.cget("type")+"_z")
        except Tkinter.TclError: #in regression test
            return
        oldax = self.ax
        if zcolumn is None or Axes3D is None:
            if zcolumn is not None:
                self._configNoDraw({self.cget("type")+"_z":None})
                print("\nmatplotlib 0.98.x does not support 3D plots.")
                print("Plotting only the first two coordinates.")
            if self.ax is self.ax3d:
                # restore zoom mode
                new_zoom_mode = self.zoom_mode
                self.ax = self.ax2d
        else:
            if self.ax3d is None:
                self.ax3d = Axes3D(self.ax.get_figure())
                try:
                    self.ax3d.set_autoscale_on(0)
                except TypeError: #Matplotlib 1.1 bug
                    self.ax2d.set_autoscale_on(0)
                    self.ax3d.set_autoscalez_on(0)
            if self.ax is self.ax2d:
                # remember zoom mode and disable zoom
                if isinstance(self.canvas,FigureCanvasTkAggRedraw):
                    self.zoom_mode = self.toolbar.mode
                    new_zoom_mode = ''
                self.ax = self.ax3d
        if self.ax is not oldax:
            if (isinstance(self.canvas,FigureCanvasTkAggRedraw) and
                new_zoom_mode != self.toolbar.mode):
                if "rect" in new_zoom_mode:
                    self.toolbar.zoom()
                elif "pan" in new_zoom_mode:
                    self.toolbar.pan()
                elif "rect" in self.toolbar.mode:
                    self.toolbar.zoom()
                elif "pan" in self.toolbar.mode:
                    self.toolbar.pan()
            #copy settings from 3d to 2d or vice versa
            for key in ("grid","decorations","xlabel","ylabel","zlabel",
                        "xticks","yticks","zticks",
                        "minx","maxx","miny","maxy","minz","maxz",
                        "azimuth", "elevation",
                        "top_title", "background"):
                self.__optionCallback(key,self.cget(key),[])

    def _delData(self,index):
        if "mpline" in data[index]:
            self.ax.lines.remove(data[index]["mpline"])
        del self.data[index]

    def clear(self):
        if len(self.ax.get_figure().axes) > 0:
            self.ax.get_figure().delaxes(self.ax.get_figure().axes[0])

    def draw(self):
        if self.redrawlabels:
            self.plotlabels()
        if len(self.ax.get_figure().axes) == 0:
            self.ax.get_figure().add_axes(self.ax)
        if isinstance(self.canvas,FigureCanvasTkAggRedraw):
            FigureCanvasTkAgg.draw(self.canvas)
        else:
            self.canvas.draw()

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
            if self.ax is self.ax2d:
                v = [d["x"],d["y"]]
            else:
                v = [d["x"],d["y"],d["z"]]
            if d["color"] is None:
                color = i
            else:
                color = d["color"]
            kw = {'color':color_list[color%len(color_list)]}
            if len(v[0]) == 1:
                # If we only have one point we draw a small circle or a pixel
                if self.cget("type") == "solution":
                    marker = 'o'
                else:
                    marker = ','
                v.append(marker)
                #tags=("data_point:%d"%(0,),curve,"data")
            else:
                stable = d["stable"]
                #tags=(curve,"data")
                kw['lw'] = line_width
                if stable is not None and not stable:
                    kw.update({'ls':'--','dashes':dashes})
            if self.ax is self.ax2d:
                self.ax.plot(*v,**kw)
            else:
                self.ax.plot3D(*v,**kw)
            d["mpline"] = self.ax.lines[-1]
        if len(self.ax.get_figure().axes) == 0:
            self.ax.get_figure().add_axes(self.ax)
            
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

    def _addData(self,data,newsect=None,color=None,stable=None):
        self.labels.append([])
        BasicGrapher._addData(self,data,newsect,color,stable)

    def plotlabels(self):
        if self.cget("realwidth") == 1 or self.cget("realheight") == 1:
            return
        self.redrawlabels = 0

        for labels in self.labels:
            for label in labels:
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
            if self.ax is self.ax2d:
                def transseq(x,y):
                    return trans(transpose([x,y]))
            else:
                # this matrix needs to be initialized or matplotlib only does
                # that when it draws
                self.ax.M = self.ax.get_proj()
                def transseq(x,y,z):
                    return trans(transpose(proj_transform(x,y,z,self.ax.M)[:2]))
            inv_trans = self.ax.transData.inverted().transform
        else:
            trans = self.ax.transData.xy_tup
            if self.ax is self.ax2d:
                def transseq(x,y):
                    return transpose(self.ax.transData.numerix_x_y(x,y))
            else:
                self.ax.M = self.ax.get_proj()
                def transseq(x,y,z):
                    return transpose(self.ax.transData.numerix_x_y(
                            *(proj_transform(x,y,z,self.ax.M)[:2])))
            inv_trans = self.ax.transData.inverse_xy_tup
        if self.cget("smart_label"):
            mp = self.inarrs()
            sp1 = self.cget("left_margin")
            sp2 = 5 #fontsize
            sp3 = self.cget("bottom_margin")
            sp4 = 5
            for d in self.data:
                if self.ax is self.ax2d:
                    seq = transseq(d["x"],d["y"])
                else:
                    seq = transseq(d["x"],d["y"],d["z"])
                seq[:,0] = (seq[:,0] - sp1) / sp2
                seq[:,1] = (seq[:,1] - sp3) / sp4
                self.map_curve(mp,seq)
                                                      
        for i, labels in enumerate(self.labels):
            for label in labels:
                if len(label["text"]) == 0:
                    continue
                [x,y] = label["xy"][:2]
                if self.ax is self.ax3d:
                    # transform 3d to 2d coordinates and compare
                    # against the 2D limits
                    z = label["xy"][2]
                    [x,y,z] = proj_transform(x, y, z, self.ax.M)
                lim = Axes.get_xlim(self.ax)
                if x < lim[0] or x > lim[1]:
                    continue
                lim = Axes.get_ylim(self.ax)
                if y < lim[0] or y > lim[1]:
                    continue
                data = trans((x,y))
                if data is not None:
                    [x,y] = data
                    if self.cget("smart_label"):
                        [xoffd1,yoffd1,xoffd2,yoffd2,
                         xofft,yofft,pos] = self.findsp(x,y,mp)
                    else:
                        [xoffd1,yoffd1,xoffd2,yoffd2,
                         xofft,yofft,pos] = self.dumblabel(i,label["j"],x,y)
                    [ha,va] = self.getpos(pos)
                    [xd1,yd1] = inv_trans((x+xoffd1,y+yoffd1))
                    [xd2,yd2] = inv_trans((x+xoffd2,y+yoffd2))
                    [xt,yt] = inv_trans((x+xofft,y+yofft))
                    # using the low-level line routines to avoid
                    # rescaling in 3D (even with auto scale off,
                    # the view is reset after each plot() by
                    # set_top_view in mplot3d)
                    line = Line2D([xd1,xd2],[yd1,yd2],linewidth=0.5,
                                  color=self.cget("foreground"))
                    self.ax.add_line(line)
                    self.ax.annotate(label["text"],(xt,yt),ha=ha,va=va,
                                     color=self.cget("foreground"),clip_on=True)
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
        for labels in self.labels:
            for label in labels:
                v = label["xy"]
                if Axes3D is None:
                    v = v[:2]
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
                if len(l) <= 3:
                    #font=self.cget("symbol_font"),
                    kw = {'ha':"center",'va':"center",'color':c,'clip_on':True}
                    if self.ax is self.ax2d:
                        self.ax.text(*(v+[l]),**kw)
                    else:
                        t = self.ax.text3D(*(v+[l]))
                        t.set_horizontalalignment(kw['ha'])
                        t.set_verticalalignment(kw['va'])
                        t.set_color(kw['color'])
                        t.set_clip_on(True)
                    label["mpsymtext"] = self.ax.texts[-1]
                    continue
                v = [[coord] for coord in v]
                markerdict = {"fillcircle" : "o", "circle": "o",
                              "square": "s", "crossquare": "x",
                              "fillsquare": "s", "diamond": "D",
                              "filldiamond": "D", "triangle": "^",
                              "doubletriangle": "^"}
                ms = None
                for i, ch in enumerate(l):
                    if ch in string.digits:
                        ms = int(l[i:])
                        l = l[:i]
                        break
                if l not in markerdict:
                    continue
                v.append(markerdict[l])
                kw = {'color': c}
                if l in ["circle", "square", "diamond", "triangle"]:
                    kw['mfc'] = self.ax.get_axis_bgcolor()
                if l in ["diamond","filldiamond","triangle","doubletriangle"]:
                    kw['ms'] = 8
                if ms is not None:
                    kw['ms'] = ms
                if self.ax is self.ax2d:
                    self.ax.plot(*v,**kw)
                else:
                    self.ax.plot3D(*v,**kw)
                label["mpsymline"] = self.ax.lines[-1]

# FIXME:  No regression tester
class InteractiveGrapher(LabeledGrapher,grapher.InteractiveGrapher):
    def __init__(self,parent=None,**kw):
        LabeledGrapher.__init__(self,parent,**kw)

class GUIGrapher(InteractiveGrapher,grapher.GUIGrapher):
    def __init__(self,parent=None,**kw):
        InteractiveGrapher.__init__(self,parent,**kw)
        #self.bind("<ButtonPress-3>",self.popupMenuWrapper)
        if kw.get("hide"):
            return
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
        options = sorted([self.configure(key)[0] for key in self.configure() 
                          if self._isInternalOption(key)])

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

def test():
    grapher.test(GUIGrapher())

if __name__=='__main__':
    test()






