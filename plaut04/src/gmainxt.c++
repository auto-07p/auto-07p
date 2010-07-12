#include <sstream>

#include <X11/Intrinsic.h>

#include <Xm/Xm.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/ComboBox.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/FileSB.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/MessageB.h>
#include <Xm/Notebook.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/Separator.h>
#include <Xm/SeparatoG.h>
#include <Xm/SpinB.h>
#include <Xm/SSpinB.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>

#include <Inventor/Xt/SoXt.h>
#include <Inventor/Xt/viewers/SoXtExaminerViewer.h>
#include <Inventor/Xt/SoXtMaterialEditor.h>

#include "gplaut04.h"
#include "solution.h"
#include "bifurcation.h"

#define LBL_OFFSET   4

#ifdef LESSTIF_VERSION
#include <Xm/ComboBoxP.h>
#ifndef XmNwrap
#define XmNwrap ((char *)&_XmStrings[19401])
#endif
#undef XmFONTLIST_DEFAULT_TAG
#define XmFONTLIST_DEFAULT_TAG NULL
#endif

#ifdef USE_EXAM_VIEWER
static SoXtExaminerViewer *renderArea;
#else
static SoXtRenderArea *renderArea;
#endif

SbBool printToPostScript (SoNode *root, const char *filename,
SoXtExaminerViewer *viewer, int printerDPI);

struct ViewerAndScene
{
    SoXtExaminerViewer *viewer;
    char               *filename;
    SoNode             *scene;
} ;

ViewerAndScene *vwrAndScene;

static const unsigned long systemLinePatternValue[] =
{
    0xfffff, 0x7777,  0x3333,  0xfafa, 0xeaea, 0xffcc, 0xffdc,0xff9c,0
};

static const char *systemLinePatternLookAndFeel[] =
{
    "SOLID LINE",   "--------",   ". . . . . ",    "_ . _ . _ .",
    "_ . . _ . .",  "_ . . . _",  "___ _ ___ _", "____ __ ____",
    "NULL "
};

GC gc;
Colormap colormap;
XColor black, grey, red, white, green, blue, exact;

Widget  topform;
Widget  xAxisList, yAxisList, zAxisList, labelsList, colorMethodSeletionList;
Widget satAniSpeedSlider, orbitAniSpeedSlider, dimButton;

typedef struct EditMenuItems
{
    Widget *items;
    int     which;
} EditMenuItems;

EditMenuItems *typeMenuItems, *styleMenuItems, *coordMenuItems;

EditMenuItems *coordSystemMenuItems;
Widget fileDialog = NULL;

XmStringTable xList = (XmStringTable) 0 ;
XmStringTable yList = (XmStringTable) 0 ;
XmStringTable zList = (XmStringTable) 0 ;
XmStringTable lblList = (XmStringTable) 0 ;
XmStringTable clrMethodList= (XmStringTable) 0 ;

static void xListCallBack(Widget combo, XtPointer client_data, XtPointer call_data);
static void yListCallBack(Widget combo, XtPointer client_data, XtPointer call_data);
static void zListCallBack(Widget combo, XtPointer client_data, XtPointer call_data);
static void lblListCallBack(Widget combo, XtPointer client_data, XtPointer call_data);
static void fileDialogCB(Widget, XtPointer client_data, XtPointer data);
static void hidenDialogShell (Widget widget, XtPointer client_data, XtPointer call_data);
static void applyPreferDialogChangeAndUpdateScene(Widget widget, XtPointer client_data, XtPointer call_data);
static void closePreferDialogAndUpdateScene(Widget widget, XtPointer client_data, XtPointer call_data);
static void closePreferDialogAndGiveUpChange(Widget widget, XtPointer client_data, XtPointer call_data);
static void savePreferAndUpdateScene(Widget widget, XtPointer client_data, XtPointer call_data);
static void getFileName(int fileMode);
static void showAboutDialog();
static void createPreferDialog();
extern SoSeparator * createBoundingBox();
static void showAboutCB(Widget, XtPointer, XtPointer);

////////////////////////////////////////////////////////////////////////
//
//  functions
//
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
//
static void
orbitSpeedCB(Widget, XtPointer userData, XtPointer callData)
//
////////////////////////////////////////////////////////////////////////
{
    XmScaleCallbackStruct *data = (XmScaleCallbackStruct *) callData;
    orbitSpeed = data->value/50.0;                ///50.0;     ///75.0;
    if(orbitSpeed == 0.0) orbitSpeed = 0.0001;
    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
static void
satSpeedCB(Widget, XtPointer userData, XtPointer callData)
//
////////////////////////////////////////////////////////////////////////
{
    XmScaleCallbackStruct *data = (XmScaleCallbackStruct *) callData;
    satSpeed = data->value/100.0;
    updateScene();
}

////////////////////////////////////////////////////////////////////////
//
static void
numPeriodAnimatedCB(Widget, XtPointer userData, XtPointer callData)
//
////////////////////////////////////////////////////////////////////////
{
    XmComboBoxCallbackStruct *cbs = (XmComboBoxCallbackStruct *)callData;
    char *myChoice = (char *) XmStringUnparse (cbs->item_or_text, XmFONTLIST_DEFAULT_TAG,
        XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);

    if ( strcmp(myChoice, "inf") == 0 )
    {
       numPeriodAnimated = -1; 
    } 
    else 
    {
       numPeriodAnimated = atof(myChoice);
    }

//cout <<" Num Period Animated "<<myChoice<<"   "<<numPeriodAnimated;

    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
static void
colorMethodSelectionCB(Widget, XtPointer userData, XtPointer callData)
//
////////////////////////////////////////////////////////////////////////
{
    XmComboBoxCallbackStruct *cbs = (XmComboBoxCallbackStruct *)callData;
    char *myChoice = (char *) XmStringUnparse (cbs->item_or_text, XmFONTLIST_DEFAULT_TAG,
        XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
    int choice = (int) cbs->item_position;

    coloringMethod = (strcasecmp(myChoice,"COMP")==0) ?  CL_COMPONENT:
    ((strcasecmp(myChoice,"TYPE")==0) ? CL_ORBIT_TYPE :
    ((strcasecmp(myChoice,"CURV")==0) ? CL_CURVE_NUMBER:
    ((strcasecmp(myChoice,"BRAN")==0) ? CL_BRANCH_NUMBER:
    ((strcasecmp(myChoice,"PONT")==0) ? CL_POINT_NUMBER :
    ((strcasecmp(myChoice,"LABL")==0) ? CL_LABELS:
     ((strcasecmp(myChoice,"STAB")==0) ? CL_STABILITY : choice - specialColorItems))))));
    coloringMethodType[whichType] = coloringMethod;

    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
static void
lineWidthCB(Widget, XtPointer userData, XtPointer callData)
//
////////////////////////////////////////////////////////////////////////
{
    Widget        spin = (Widget) userData;
    unsigned char type;
    int           position;

    XtVaGetValues (spin, XmNspinBoxChildType, &type, XmNposition, &position, NULL);
#ifdef LESSTIF_VERSION
    if (position < 10) {
      position = 10;
      XtVaSetValues (spin, XmNposition, position, NULL);
    }
#endif
    lineWidthScaler = position/10.0;
    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
//  This is called by Xt when a menu item is picked from the File menu.
//
static void
fileMenuPick(Widget, void *userData, XtPointer *)
//
////////////////////////////////////////////////////////////////////////
{
    int    which = (long) userData;

    switch (which)
    {
        case SAVE_ITEM:
            getFileName(SAVE_ITEM);
            break;

        case QUIT_ITEM:
            postDeals();
            exit(0);
            break;
        case PRINT_ITEM:
            getFileName(PRINT_ITEM);
            break;
        case OPEN_ITEM:
            getFileName(OPEN_ITEM);
            break;
        default:
            printf("UNKNOWN file menu item!!!\n"); break;
    }
}


////////////////////////////////////////////////////////////////////////
//
//  This is called by Xt when a menu item is picked from the Edit menu.
//
static void
editMenuPick( Widget w, void *userData, XmAnyCallbackStruct *cb)
//
////////////////////////////////////////////////////////////////////////
{
    int which = (long) userData;

    Arg     args[1];
    EditMenuItems *menuItems;
    XtSetArg(args[0], XmNuserData, &menuItems);
    XtGetValues(w, args, 1);
    menuItems->which = which;
    whichCoordSystem = which;
    whichCoordSystemOld = whichCoordSystem;

    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
//  This is called by Xt when a menu item is picked from the TYPE menu.
//
static void
typeMenuPick(Widget w, void *userData, XmAnyCallbackStruct *cb)
//
////////////////////////////////////////////////////////////////////////
{
    int which = (long) userData;

// get the user data for this menu item.
    Arg     args[1];
    EditMenuItems *menuItems;
    XtSetArg(args[0], XmNuserData, &menuItems);
    XtGetValues(w, args, 1);

    menuItems->which = which;
    whichType    = which;

    if ( whichType != whichTypeOld )
    {
        graphWidgetToggleSet = 0;
        if( whichType != BIFURCATION )
        {
            setShow3D = setShow3DSol;
            for(int i=0; i<11; ++i)
            {
                optBif[i]  = options[i];
                options[i] = optSol[i];
                graphWidgetToggleSet |= options[i] << i;
            }
        }
        else
        {
            setShow3D = setShow3DBif;
            for(int i=0; i<11; ++i)
            {
                optSol[i]  = options[i];
                options[i] = optBif[i];
                graphWidgetToggleSet |= options[i] << i;
            }
        }
        coloringMethodType[whichType] = coloringMethod;
    }

    whichTypeOld = whichType;

    setListValue();
    XmString xString;
    if (setShow3D)
        xString = XmStringCreateLocalized((char *)"3D");
    else
        xString = XmStringCreateLocalized((char *)"2D");
    XtVaSetValues (dimButton, XmNlabelString, xString, NULL);
    XmStringFree(xString);

    XtVaSetValues(xAxisList, XmNselectedPosition, xCoordIndices[0], NULL);
    XtVaSetValues(yAxisList, XmNselectedPosition, yCoordIndices[0], NULL);
    XtVaSetValues(zAxisList, XmNselectedPosition, zCoordIndices[0], NULL);

    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
//       This is called by Xt when a menu item is picked from the Option menu.
//
static void
optMenuPick(Widget widget, void *userData, XmAnyCallbackStruct *cb)  
//
////////////////////////////////////////////////////////////////////////
{
    XmToggleButtonCallbackStruct *toggle = (XmToggleButtonCallbackStruct *)cb;
    int which = (long) userData;

    if (toggle->set == XmSET)
    {
        graphWidgetToggleSet |= (1 << which);
        options[which] = true;
    }
    else
    {
        options[which] = false;
        graphWidgetToggleSet &= ~(1 << which);
    }

    if(options[OPT_SAT_ANI])
        XtVaSetValues (satAniSpeedSlider, XmNeditable, TRUE, NULL);
    else
        XtVaSetValues (satAniSpeedSlider, XmNeditable, FALSE, NULL);

    if(options[OPT_PERIOD_ANI])
        XtVaSetValues (orbitAniSpeedSlider, XmNeditable, TRUE, NULL);
    else
        XtVaSetValues (orbitAniSpeedSlider, XmNeditable, FALSE, NULL);

    if(useR3B && (graphWidgetToggleSet & (1<<OPT_NORMALIZE_DATA)))
    {
        options[OPT_PRIMARY] = false;
        options[OPT_LIB_POINTS] = false;
        options[OPT_REF_PLAN] = false;
        graphWidgetToggleSet &= ~(1 << OPT_REF_PLAN);
        graphWidgetToggleSet &= ~(1 << OPT_LIB_POINTS);
        graphWidgetToggleSet &= ~(1 << OPT_PRIMARY);
    }

    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
static void lessTifFixupComboBox(Widget w, XmStringTable strList, int nItems,
                               int visible, int width, int columns)
//
////////////////////////////////////////////////////////////////////////
{
#ifdef LESSTIF_VERSION
    Widget list = CB_List(w);
    XmListDeleteAllItems(list);
    for (int i = 0; i < nItems; i++)
	XmListAddItem(list,strList[i],0);
    XtVaSetValues(w,
        XmNvisibleItemCount,   visible,
        XmNwidth,              width,
        XmNcolumns,            columns,
        NULL);
    XmComboBoxUpdate(w);
#endif
}


///////////////////////////////////////////////////////////////////////////
//
void
setListValue()
//
///////////////////////////////////////////////////////////////////////////
{
    int nar;
    int count = labels.size();
    lblList = (XmStringTable) XtMalloc(count * sizeof (XmString *));
    for (int i = 0; i < count; i++)
        lblList[i] = XmStringCreateLocalized (const_cast<char *>(labels[i].c_str()));

    if(whichType != BIFURCATION)
    {
        xCoordIndices = dai.solX;
        yCoordIndices = dai.solY;
        zCoordIndices = dai.solZ;
        nar = mySolNode->nar();
    }
    else
    {
        xCoordIndices = dai.bifX;
        yCoordIndices = dai.bifY;
        zCoordIndices = dai.bifZ;
        nar = myBifNode->nar();
    }

    XtVaSetValues(xAxisList, XmNitems, xList, XmNitemCount, nar, NULL);
    XtVaSetValues(yAxisList, XmNitems, yList, XmNitemCount, nar, NULL);
    XtVaSetValues(zAxisList, XmNitems, zList, XmNitemCount, nar, NULL);
    lessTifFixupComboBox(xAxisList, xList, nar, 10, 60, 2);
    lessTifFixupComboBox(yAxisList, yList, nar, 10, 60, 2);
    lessTifFixupComboBox(zAxisList, zList, nar, 10, 60, 2);

    coloringMethodList.clear();
    coloringMethodList.push_back("BRAN");
    coloringMethodList.push_back("STAB");
    coloringMethodList.push_back("PONT");
    coloringMethodList.push_back("CURV");

    specialColorItems = 4;

    if(whichType != BIFURCATION)
    {
        coloringMethodList.push_back("TYPE");
        coloringMethodList.push_back("LABL");
        coloringMethodList.push_back("COMP");
        specialColorItems = 7;
    }
    for(int i=0; i<nar; ++i)
    {
        std::stringstream s;
        s << i;
        coloringMethodList.push_back(s.str());
    }
    if(whichType != BIFURCATION)
    {
        for(int i=0; i<mySolNode->npar(); ++i)
        {
            std::stringstream s;
            s << "PAR(" << mySolNode->parID(i)+1 << ")";
            coloringMethodList.push_back(s.str());
        }

    }

    count = coloringMethodList.size();
    for (int i = 0; i < count; ++i)
        clrMethodList[i] = XmStringCreateLocalized (const_cast<char *>(coloringMethodList[i].c_str()));
    XtVaSetValues(colorMethodSeletionList, XmNitems, clrMethodList, 
	               XmNitemCount, count, NULL);
    long nItems = labels.size();
    XtVaSetValues(labelsList, XmNitems, lblList, XmNitemCount, nItems, NULL);
    lessTifFixupComboBox(colorMethodSeletionList, clrMethodList, 
            count, 10, 80, 5);
    lessTifFixupComboBox(labelsList, lblList, nItems, 10, 86, 6);

    if(setShow3D)
        XtSetSensitive (zAxisList, true);
    else
    {
        zCoordIndices.clear();
        zCoordIndices.push_back(-1);
        XtSetSensitive (zAxisList, false);
    }

}


////////////////////////////////////////////////////////////////////////
//
//  This is called by Xt when a menu item is picked from the STYLE menu.
//
static void
styleMenuPick(Widget w, void *userData, XmAnyCallbackStruct *cb)
//
////////////////////////////////////////////////////////////////////////
{
    int which = (long) userData;

// get the user data for this menu item.
    Arg     args[1];
    EditMenuItems *menuItems;
    XtSetArg(args[0], XmNuserData, &menuItems);
    XtGetValues(w, args, 1);

    menuItems->which = which;
    whichStyle = which;
    whichStyleOld = whichStyle;

    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
//  This is called by Xt when a menu item is picked from the STYLE menu.
//
static void
coordMenuPick(Widget w, void *userData, XmAnyCallbackStruct *cb)
//
////////////////////////////////////////////////////////////////////////
{
    int which = (long) userData;

// get the user data for this menu item.
    Arg     args[1];
    EditMenuItems *menuItems;
    XtSetArg(args[0], XmNuserData, &menuItems);
    XtGetValues(w, args, 1);

    menuItems->which = which;

    if(which == DRAW_TICKER)
        blDrawTicker = !blDrawTicker ;
    else
    {
        whichCoord = which;
        whichCoordOld = whichCoord;
    }

    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
//  This is called by Xt just before the TYPE menu is displayed.
//
static void 
typeMenuDisplay(Widget, void *userData, XtPointer)
//
////////////////////////////////////////////////////////////////////////
{
    EditMenuItems *menuItems = (EditMenuItems *) userData;

    TOGGLE_OFF(menuItems->items[SOLUTION]);
    TOGGLE_OFF(menuItems->items[BIFURCATION]);

    TOGGLE_ON(menuItems->items[menuItems->which]);

    XtSetSensitive (menuItems->items[SOLUTION], TRUE);
    XtSetSensitive (menuItems->items[BIFURCATION], TRUE);

    if(mySolNode->numOrbits() == 0)
        XtSetSensitive (menuItems->items[SOLUTION], FALSE);

    if(myBifNode->totalNumPoints() == 0)
        XtSetSensitive (menuItems->items[BIFURCATION], FALSE);

}


////////////////////////////////////////////////////////////////////////
//
//  This is called by Xt just before the STYLE menu is displayed.
//
static void 
styleMenuDisplay(Widget, void *userData, XtPointer)
//
////////////////////////////////////////////////////////////////////////
{
    EditMenuItems *menuItems = (EditMenuItems *) userData;
    TOGGLE_OFF(menuItems->items[LINE]);
    TOGGLE_OFF(menuItems->items[TUBE]);
    TOGGLE_OFF(menuItems->items[SURFACE]);
    TOGGLE_OFF(menuItems->items[MESH_POINTS]);
    TOGGLE_OFF(menuItems->items[ALL_POINTS]);

    if((useR3B && whichCoordSystem != ROTATING_F) || whichType == BIFURCATION)
    {
        XtSetSensitive (menuItems->items[SURFACE], false);
        XtSetSensitive (menuItems->items[MESH_POINTS], false);
        XtSetSensitive (menuItems->items[ALL_POINTS], false);
        if(useR3B && (menuItems->which == SURFACE || menuItems->which == MESH_POINTS || menuItems->which == ALL_POINTS))
            menuItems->which = LINE;
    }
    else
    {
        XtSetSensitive (menuItems->items[SURFACE], true);
        XtSetSensitive (menuItems->items[MESH_POINTS], true);
        XtSetSensitive (menuItems->items[ALL_POINTS], true);
    }

    TOGGLE_ON(menuItems->items[menuItems->which]);

}


////////////////////////////////////////////////////////////////////////
//
//  This is called by Xt just before the STYLE menu is displayed.
//
static void 
coordMenuDisplay(Widget, void *userData, XtPointer)
//
////////////////////////////////////////////////////////////////////////
{
    EditMenuItems *menuItems = (EditMenuItems *) userData;

    if(menuItems->which == DRAW_TICKER)
    {
        if(blDrawTicker)
            XmToggleButtonSetState(menuItems->items[menuItems->which], TRUE, FALSE);
        else
            XmToggleButtonSetState(menuItems->items[menuItems->which], FALSE, FALSE);
    }
    else
    {
        TOGGLE_OFF(menuItems->items[NO_COORD]);
        TOGGLE_OFF(menuItems->items[COORDORIGIN]);
        TOGGLE_OFF(menuItems->items[LEFTBACK]);
        TOGGLE_OFF(menuItems->items[LEFTAHEAD]);
        TOGGLE_OFF(menuItems->items[COORD_AT_ORIGIN]);
        TOGGLE_OFF(menuItems->items[DRAW_TICKER]);

        TOGGLE_ON(menuItems->items[menuItems->which]);
        if(blDrawTicker)
            XmToggleButtonSetState(menuItems->items[DRAW_TICKER], TRUE, FALSE);
    }
}


////////////////////////////////////////////////////////////////////////
//
//  This is called by Xt just before the Edit menu is displayed.
//
static void
centerMenuDisplay(Widget, void *userData, XtPointer)
//
////////////////////////////////////////////////////////////////////////
{
    EditMenuItems *menuItems = (EditMenuItems *) userData;

    TOGGLE_OFF(menuItems->items[ROTATING_F]);
    TOGGLE_OFF(menuItems->items[INERTIAL_B]);
    TOGGLE_OFF(menuItems->items[INERTIAL_S]);
    TOGGLE_OFF(menuItems->items[INERTIAL_E]);
    TOGGLE_ON(menuItems->items[menuItems->which]);

    if(whichType == SOLUTION)
    {
        XtSetSensitive (menuItems->items[ROTATING_F], true);
        XtSetSensitive (menuItems->items[INERTIAL_B], true);
        XtSetSensitive (menuItems->items[INERTIAL_E], true);
        XtSetSensitive (menuItems->items[INERTIAL_S], true);
    }
    else
    {
        XtSetSensitive (menuItems->items[ROTATING_F], false);
        XtSetSensitive (menuItems->items[INERTIAL_B], false);
        XtSetSensitive (menuItems->items[INERTIAL_S], false);
        XtSetSensitive (menuItems->items[INERTIAL_E], false);

    }

}


////////////////////////////////////////////////////////////////////////
//
//  This is called by Xt just before the TYPE menu is displayed.
//
static void 
optMenuDisplay(Widget, void *userData, XtPointer)
//
////////////////////////////////////////////////////////////////////////
{
    EditMenuItems *menuItems = (EditMenuItems *) userData;

    for (unsigned int i = 0; i < XtNumber (graphWidgetItems); i++)
    {
        if (!useR3B && (i == OPT_PRIMARY || i == OPT_LIB_POINTS)) continue;
        if (graphWidgetToggleSet & (1<<i))
        {
            XmToggleButtonSetState(menuItems->items[i], TRUE, FALSE);
        }
        else
        {
            XmToggleButtonSetState(menuItems->items[i], FALSE, FALSE);
        }
    }

    if(useR3B)
    {
        if(!blMassDependantOption)
        {
            XtSetSensitive (menuItems->items[OPT_PRIMARY], false);
            XtSetSensitive (menuItems->items[OPT_LIB_POINTS], false);
        }
        else
        {
            XtSetSensitive (menuItems->items[OPT_PRIMARY], true);
            XtSetSensitive (menuItems->items[OPT_LIB_POINTS], true);
        }

        if(graphWidgetToggleSet & (1<<OPT_NORMALIZE_DATA))
        {
            XtSetSensitive (menuItems->items[OPT_PRIMARY], false);
            XtSetSensitive (menuItems->items[OPT_LIB_POINTS], false);
            XtSetSensitive (menuItems->items[OPT_REF_PLAN], false);
            XmToggleButtonSetState(menuItems->items[OPT_PRIMARY], FALSE, FALSE);
            XmToggleButtonSetState(menuItems->items[OPT_LIB_POINTS], FALSE, FALSE);
            XmToggleButtonSetState(menuItems->items[OPT_REF_PLAN], FALSE, FALSE);
        }
        else
        {
            XtSetSensitive (menuItems->items[OPT_PRIMARY], true);
            XtSetSensitive (menuItems->items[OPT_PRIMARY], true);
            XtSetSensitive (menuItems->items[OPT_REF_PLAN], true);
        }
    }

    if(whichType == BIFURCATION)
    {
        XtUnmanageChild(menuItems->items[OPT_PERIOD_ANI]);
        XtUnmanageChild(menuItems->items[OPT_SAT_ANI]);
        XtManageChild(menuItems->items[OPT_DRAW_LABELS]);
        XtManageChild(menuItems->items[OPT_LABEL_NUMBERS]);
        XtSetSensitive (menuItems->items[OPT_LABEL_NUMBERS],
            (graphWidgetToggleSet & (1<<OPT_DRAW_LABELS)) != 0);
    }
    else
    {
        XtUnmanageChild(menuItems->items[OPT_DRAW_LABELS]);
        XtUnmanageChild(menuItems->items[OPT_LABEL_NUMBERS]);
        XtManageChild(menuItems->items[OPT_PERIOD_ANI]);
        XtManageChild(menuItems->items[OPT_SAT_ANI]);
    }
}


// A push item has no on/off state - it merely gets selected
#define PUSH_ITEM(ITEM,NAME,KONST,FUNC) \
    ITEM = XtCreateManagedWidget(NAME, \
    xmPushButtonGadgetClass, pulldown, args, n); \
    XtAddCallback(ITEM, XmNactivateCallback,\
    (XtCallbackProc) FUNC, \
    (XtPointer) KONST)

// A toggle item has on/off state
#define TOGGLE_ITEM(ITEM,NAME,KONST,FUNC) \
    ITEM = XtCreateManagedWidget(NAME, \
    xmToggleButtonGadgetClass, pulldown, args, n); \
    XtAddCallback(ITEM, XmNvalueChangedCallback,\
    (XtCallbackProc) FUNC, \
    (XtPointer) KONST)

// A separator draws a line between menu items
#define SEP_ITEM(NAME) \
    (void) XtCreateManagedWidget("separator", \
    xmSeparatorGadgetClass, pulldown, NULL, 0)

////////////////////////////////////////////////////////////////////////
//
//  This creates the File menu and all its items.
//
static Widget
buildFileMenu(Widget menubar)
//
////////////////////////////////////////////////////////////////////////
{
    Widget  items[4];
    Widget  pulldown;
    Arg     args[8];
    int        n;

    Arg popupargs[2];
    int popupn = 0;

    pulldown = XmCreatePulldownMenu(menubar, (char *)"fileMenu", popupargs, popupn);

// Accelerators are keyboard shortcuts for the menu items
    const char *openAccel  = "Ctrl<Key>o";
    const char *saveAccel  = "Ctrl<Key>s";
    const char *quitAccel  = "Ctrl<Key>q";
    XmString openAccelText  = XmStringCreate((char *)"Ctrl+o", (char *)XmSTRING_DEFAULT_CHARSET);
    XmString saveAccelText  = XmStringCreate((char *)"Ctrl+s", (char *)XmSTRING_DEFAULT_CHARSET);
    XmString printAccelText = XmStringCreate((char *)"Ctrl+p", (char *)XmSTRING_DEFAULT_CHARSET);
    XmString quitAccelText  = XmStringCreate((char *)"Ctrl+q", (char *)XmSTRING_DEFAULT_CHARSET);
    n = 0;
    XtSetArg(args[n], XmNaccelerator, openAccel); n++;
    XtSetArg(args[n], XmNacceleratorText, openAccelText); n++;
    PUSH_ITEM(items[0], "Open...", OPEN_ITEM, fileMenuPick);

    n = 0;
    XtSetArg(args[n], XmNaccelerator, saveAccel); n++;
    XtSetArg(args[n], XmNacceleratorText, saveAccelText); n++;
    PUSH_ITEM(items[1], "Export...", SAVE_ITEM, fileMenuPick);

    n = 0;
    const char *printAccel = "Ctrl<Key>p";
    XtSetArg(args[n], XmNaccelerator, printAccel); n++;
    XtSetArg(args[n], XmNacceleratorText, printAccelText); n++;
    PUSH_ITEM(items[2], "Print...", PRINT_ITEM, fileMenuPick);

    SEP_ITEM("separator");

    n = 0;
    XtSetArg(args[n], XmNaccelerator, quitAccel); n++;
    XtSetArg(args[n], XmNacceleratorText, quitAccelText); n++;
    PUSH_ITEM(items[3], "Quit",    QUIT_ITEM, fileMenuPick);
    XtManageChildren(items, 4);

    return pulldown;
}


////////////////////////////////////////////////////////////////////////
//
//  This creates the Help menu and all its items.
//
static Widget
buildHelpMenu(Widget menubar)
//
////////////////////////////////////////////////////////////////////////
{
    Widget  items[2];
    Widget  pulldown;
    Arg     args[8];
    int     n;

// Tell motif to create the menu in the popup plane
    Arg popupargs[2];
    int popupn = 0;

    pulldown = XmCreatePulldownMenu(menubar, (char *)"helpMenu", popupargs, popupn);

    n = 0;
    PUSH_ITEM(items[0], "About", ITEM_ONE, showAboutDialog);

    SEP_ITEM("separator");

    n = 0;
    PUSH_ITEM(items[1], "HELP", ITEM_TWO, showHelpDialog);
    XtManageChildren(items, 2);

    return pulldown;
}


////////////////////////////////////////////////////////////////////////
//
//  This creates the TYPE menu and all its items.
//
static Widget
buildOptionMenu(Widget menubar)
//
////////////////////////////////////////////////////////////////////////
{
    Widget  pulldown;
    Arg     args[13];
    int        n;

    EditMenuItems *menuItems = new EditMenuItems;
    menuItems->items = new Widget[XtNumber (graphWidgetItems)];

// Tell motif to create the menu in the popup plane
    Arg popupargs[XtNumber (graphWidgetItems)];
    int popupn = 0;

    pulldown = XmCreatePulldownMenu(menubar, (char *)"optionMenu", popupargs, popupn);

    XtAddCallback(pulldown, XmNmapCallback,
        (XtCallbackProc) optMenuDisplay, (XtPointer) menuItems);

    n = 0;
    int mq = 0;
    XtSetArg(args[n], XmNuserData, menuItems); n++;
    if(!useR3B)
    {
        TOGGLE_ITEM(menuItems->items[OPT_PERIOD_ANI], "Hightlight Orbit",     OPT_PERIOD_ANI, optMenuPick); ++mq;
        TOGGLE_ITEM(menuItems->items[OPT_SAT_ANI], "Orbit Animation",      OPT_SAT_ANI,    optMenuPick); ++mq;
    }
    TOGGLE_ITEM(menuItems->items[OPT_REF_PLAN], "Draw Reference Plane", OPT_REF_PLAN,   optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[OPT_REF_SPHERE], "Draw Reference Sphere", OPT_REF_SPHERE,optMenuPick); ++mq;
    if(useR3B)
    {
        TOGGLE_ITEM(menuItems->items[OPT_PRIMARY], "Draw Primaries",       OPT_PRIMARY,    optMenuPick); ++mq;
        TOGGLE_ITEM(menuItems->items[OPT_LIB_POINTS], "Draw Libration Pts",   OPT_LIB_POINTS, optMenuPick); ++mq;
        SEP_ITEM("separator");
        TOGGLE_ITEM(menuItems->items[OPT_PERIOD_ANI], "Orbit Animation",      OPT_PERIOD_ANI, optMenuPick); ++mq;
        TOGGLE_ITEM(menuItems->items[OPT_SAT_ANI], "Satellite Animation",  OPT_SAT_ANI,    optMenuPick); ++mq;
    }
    TOGGLE_ITEM(menuItems->items[OPT_DRAW_LABELS], "Draw Labels",          OPT_DRAW_LABELS, optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[OPT_LABEL_NUMBERS], "Show Label Numbers",   OPT_LABEL_NUMBERS, optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[OPT_BACKGROUND], "Draw Background",      OPT_BACKGROUND, optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[OPT_LEGEND], "Add Legend",           OPT_LEGEND, optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[OPT_NORMALIZE_DATA], "Normalize Data",       OPT_NORMALIZE_DATA, optMenuPick); ++mq;
    if (useR3B) {
        XtManageChildren(menuItems->items, mq);
    } else {
        XtManageChildren(menuItems->items, OPT_REF_SPHERE+1);
        XtManageChildren(&menuItems->items[OPT_PERIOD_ANI], mq-OPT_REF_SPHERE-1);
    }
    Widget pushitem;
    SEP_ITEM("separator");
    PUSH_ITEM(pushitem, "PREFERENCES", ITEM_ONE, createPreferDialog);
    XtManageChild(pushitem);

    return pulldown;
}


////////////////////////////////////////////////////////////////////////
//
//  This creates the Edit menu and all its items.
//
static Widget
buildCenterMenu(Widget menubar)
//
////////////////////////////////////////////////////////////////////////
{
    Widget  pulldown;
    Arg     args[4];
    int        n;

    coordSystemMenuItems = new EditMenuItems;
    coordSystemMenuItems->items = new Widget[4];
    coordSystemMenuItems->which = whichCoordSystem;

// Tell motif to create the menu in the popup plane
    Arg popupargs[4];
    int popupn = 0;

    pulldown = XmCreatePulldownMenu(menubar, (char *)"editMenu", popupargs, popupn);

    XtAddCallback(pulldown, XmNmapCallback,
        (XtCallbackProc)centerMenuDisplay, (XtPointer) coordSystemMenuItems);

    n = 0;
    XtSetArg(args[n], XmNuserData, coordSystemMenuItems); n++;
    XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
    TOGGLE_ITEM(coordSystemMenuItems->items[0],
        "Rotating Frame", ROTATING_F, editMenuPick);
    SEP_ITEM("separator");
    TOGGLE_ITEM(coordSystemMenuItems->items[1],
        "Bary Centered",   INERTIAL_B, editMenuPick);
    TOGGLE_ITEM(coordSystemMenuItems->items[2],
        "Big Primary Centered",    INERTIAL_S, editMenuPick);
    TOGGLE_ITEM(coordSystemMenuItems->items[3],
        "Small Primary Centered",  INERTIAL_E, editMenuPick);

    XtManageChildren(coordSystemMenuItems->items, 4);

    return pulldown;
}


////////////////////////////////////////////////////////////////////////
//
//  This creates the STYLE menu and all its items.
//
static Widget
buildStyleMenu(Widget menubar)
//
////////////////////////////////////////////////////////////////////////
{
    Widget  pulldown;
    Arg     args[5];
    int        n;

    styleMenuItems = new EditMenuItems;
    styleMenuItems->items = new Widget[5];   
    styleMenuItems->which = whichStyle;

// Tell motif to create the menu in the popup plane
    Arg popupargs[5];                         
    int popupn = 0;

    pulldown = XmCreatePulldownMenu(menubar, (char *)"styleMenu", popupargs, popupn);

    XtAddCallback(pulldown, XmNmapCallback,
        (XtCallbackProc) styleMenuDisplay, (XtPointer) styleMenuItems);

    n = 0;
    XtSetArg(args[n], XmNuserData, styleMenuItems); n++;
    XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
    TOGGLE_ITEM(styleMenuItems->items[0], "Line",    LINE, styleMenuPick);
    TOGGLE_ITEM(styleMenuItems->items[1], "Tube",    TUBE, styleMenuPick);
    TOGGLE_ITEM(styleMenuItems->items[2], "Surface", SURFACE, styleMenuPick);
    TOGGLE_ITEM(styleMenuItems->items[3], "Mesh Points", MESH_POINTS, styleMenuPick);
    TOGGLE_ITEM(styleMenuItems->items[4], "All Points", ALL_POINTS, styleMenuPick);

    XtManageChildren(styleMenuItems->items, 5);

    return pulldown;
}


////////////////////////////////////////////////////////////////////////
//
//  This creates the Coordinates menu and all its items.
//
static Widget
buildCoordMenu(Widget menubar)
//
////////////////////////////////////////////////////////////////////////
{
    Widget  pulldown;
    Arg     args[3];
    int        n;

    coordMenuItems = new EditMenuItems;
    coordMenuItems->items = new Widget[6];
    coordMenuItems->which = whichCoord;

    Arg popupargs[3];
    int popupn = 0;

    pulldown = XmCreatePulldownMenu(menubar, (char *)"coordMenu", popupargs, popupn);

    XtAddCallback(pulldown, XmNmapCallback,
        (XtCallbackProc) coordMenuDisplay, (XtPointer) coordMenuItems);

    n = 0;
    XtSetArg(args[n], XmNuserData, coordMenuItems); n++;
    XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
    TOGGLE_ITEM(coordMenuItems->items[0], "NONE",   NO_COORD, coordMenuPick);
    TOGGLE_ITEM(coordMenuItems->items[1], "Coord Center",    COORDORIGIN, coordMenuPick);
    TOGGLE_ITEM(coordMenuItems->items[2], "Left and Back ",   LEFTBACK, coordMenuPick);
    TOGGLE_ITEM(coordMenuItems->items[3], "Left and Ahead",   LEFTAHEAD, coordMenuPick);
    TOGGLE_ITEM(coordMenuItems->items[4], "At Origin",   COORD_AT_ORIGIN, coordMenuPick);
    SEP_ITEM("separator");
    TOGGLE_ITEM(coordMenuItems->items[5], "Draw Scale",   DRAW_TICKER, coordMenuPick);

    XtManageChildren(coordMenuItems->items, 6);

    return pulldown;
}


////////////////////////////////////////////////////////////////////////
//
//  This creates the TYPE menu and all its items.
//
static Widget
buildTypeMenu(Widget menubar)
//
////////////////////////////////////////////////////////////////////////
{
    Widget  pulldown;
    Arg     args[2];
    int        n;

    typeMenuItems = new EditMenuItems;
    typeMenuItems->items = new Widget[2];
    typeMenuItems->which = whichType;

// Tell motif to create the menu in the popup plane
    Arg popupargs[2];
    int popupn = 0;

    pulldown = XmCreatePulldownMenu(menubar, (char *)"typeMenu", popupargs, popupn);

    XtAddCallback(pulldown, XmNmapCallback,
        (XtCallbackProc) typeMenuDisplay, (XtPointer) typeMenuItems);

    n = 0;
    XtSetArg(args[n], XmNuserData, typeMenuItems); n++;
    XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
    TOGGLE_ITEM(typeMenuItems->items[0], "Solution",    SOLUTION, typeMenuPick);
    TOGGLE_ITEM(typeMenuItems->items[1], "Bifurcation", BIFURCATION, typeMenuPick);

    XtManageChildren(typeMenuItems->items, 2);

    return pulldown;
}


////////////////////////////////////////////////////////////////////////
//
//  This creates the pulldown menu bar and its menus.
//
static Widget
buildMenu(Widget parent)
//
////////////////////////////////////////////////////////////////////////
{
    Widget  menuButtons[7];
    Widget  pulldown2;
    Widget  pulldown1, pulldown3, pulldown4, pulldown5, pulldown6, pulldown7;
    Arg     args[8];
    int        n, m;

// menu bar
    Widget menubar = XmCreateMenuBar(parent, (char *)"menuBar", NULL, 0);
#ifndef LESSTIF_VERSION
    XtVaSetValues (menubar, XmNshadowThickness, 1, NULL);
#endif
    pulldown1 = buildFileMenu(menubar);
    if(useR3B)
        pulldown2 = buildCenterMenu(menubar);
    pulldown3 = buildStyleMenu(menubar);
    pulldown4 = buildTypeMenu(menubar);
    pulldown7 = buildCoordMenu(menubar);
    pulldown5 = buildOptionMenu(menubar);
    pulldown6 = buildHelpMenu(menubar);

#ifdef USE_BK_COLOR
// set the background color for the pull down menus.
    XtVaSetValues (pulldown1, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
    XtVaSetValues (pulldown2, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
    XtVaSetValues (pulldown3, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
    XtVaSetValues (pulldown4, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
    XtVaSetValues (pulldown5, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
    XtVaSetValues (pulldown6, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
#endif

// the text in the menubar for these menus
    n = 0; m=0;
    XtSetArg(args[n], XmNsubMenuId, pulldown1); n++;
    menuButtons[m] = XtCreateManagedWidget("File",
        xmCascadeButtonGadgetClass,
        menubar, args, n);
    ++m;

    n = 0;
    XtSetArg(args[n], XmNsubMenuId, pulldown4); n++;
    menuButtons[m] = XtCreateManagedWidget("Type",
        xmCascadeButtonGadgetClass,
        menubar, args, n);
    ++m;

    n = 0;
    XtSetArg(args[n], XmNsubMenuId, pulldown3); n++;
    menuButtons[m] = XtCreateManagedWidget("Style",
        xmCascadeButtonGadgetClass,
        menubar, args, n);
    ++m;

    n = 0;
    XtSetArg(args[n], XmNsubMenuId, pulldown7); n++;
    menuButtons[m] = XtCreateManagedWidget("Draw Coord",
        xmCascadeButtonGadgetClass,
        menubar, args, n);
    ++m;

    if(useR3B)
    {
        n = 0;
        XtSetArg(args[n], XmNsubMenuId, pulldown2); n++;
        menuButtons[m] = XtCreateManagedWidget("Center",
            xmCascadeButtonGadgetClass,
            menubar, args, n);
        ++m;
    }

    n = 0;
    XtSetArg(args[n], XmNsubMenuId, pulldown5); n++;
    menuButtons[m] = XtCreateManagedWidget("Options",
        xmCascadeButtonGadgetClass,
        menubar, args, n);
    ++m;

    n = 0;
    XtSetArg(args[n], XmNsubMenuId, pulldown6); n++;
    XtSetArg(args[n], XmNmenuHelpWidget, pulldown6); n++;
    menuButtons[m] = XtCreateManagedWidget("Help",
        xmCascadeButtonGadgetClass,
        menubar, args, n);
    ++m;

// manage the menu buttons
    XtManageChildren(menuButtons, m);

    return menubar;
}


////////////////////////////////////////////////////////////////////////
//
static void 
dimensionToggledCB(Widget w, XtPointer client_data, XtPointer cbs)
//
////////////////////////////////////////////////////////////////////////
{
    static bool buttonState = setShow3D;
    buttonState = !buttonState;
    if(buttonState)
    {
        setShow3D = true;
        XtSetSensitive (zAxisList, true);
        setListValue();
        XmString xString = XmStringCreateLocalized((char *)"3D");
        XtVaSetValues (w, XmNlabelString, xString, NULL);
        XmStringFree(xString);
    }
    else
    {
        setShow3D = false;
        zCoordIndices.clear();
        zCoordIndices.push_back(-1);
        XtSetSensitive (zAxisList, false);
        XmString xString = XmStringCreateLocalized((char *)"2D");
        XtVaSetValues (w, XmNlabelString, xString, NULL);
        XmStringFree(xString);
    }

    if(whichType != BIFURCATION)
        setShow3DSol = setShow3D;
    else
        setShow3DBif = setShow3D;

    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
static void 
createBdBoxCB(Widget w, XtPointer client_data, XtPointer cbs)
//
////////////////////////////////////////////////////////////////////////
{
    static bool btnState = false;
    SoSeparator * scene = (SoSeparator *) client_data;
    btnState = !btnState;
    if(btnState)
        scene->addChild(createBoundingBox());
    else
    {
        SoSeparator * bdBox = (SoSeparator *)SoNode::getByName("bdBox");
        scene->removeChild(bdBox);
    }
}


////////////////////////////////////////////////////////////////////////
//
//  This creates the main window contents. In this case, we have a
//  menubar at the top of the window, and a render area filling out
//  the remainder. These widgets are layed out with a Motif form widget.
//
static void
buildMainWindow(Widget parent, SoSeparator *root)
//
////////////////////////////////////////////////////////////////////////
{
    Arg  args[15];
    int  n,i;

    SoSeparator *sceneGraph = new SoSeparator;
    sceneGraph->ref();

    SoEventCallback *mouseEventCB = new SoEventCallback;
    sceneGraph->addChild(mouseEventCB);
    sceneGraph->addChild(root);

// build the toplevel widget
    topform = XtCreateWidget("topform", xmFormWidgetClass, parent,NULL, 0);
// build menubar
    Widget menubar = buildMenu(topform);
#ifdef USE_BK_COLOR
    XtVaSetValues (topform, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
    XtVaSetValues (menubar, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
#endif

// build carrier for the x, y, z, and label lists.
    Widget listCarrier= XtCreateWidget("ListCarrier",
        xmFormWidgetClass, topform, NULL, 0);
#ifdef USE_BK_COLOR
    XtVaSetValues (listCarrier, XtVaTypedArg,
        XmNbackground, XmRString, "white", 4,
        NULL);
#endif

//build the xAxis drop down list
    int nItems = xAxis.size();
    xList = (XmStringTable) XtMalloc(nItems * sizeof (XmString *));
    for (i = 0; i < nItems; i++)
        xList[i] = XmStringCreateLocalized (const_cast<char *>(xAxis[i].c_str()));

    xAxisList=XtVaCreateManagedWidget ("xAxis",
        xmComboBoxWidgetClass, listCarrier,
        XmNcomboBoxType,       XmDROP_DOWN_COMBO_BOX,
        XmNitemCount,          nItems,
        XmNitems,              xList,
        XmNselectedPosition,   xCoordIndices[0],
        XmNcolumns,            2, 
        XmNmarginHeight,       1,
        XmNpositionMode,       XmZERO_BASED,
        NULL);

    lessTifFixupComboBox(xAxisList, xList, nItems, 10, 60, 2);

// Add Callback function for the x-axis drop down list
    XtAddCallback (xAxisList, XmNselectionCallback,
        xListCallBack, NULL);

#ifdef USE_BK_COLOR
    XtVaSetValues (xAxisList, XtVaTypedArg,
        XmNbackground, XmRString, "white", 4,
        NULL);
#endif

// build the yAxis drop down list
    nItems = yAxis.size();
    yList = (XmStringTable) XtMalloc(nItems * sizeof (XmString *));
    for (i = 0; i < nItems; i++)
        yList[i] = XmStringCreateLocalized(const_cast<char *>(yAxis[i].c_str()));

    yAxisList=XtVaCreateManagedWidget ("yAxis",
        xmComboBoxWidgetClass, listCarrier,
        XmNcomboBoxType,       XmDROP_DOWN_COMBO_BOX,
        XmNitemCount,          nItems,
        XmNitems,              yList,
        XmNselectedPosition,   yCoordIndices[0],
        XmNcolumns,            2,
        XmNmarginHeight,       1,
        XmNpositionMode,       XmZERO_BASED,
        NULL);

    lessTifFixupComboBox(yAxisList, yList, nItems, 10, 60, 2);

    XtAddCallback (yAxisList, XmNselectionCallback,
        yListCallBack, NULL);

#ifdef USE_BK_COLOR
    XtVaSetValues (yAxisList, XtVaTypedArg,
        XmNbackground, XmRString, "white", 4,
        NULL);
#endif

// build the zAxis drop down list
    nItems = zAxis.size();
    zList = (XmStringTable) XtMalloc(nItems * sizeof (XmString *));
    for (i = 0; i < nItems; i++)
        zList[i] = XmStringCreateLocalized(const_cast<char *>(zAxis[i].c_str()));

    zAxisList=XtVaCreateManagedWidget ("zAxis",
        xmComboBoxWidgetClass, listCarrier,
        XmNcomboBoxType,       XmDROP_DOWN_COMBO_BOX,
        XmNitemCount,          nItems,
        XmNitems,              zList,
        XmNselectedPosition,   zCoordIndices[0],
        XmNcolumns,            2,
        XmNmarginHeight,       1,
        XmNpositionMode,       XmZERO_BASED,
        NULL);

    lessTifFixupComboBox(zAxisList, zList, nItems, 10, 60, 2);

// Add Callback function for the z-axis drop down list
    XtAddCallback (zAxisList, XmNselectionCallback,
        zListCallBack, NULL);

#ifdef USE_BK_COLOR
    XtVaSetValues (zAxisList, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
#endif

// build the LABELs drop down list
    nItems = labels.size();
    lblList = (XmStringTable) XtMalloc(nItems * sizeof (XmString *));
    for (i = 0; i < nItems; i++)
        lblList[i] = XmStringCreateLocalized (const_cast<char *>(labels[i].c_str()));

    labelsList=XtVaCreateManagedWidget ("Labels",
        xmComboBoxWidgetClass, listCarrier,
        XmNcomboBoxType,       XmDROP_DOWN_COMBO_BOX,
        XmNitemCount,          nItems,
        XmNitems,              lblList,
        XmNcolumns,            6,
        XmNmarginHeight,       1,
        XmNselectedPosition,   lblChoice[0]+LBL_OFFSET, //lblIndices[0],
        XmNpositionMode,       XmZERO_BASED,
        NULL);

    lessTifFixupComboBox(labelsList, lblList, nItems, 10, 86, 6);

    for (i = 0; i < nItems; i++)
        XmStringFree(lblList[i]);

// Add Callback function for the LABELs drop down list
    XtAddCallback (labelsList, XmNselectionCallback,
        lblListCallBack, NULL);

#ifdef USE_BK_COLOR
    XtVaSetValues (labelsList, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
    XtVaSetValues (labelsList, XtVaTypedArg,
        XmNforeground, XmRString, "red", 4,
        NULL);
#endif

// build the COLORING Method drop down list
    nItems = coloringMethodList.size();
    clrMethodList = (XmStringTable) XtMalloc(nItems * sizeof (XmString *));

    for (i = 0; i < nItems; ++i)
        clrMethodList[i] = XmStringCreateLocalized(const_cast<char *>(coloringMethodList[i].c_str()));

    colorMethodSeletionList=XtVaCreateManagedWidget ("coloringMethodlist",
        xmComboBoxWidgetClass, listCarrier,
        XmNcomboBoxType,       XmDROP_DOWN_COMBO_BOX,
        XmNitemCount,          nItems,
        XmNitems,              clrMethodList,
        XmNcolumns,            5,
        XmNmarginHeight,       1,
        XmNselectedPosition,   (coloringMethod < 0 ?
                coloringMethod+CL_SP_ITEMS : coloringMethod+specialColorItems),
        XmNpositionMode,       XmZERO_BASED,
        NULL);

    lessTifFixupComboBox(colorMethodSeletionList, clrMethodList, nItems, 10, 80, 5);

    XtAddCallback (colorMethodSeletionList, XmNselectionCallback,
        colorMethodSelectionCB, NULL);

// build the numPeriodAnimated drop down list
    nItems = 7;
    char (*numberPList)[5] = new char [nItems][5];
    XmStringTable numPList = (XmStringTable) XtMalloc(nItems * sizeof (XmString *));

    int iam = 1;
    sprintf(numberPList[0], "%i", 0);
    for (i = 0; i < nItems-2; ++i)
    {
        sprintf(numberPList[i+1], "%i", iam);
        iam *= 2;
    }
    sprintf(numberPList[nItems-1], "%s", "inf");

    for (i = 0; i < nItems; ++i)
        numPList[i] = XmStringCreateLocalized (numberPList[i]);

    if (numPeriodAnimated > 0)
        i = ((int)(log(numPeriodAnimated)/log(2.0))) + 1;
    else if (numPeriodAnimated == 0)
        i = 0;
    else
        i = nItems - 1;

    Widget numPeriodAnimatedList=XtVaCreateManagedWidget ("list",
        xmComboBoxWidgetClass, listCarrier,
        XmNcomboBoxType,       XmDROP_DOWN_COMBO_BOX,
        XmNitemCount,          nItems,
        XmNitems,              numPList,
        XmNcolumns,            3,
        XmNmarginHeight,       1,
        XmNselectedPosition,   i,
        XmNpositionMode,       XmZERO_BASED,
        NULL);

    lessTifFixupComboBox(numPeriodAnimatedList, numPList, nItems, 10, 70, 3);

// Add Callback function for the numberPeriodAnimated drop down list
    XtAddCallback (numPeriodAnimatedList, XmNselectionCallback,
        numPeriodAnimatedCB, NULL);

    for (i = 0; i < nItems; i++)
        XmStringFree(numPList[i]);
    XtFree((char *)numPList);
//    delete []numberPList;
//----------------------------------------------------------------> Nov 06 End

// create labels for the x, y, z, and labels drop down lists
    Widget xLbl = XtVaCreateManagedWidget("X",xmLabelWidgetClass, listCarrier, NULL);
    Widget yLbl = XtVaCreateManagedWidget("Y",xmLabelWidgetClass, listCarrier, NULL);
    Widget zLbl = XtVaCreateManagedWidget("Z",xmLabelWidgetClass, listCarrier, NULL);
    Widget lLbl = XtVaCreateManagedWidget("Label",xmLabelWidgetClass, listCarrier, NULL);
    Widget colorLbl = XtVaCreateManagedWidget("Color",xmLabelWidgetClass, listCarrier, NULL);
    Widget numPeriodLbl = XtVaCreateManagedWidget("Period",xmLabelWidgetClass, listCarrier, NULL);

    Widget orbitSldLbl = XtVaCreateManagedWidget("Orbit",xmLabelWidgetClass, listCarrier, NULL);
    Widget satSldLbl = XtVaCreateManagedWidget(useR3B ? "Sat " : "Anim",xmLabelWidgetClass, listCarrier, NULL);
    Widget spLbl   = XtVaCreateManagedWidget("   Line  ",xmLabelWidgetClass, listCarrier, NULL);
    Widget spLbl2  = XtVaCreateManagedWidget("Thickness",xmLabelWidgetClass, listCarrier, NULL);

#ifdef USE_BK_COLOR
//set the background color for the labels
    XtVaSetValues (xLbl, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
    XtVaSetValues (yLbl, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
    XtVaSetValues (zLbl, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
    XtVaSetValues (lLbl, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
#endif

// Create slider to control speed
    n = 0;
    XtSetArg(args[n], XmNminimum, MIN_SAT_SPEED); n++;
    XtSetArg(args[n], XmNmaximum, MAX_SAT_SPEED); n++;
    XtSetArg(args[n], XmNvalue, satSpeed*100);n++;//(MAX_SPEED-MIN_SPEED)/2.0); n++;
    XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
    if(options[OPT_SAT_ANI])
    {
        XtSetArg(args[n], XmNeditable, TRUE); n++;
    }
    else
    {
        XtSetArg(args[n], XmNeditable, FALSE); n++;
    }


    satAniSpeedSlider =
        XtCreateWidget("Speed", xmScaleWidgetClass, listCarrier, args, n);

#ifdef USE_BK_COLOR
    XtVaSetValues (satAniSpeedSlider, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
#endif

// Callbacks for the slider
    XtAddCallback(satAniSpeedSlider, XmNvalueChangedCallback,
        satSpeedCB, NULL);

    n = 0;
    XtSetArg(args[n], XmNminimum, MIN_ORBIT_SPEED); n++;
    XtSetArg(args[n], XmNmaximum, MAX_ORBIT_SPEED); n++;
    XtSetArg(args[n], XmNvalue, orbitSpeed*50);n++;
    XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
    if(options[OPT_PERIOD_ANI])
    {
        XtSetArg(args[n], XmNeditable, TRUE); n++;
    }
    else
    {
        XtSetArg(args[n], XmNeditable, FALSE); n++;
    }
    orbitAniSpeedSlider =
        XtCreateWidget("Speed2", xmScaleWidgetClass, listCarrier, args, n);

#ifdef USE_BK_COLOR
    XtVaSetValues (orbitAniSpeedSlider, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
#endif

// Callbacks for the slider2
    XtAddCallback(orbitAniSpeedSlider, XmNvalueChangedCallback,
        orbitSpeedCB, NULL);

// create spinbox for the line width control.
    n = 0;
    XtSetArg (args[n], XmNarrowSize, 12); n++;
    XtSetArg (args[n], XmNspinBoxChildType, XmNUMERIC); n++;
    XtSetArg (args[n], XmNminimumValue, 10); n++;
    XtSetArg (args[n], XmNdecimalPoints, 1); n++;
    XtSetArg (args[n], XmNincrementValue, 1); n++;
    XtSetArg (args[n], XmNeditable, TRUE); n++;
    XtSetArg (args[n], XmNpositionType, XmPOSITION_VALUE); n++;
    XtSetArg (args[n], XmNposition, lineWidthScaler+10); n++;
    XtSetArg (args[n], XmNcolumns,  3); n++;
    XtSetArg (args[n], XmNwrap, FALSE); n++;
#ifdef LESSTIF_VERSION
    XtSetArg (args[n], XmNmaximumValue, 110); n++;
    Widget spinBox = XmCreateSpinBox (listCarrier, (char *)"spinBox", args, 1);
    Widget tf = XmCreateTextField(spinBox, (char *)"tf", args+1, n-1);
    XtManageChild(tf);
    XtManageChild(spinBox);
    XtAddCallback(tf, XmNvalueChangedCallback,
        lineWidthCB, (XtPointer)tf);
#else
    XtSetArg (args[n], XmNmaximumValue, 100); n++;
    Widget spinBox = XmCreateSimpleSpinBox (listCarrier, (char *)"spinBox", args, n);

// Callbacks for the spinebox
    XtAddCallback(spinBox, XmNvalueChangedCallback,
        lineWidthCB, (XtPointer)spinBox);
#endif

// create RENDER AREA FOR THE graphics.
#ifdef USE_EXAM_VIEWER
    renderArea = new SoXtExaminerViewer(topform);
#else
    renderArea = new SoXtRenderArea(topform);
#endif

    renderArea->setSize(SbVec2s(winWidth, winHeight));
    renderArea->setBackgroundColor(envColors[0]);
    if(useR3B)
        renderArea->setTransparencyType(SoGLRenderAction::SORTED_OBJECT_BLEND);

#ifdef USE_EXAM_VIEWER
    n = 0;
    Widget newButton =  XmCreatePushButton(renderArea->getAppPushButtonParent(), (char *)"BOX", NULL, 0);
    XtAddCallback(newButton, XmNactivateCallback, createBdBoxCB, sceneGraph);
    renderArea->addAppPushButton(newButton);

    char xString[5];
    if(setShow3D)
    {
        XtSetSensitive (zAxisList, true);
        strcpy(xString,"3D");
    }
    else
    {
        strcpy(xString,"2D");
        XtSetSensitive (zAxisList, false);
    }

    dimButton =  XmCreatePushButton(renderArea->getAppPushButtonParent(), xString, NULL, 0);
    XtAddCallback(dimButton, XmNactivateCallback, dimensionToggledCB, sceneGraph);
    renderArea->addAppPushButton(dimButton);

// used for printing  scene to ps files
    vwrAndScene = new ViewerAndScene;
    vwrAndScene->scene  = renderArea->getSceneGraph();
    vwrAndScene->viewer = renderArea;
#endif

// layout
//Positioning the x-label for the x-axis drop down list
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopOffset,         3            ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomOffset,      3            ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNleftOffset,        5            ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_NONE); n++;
    XtSetValues(xLbl, args, n);

//Positioning the x-axis' drop down list
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_FORM  ); n++;
    XtSetArg(args[n], XmNtopOffset,         3              ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_FORM  ); n++;
    XtSetArg(args[n], XmNbottomOffset,      3              ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNleftWidget,        xLbl           ); n++;
    XtSetArg(args[n], XmNleftOffset,        5              ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_NONE  ); n++;
    XtSetValues(xAxisList, args, n);

//Positioning the y-label for the y-axis drop down list
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         xLbl                    ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,      xLbl                    ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        xAxisList               ); n++;
    XtSetArg(args[n], XmNleftOffset,        5                       ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_NONE           ); n++;
    XtSetValues(yLbl, args, n);

//Positioning the y-axis' drop down list
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         xAxisList               ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,      xAxisList               ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        yLbl                    ); n++;
    XtSetArg(args[n], XmNleftOffset,        5                       ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_NONE           ); n++;
    XtSetValues(yAxisList, args, n);

//Positioning the z-label for the z-axis drop down list
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         xLbl                    ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,      xLbl                    ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        yAxisList               ); n++;
    XtSetArg(args[n], XmNleftOffset,        5                       ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_NONE           ); n++;
    XtSetValues(zLbl, args, n);

//Positioning the z-axis' drop down list
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         xAxisList               ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,      xAxisList               ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        zLbl                    ); n++;
    XtSetArg(args[n], XmNleftOffset,        5                       ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_NONE           ); n++;
    XtSetValues(zAxisList, args, n);

//Positioning the LABELs' drop down list label
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         xLbl                    ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,      xLbl                    ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        zAxisList               ); n++;
    XtSetArg(args[n], XmNleftOffset,        5                       ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_NONE           ); n++;
    XtSetValues(lLbl, args, n);

//Positioning the LABELs' drop down list
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         xAxisList               ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,      xAxisList               ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        lLbl                    ); n++;
    XtSetArg(args[n], XmNleftOffset,        0                       ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_NONE           ); n++;
    XtSetValues(labelsList, args, n);

//Positioning the label for the Coloring method list
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         xLbl                    ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,      xLbl                    ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        labelsList              ); n++;
    XtSetArg(args[n], XmNleftOffset,        5                       ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_NONE           ); n++;
    XtSetValues(colorLbl, args, n);

// layout the coloring method selection on the listCarrier.
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         xAxisList               ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,      xAxisList               ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        colorLbl                ); n++;
    XtSetArg(args[n], XmNleftOffset,        0                       ); n++;
    XtSetValues(colorMethodSeletionList,args,n);
    XtManageChild(colorMethodSeletionList);

//Positioning the label for the number of period seleciton list
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         xLbl                    ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,      xLbl                    ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        colorMethodSeletionList ); n++;
    XtSetArg(args[n], XmNleftOffset,        5                       ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_NONE           ); n++;
    XtSetValues(numPeriodLbl, args, n);


// layout the numPeriodAnimatedList on the listCarrier.
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         xAxisList               ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,      xAxisList               ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
//labelsList              ); n++;
    XtSetArg(args[n], XmNleftWidget,        numPeriodLbl            ); n++;
    XtSetArg(args[n], XmNleftOffset,        0                       ); n++;
    XtSetValues(numPeriodAnimatedList,args,n);
    XtManageChild(numPeriodAnimatedList);

//  spinbox labels and spinboxes
// layout the spinBox label on the listCarrier.
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_FORM           ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
//    XtSetArg(args[n], XmNleftWidget,        colorMethodSeletionList ); n++;
    XtSetArg(args[n], XmNleftWidget,        numPeriodAnimatedList   ); n++;
    XtSetArg(args[n], XmNleftOffset,        5                       ); n++;
    XtSetValues(spLbl, args, n);

    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNtopWidget,         spLbl                   ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_FORM           ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
//    XtSetArg(args[n], XmNleftWidget,        colorMethodSeletionList ); n++;
//    XtSetArg(args[n], XmNleftWidget,        labelsList); n++;
    XtSetArg(args[n], XmNleftWidget,        numPeriodAnimatedList   ); n++;
    XtSetArg(args[n], XmNleftOffset,        5                       ); n++;
    XtSetValues(spLbl2, args, n);

// layout the spinBox on the listCarrier.
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_FORM           ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        spLbl                   ); n++;
    XtSetValues(spinBox, args, n);
    XtManageChild(spinBox);

//Positioning the label for the slider bars
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         xAxisList               ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        spinBox                 ); n++;
    XtSetArg(args[n], XmNleftOffset,        20                      ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_NONE           ); n++;
    XtSetValues(satSldLbl, args, n);

//Positioning the label for the slider bars
    n = 0;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_OPPOSITE_WIDGET ); n++;
    XtSetArg(args[n], XmNbottomWidget,      xAxisList                ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET          ); n++;
    XtSetArg(args[n], XmNleftWidget,        spinBox                  ); n++;
    XtSetArg(args[n], XmNleftOffset,        20                       ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_NONE            ); n++;
    XtSetValues(orbitSldLbl, args, n);

// Layout the slider bar on the listCarrier
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_FORM           ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        satSldLbl               ); n++;
    XtSetArg(args[n], XmNleftOffset,        5                       ); n++;
    XtSetValues(satAniSpeedSlider, args, n);
    XtManageChild(satAniSpeedSlider);

    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNtopWidget,         satAniSpeedSlider             ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_FORM           ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET         ); n++;
    XtSetArg(args[n], XmNleftWidget,        satSldLbl                 ); n++;
    XtSetArg(args[n], XmNleftOffset,        5                       ); n++;
    XtSetValues(orbitAniSpeedSlider, args, n);
    XtManageChild(orbitAniSpeedSlider);

// Positioning the menu bar.
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_NONE); n++;
    XtSetValues(menubar, args, n);

// Positioning the listCarrier.
    n = 0;
#ifdef LIST_UNDER_MENUBAR
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         menubar        ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_NONE); n++;
#else
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_FORM); n++;
#endif
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_FORM); n++;
    XtSetValues(listCarrier, args, n);

// Positioning the Render Area
    n = 0;
#ifdef LIST_UNDER_MENUBAR
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         xAxisList      ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_FORM  ); n++;
#else
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,         menubar        ); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,      xAxisList      ); n++;
#endif
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_FORM  ); n++;
    XtSetArg(args[n], XmNrightAttachment,   XmATTACH_FORM  ); n++;
    XtSetValues(renderArea->getWidget(), args, n);

// manage the children
    XtManageChild(menubar);
    XtManageChild(listCarrier);

    updateScene();
// these two lines are the third method for showing in 2D/3D
    renderArea->setSceneGraph(sceneGraph);
    renderArea->show();

    XtManageChild(topform);

#ifdef USE_BK_COLOR
    XtVaSetValues (mainWindow, XtVaTypedArg,
		   XmNbackground, XmRString, "white", 6, NULL);
#endif

    mouseEventCB->addEventCallback(
	    SoMouseButtonEvent::getClassTypeId(),
	    myMousePressCB,
            renderArea->getSceneManager()->getSceneGraph());
}


////////////////////////////////////////////////////////////////////////
//
//     When the line color changed, this function will be raised.
//
static void
lineColorValueChangedCB(Widget w, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XmSpinBoxCallbackStruct *sb = (XmSpinBoxCallbackStruct *) call_data;
    int value;
    int which;
    int lineNumber=0, columnNumber =0;

    if (sb->reason == XmCR_OK)
    {
        XtVaGetValues (sb->widget,  XmNuserData, &which,   NULL);
        XtVaGetValues (sb->widget,  XmNposition, &value,   NULL);
        lineNumber = which / 3;
        columnNumber = which % 3;
        lineColorTemp[lineNumber][columnNumber] = value/10.0;
    }
}


////////////////////////////////////////////////////////////////////////
//
//     When the line pattern selection changed, this function will be raised.
//
static void
linePatternValueChangedCB(Widget w, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XmSpinBoxCallbackStruct *sb = (XmSpinBoxCallbackStruct *) call_data;
    int position, which;

    if (sb->reason == XmCR_OK)
    {
        XtVaGetValues (w, XmNposition, &position, NULL);
        XtVaGetValues (w, XmNuserData, &which,   NULL);
        linePatternTemp[which] = systemLinePatternValue[position];
    }
}


////////////////////////////////////////////////////////////////////////
//
//  This creates the COLOR and LINE preference sheet stuff.
//
static Widget
createLineColorAndPatternPrefSheetGuts(Widget parent, char *name, int id)
//
////////////////////////////////////////////////////////////////////////
{
    Widget widgetList[7];
    Arg args[15];
    int n;

    Widget form = XtCreateWidget("", xmFormWidgetClass, parent, NULL, 0);

    n=0;
    XtSetArg(args[n], XmNcolumns, 12); n++;
    widgetList[0] = XtCreateWidget(name,
        xmLabelGadgetClass, form, args, n);
    XtManageChild (widgetList[0]);

    Widget spin = XmCreateSpinBox (form, (char *)"spin", NULL, 0);
    XtAddCallback (spin, XmNvalueChangedCallback, lineColorValueChangedCB, NULL);

// Create the red field
    n = 0;
    XtSetArg (args[n], XmNspinBoxChildType, XmNUMERIC) ; n++;
    XtSetArg (args[n], XmNcolumns,          3        ) ; n++;
    XtSetArg (args[n], XmNeditable,         FALSE    ) ; n++;
    XtSetArg (args[n], XmNminimumValue,     0        ) ; n++;
    XtSetArg (args[n], XmNmaximumValue,     10       ) ; n++;
    XtSetArg (args[n], XmNdecimalPoints,    1        ) ; n++;
    XtSetArg (args[n], XmNposition, lineColor[id][0]*10) ; n++;
    XtSetArg (args[n], XmNwrap,             TRUE     ) ; n++;
    XtSetArg (args[n], XmNuserData,         id*3     ) ; n++;

    widgetList[1]= XmCreateTextField (spin, (char *)"redText", args, n);
    XtManageChild (widgetList[1]);

// Create the green field
    n = 0;
    XtSetArg (args[n], XmNspinBoxChildType,XmNUMERIC); n++;
    XtSetArg (args[n], XmNcolumns,         3        ); n++;
    XtSetArg (args[n], XmNeditable,        FALSE    ); n++;
    XtSetArg (args[n], XmNminimumValue,    0        ); n++;
    XtSetArg (args[n], XmNmaximumValue,    10       ); n++;
    XtSetArg (args[n], XmNdecimalPoints,   1        ); n++;
    XtSetArg (args[n], XmNwrap,            TRUE     ); n++;
    XtSetArg (args[n], XmNposition, lineColor[id][1]*10); n++;
    XtSetArg (args[n], XmNuserData,        id*3+1  ); n++;

    widgetList[2]= XmCreateTextField (spin, (char *)"greenText", args, n);
    XtManageChild (widgetList[2]);

// Create the blue field
    n = 0;
    XtSetArg (args[n], XmNspinBoxChildType,XmNUMERIC); n++;
    XtSetArg (args[n], XmNcolumns,         3        ); n++;
    XtSetArg (args[n], XmNeditable,        FALSE    ); n++;
    XtSetArg (args[n], XmNminimumValue,    0        ); n++;
    XtSetArg (args[n], XmNmaximumValue,    10       ); n++;
    XtSetArg (args[n], XmNdecimalPoints,   1        ); n++;
    XtSetArg (args[n], XmNwrap,            TRUE     ); n++;
    XtSetArg (args[n], XmNposition, lineColor[id][2]*10); n++;
    XtSetArg (args[n], XmNuserData,        id*3+2   ); n++;

    widgetList[3]= XmCreateTextField (spin, (char *)"blueText", args, n);
    XtManageChild (widgetList[3]);

// create the line pattern
    int lengthOfSysPatternArray = XtNumber( systemLinePatternLookAndFeel );
    XmStringTable strList = (XmStringTable) XtMalloc (
        (unsigned) lengthOfSysPatternArray * sizeof (XmString *));
    for (int i = 0; i < lengthOfSysPatternArray; i++)
      strList[i] = XmStringCreateLocalized ((char *)systemLinePatternLookAndFeel[i]);

    n = 0;
    XtSetArg (args[n], XmNspinBoxChildType,  XmSTRING); n++;
    XtSetArg (args[n], XmNcolumns,           12        ) ; n++;
    XtSetArg (args[n], XmNeditable,          FALSE); n++;
    XtSetArg (args[n], XmNnumValues,         lengthOfSysPatternArray); n++;
    XtSetArg (args[n], XmNvalues,            strList); n++;
    XtSetArg (args[n], XmNwrap, TRUE); n++;
    XtSetArg (args[n], XmNshadowThickness, 0); n++;
    XtSetArg (args[n], XmNuserData,        id   ); n++;

#ifdef LESSTIF_VERSION
    Widget lpSpinBox = XmCreateSpinBox (form, (char *)"lpSimple", args, 0);
    Widget tf = XmCreateTextField(lpSpinBox, (char *)"tf", args, n);
    XtManageChild(tf);
    XtAddCallback(tf, XmNvalueChangedCallback,
                  linePatternValueChangedCB, (XtPointer)tf);
#else
    Widget lpSpinBox = XmCreateSimpleSpinBox (form, (char *)"lpsimple", args, n);
    XtAddCallback (lpSpinBox, XmNvalueChangedCallback, linePatternValueChangedCB, NULL);
#endif

    for (int i = 0; i < lengthOfSysPatternArray; i++)
        XmStringFree (strList[i]);
    delete strList;

// layout
    n = 0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopOffset,     8            ); n++;
    XtSetValues(widgetList[0], args, n);

    n = 0;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNleftWidget,     widgetList[0]  ); n++;
    XtSetArg(args[n], XmNleftOffset,     10             ); n++;
    XtSetValues(spin, args, n);

    n = 0;
    XtSetArg(args[n], XmNrightAttachment,  XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightOffset,      10             ); n++;
    XtSetArg(args[n], XmNleftAttachment,   XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNleftWidget,       spin  ); n++;
    XtSetArg(args[n], XmNleftOffset,       30             ); n++;
    XtSetValues(lpSpinBox, args, n);

    XtManageChild (spin);
    XtManageChild (lpSpinBox);
    XtManageChild (form);

    return form;
}


////////////////////////////////////////////////////////////////////////
//
static Widget 
createColorAndLinePrefSheetHeader(Widget parent)
//
////////////////////////////////////////////////////////////////////////
{
    Widget widget;
    Arg args[6];
    int n;

// create a form to hold verything together
    Widget form = XtCreateWidget("", xmFormWidgetClass,
        parent, NULL, 0);

// create the first line
    n=0;
    XtSetArg(args[n], XmNtopAttachment,     XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopOffset,         20           ); n++;
    XtSetArg(args[n], XmNleftAttachment,    XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomOffset,      10           ); n++;
    widget = XtCreateWidget("|PT TYPE| RED  GREEN BLUE   |    LINE PATTERN     |",
        xmLabelGadgetClass, form, args, n);

    XtManageChild(widget);

    return form;
}


///////////////////////////////////////////////////////////////////////
//
//  This simply creates the default parts of the pref dialog.
//
static void
createLineAttrPrefSheetParts(Widget widgetList[], int &num, Widget form, const char** name)
//
////////////////////////////////////////////////////////////////////////
{
    for(int i=0; i<NUM_SP_POINTS; ++i)
    {
        linePatternTemp[i] = linePattern[i];
        linePatternOld[i]  = linePattern[i];
        for(int j=0; j<3; ++j)
        {
            lineColorTemp[i][j] = lineColor[i][j];
            lineColorOld[i][j]  = lineColor[i][j];
        }
    }

    for(int i=0; i<NUM_SP_POINTS; ++i)
        widgetList[num++] = createLineColorAndPatternPrefSheetGuts(form, (char *)name[i], i);
}


////////////////////////////////////////////////////////////////////////
//
//  Given a widget list for the preference sheet and it's lenght
//  lay them out one after the other and manage them all. The dialog
//  is them mapped onto the screen.
//
static void
layoutLineAttrPartsOnThePrefSheet(Widget widgetList[],
int num, Widget form)
//
////////////////////////////////////////////////////////////////////////
{
    Arg args[12];
    int n;

// layout
    n = 0;
    XtSetArg(args[n], XmNleftAttachment,        XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopAttachment,         XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopOffset,         20             ); n++;
    XtSetValues(widgetList[0], args, n);

    n = 0;
    XtSetArg(args[n], XmNtopAttachment,         XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopOffset,         20             ); n++;
    XtSetArg(args[n], XmNrightAttachment,       XmATTACH_FORM); n++;
    XtSetValues(widgetList[1], args, n);

    for (int i=2; i<num; i++)
    {
        n = 0;
        XtSetArg(args[n], XmNtopAttachment,     XmATTACH_WIDGET); n++;
        XtSetArg(args[n], XmNtopWidget,         widgetList[i-2]); n++;
        if(i%2 == 0)
        {
            XtSetArg(args[n], XmNleftAttachment,        XmATTACH_FORM); n++;
        }
        else
        {
            XtSetArg(args[n], XmNrightAttachment,       XmATTACH_FORM); n++;
        }
        if (i == (num - 1) )
        {
            XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_FORM); n++;
        }
        XtSetValues(widgetList[i], args, n);
    }

    XtManageChildren(widgetList, num);
}


////////////////////////////////////////////////////////////////////////
//
static void
createPreferShellAndPanedWindow(Widget &dialog_shell, Widget &panedWin)
//
////////////////////////////////////////////////////////////////////////
{
    Arg args[5];
    unsigned char modality = (unsigned char)XmDIALOG_PRIMARY_APPLICATION_MODAL;

    int n=0;
    XtSetArg(args[n], XmNdialogStyle, modality); ++n;
    XtSetArg(args[n], XmNresizePolicy, XmRESIZE_NONE); ++n;
    XtSetArg(args[n], XmNnoResize, TRUE); ++n; 
    dialog_shell = XmCreateDialogShell(topform, (char *)"Preference Dialog", args, n);

    n=0;
    XtSetArg(args[n], XmNsashWidth, 2); ++n;
    XtSetArg(args[n], XmNsashHeight, 2); ++n;
    XtSetArg(args[n], XmNmarginHeight, 12); ++n;
    XtSetArg(args[n], XmNmarginWidth, 12); ++n;
    panedWin = XmCreatePanedWindow(dialog_shell, (char *)"pane",args,n);

}


////////////////////////////////////////////////////////////////////////
//
static void
createPreferActionFormControls(Widget &parent)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[13];

    Widget saveBtn, closeBtn, applyBtn, cancelBtn;

    n=0;
    Widget form = XmCreateForm(parent, (char *)"control form", args, n);

    n=0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); ++n;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); ++n;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); ++n;
    XtSetArg(args[n], XmNleftOffset, 100); ++n;
    saveBtn = XmCreatePushButton(form, (char *)" Save ", args, n);
    XtManageChild (saveBtn);
    XtAddCallback (saveBtn, XmNactivateCallback, savePreferAndUpdateScene, parent );

    n=0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); ++n;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); ++n;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); ++n;
    XtSetArg(args[n], XmNrightOffset, 50); ++n;
    cancelBtn = XmCreatePushButton(form, (char *)" Cancel ", args, n);
    XtManageChild (cancelBtn);
    XtAddCallback (cancelBtn, XmNactivateCallback, closePreferDialogAndGiveUpChange, parent);

    n=0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); ++n;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); ++n;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); ++n;
    XtSetArg(args[n], XmNleftWidget, saveBtn); ++n;
    XtSetArg(args[n], XmNleftOffset, 50); ++n;
    closeBtn = XmCreatePushButton(form, (char *)" Update ", args, n);
    XtManageChild (closeBtn);
    XtAddCallback (closeBtn, XmNactivateCallback, closePreferDialogAndUpdateScene, parent );

    n=0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); ++n;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); ++n;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); ++n;
    XtSetArg(args[n], XmNleftWidget, closeBtn); ++n;
    XtSetArg(args[n], XmNleftOffset, 50); ++n;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); ++n;
    XtSetArg(args[n], XmNrightWidget, cancelBtn); ++n;
    XtSetArg(args[n], XmNrightOffset, 50); ++n;
    applyBtn = XmCreatePushButton(form, (char *)" Apply ", args, n);
    XtManageChild (applyBtn);
    XtAddCallback (applyBtn, XmNactivateCallback, applyPreferDialogChangeAndUpdateScene, parent );

    XtManageChild (form);
}


////////////////////////////////////////////////////////////////////////
//
static Widget
createPreferDefaultPageFrames(Widget parent, const char *frameName)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[12];
    Widget frame, label;

// create frame for group color settings.
    n = 0;
    XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
    frame = XmCreateFrame(parent, (char *)"frame", args, n);

    n = 0;
    XtSetArg(args[n], XmNframeChildType, XmFRAME_TITLE_CHILD); n++;
    XtSetArg(args[n], XmNchildVerticalAlignment, XmALIGNMENT_CENTER); n++;
    label = XmCreateLabelGadget (frame, (char *)frameName, args, n);
    XtManageChild (label);
    return frame;
}


////////////////////////////////////////////////////////////////////////
//
static void
graphCoordinateSystemToggledCB(Widget widget, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    int which = (long) client_data;
    XmToggleButtonCallbackStruct *state = (XmToggleButtonCallbackStruct *) call_data;

    if (state->set == XmSET)
        whichCoordSystemTemp = which;
    else
        whichCoordSystemTemp = 0;
}


////////////////////////////////////////////////////////////////////////
//
static void
createGraphCoordinateSystemFrameGuts(Widget frame)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[12];
    const char *coordSysItems[]=
    {
        "Rotating Frame", "Barycenter " ,
        "Large Primary Center", "Small Primary Center"
    };

// create default selections
    n = 0;
    XtSetArg (args[n], XmNpacking, XmPACK_COLUMN); ++n;
    XtSetArg (args[n], XmNnumColumns, 4); ++n;
    Widget toggleBox = XmCreateRadioBox(frame, (char *)"radio", args, n);

    whichCoordSystemOld  = whichCoordSystem;
    whichCoordSystemTemp = whichCoordSystem;

    for (unsigned int i = 0; i < XtNumber (coordSysItems); i++)
    {
        n = 0;
        if ((unsigned long)whichCoordSystem == (unsigned long)i) XtSetArg(args[n++], XmNset, XmSET);
        Widget w = XmCreateToggleButtonGadget (toggleBox, (char *)coordSysItems[i], args, n);
        XtAddCallback (w, XmNvalueChangedCallback, graphCoordinateSystemToggledCB, (XtPointer) i);
        XtManageChild (w);
    }

    XtManageChild (toggleBox);
    XtManageChild (frame);
}

////////////////////////////////////////////////////////////////////////
//
static void
graphStyleWidgetToggledCB(Widget widget, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    int which = (long) client_data;
    XmToggleButtonCallbackStruct *state = (XmToggleButtonCallbackStruct *) call_data;

    if (state->set == XmSET)
        whichStyleTemp = which;
    else
        whichStyleTemp = 0;
}


////////////////////////////////////////////////////////////////////////
//
static void
createGraphStyleFrameGuts(Widget frame)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[12];
    const char * graphStyleItems[]=
    {
        "Line Style", "Tube Style" , "Surface Style"
    };

// create default selections
    n = 0;
    XtSetArg (args[n], XmNpacking, XmPACK_COLUMN); ++n;
    XtSetArg (args[n], XmNnumColumns, 4); ++n;
    Widget toggleBox = XmCreateRadioBox(frame, (char *)"togglebox", args, n);
    whichStyleOld  = whichStyle;
    whichStyleTemp = whichStyle;

    for (unsigned int i = 0; i < XtNumber (graphStyleItems); i++)
    {
        n = 0;
        if ((unsigned int)whichStyle == i) XtSetArg(args[n++], XmNset, XmSET);
        Widget w = XmCreateToggleButtonGadget (toggleBox, (char *)graphStyleItems[i], args, n);
        XtAddCallback (w, XmNvalueChangedCallback, graphStyleWidgetToggledCB, (XtPointer) i);
        XtManageChild (w);
    }

    XtManageChild (toggleBox);
    XtManageChild (frame);
}


////////////////////////////////////////////////////////////////////////
//
static void
graphTypeWidgetToggledCB(Widget widget, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    int which = (long) client_data;
    XmToggleButtonCallbackStruct *state = (XmToggleButtonCallbackStruct *) call_data;

    if (state->set == XmSET)
        whichTypeTemp = which;
    else
        whichTypeTemp = 0;
}


////////////////////////////////////////////////////////////////////////
//
static void
graphCoordWidgetToggledCB(Widget widget, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    int which = (long) client_data;
    XmToggleButtonCallbackStruct *state = (XmToggleButtonCallbackStruct *) call_data;

    if (state->set == XmSET)
        whichCoordTemp = which;
    else
        whichCoordTemp = 0;
}


////////////////////////////////////////////////////////////////////////
//
static void
createGraphTypeFrameGuts(Widget frame)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[12];
    const char * graphTypeItems[]={"Solution Diagram", "Bifurcation Diagram" };

// create default selections
    n = 0;
    XtSetArg (args[n], XmNpacking, XmPACK_COLUMN); ++n;
    XtSetArg (args[n], XmNnumColumns, 3); ++n;
    XtSetArg (args[n], XmNindicatorType, XmONE_OF_MANY_ROUND); ++n;
    XtSetArg (args[n], XmNindicatorOn, XmINDICATOR_CHECK); ++n;
    Widget toggleBox = XmCreateRadioBox(frame, (char *)"radiobox", args, n);

    whichTypeOld  = whichType;
    whichTypeTemp = whichType;

    for (unsigned int i = 0; i < XtNumber (graphTypeItems); i++)
    {
        n = 0;
        if ((unsigned int)whichType == i) XtSetArg(args[n++], XmNset, XmSET);
        Widget w = XmCreateToggleButtonGadget (toggleBox, (char *)graphTypeItems[i], args, n);
        XtAddCallback (w, XmNvalueChangedCallback, graphTypeWidgetToggledCB, (XtPointer) i);
        XtManageChild (w);
    }

    XtManageChild (toggleBox);
    XtManageChild (frame);
}


////////////////////////////////////////////////////////////////////////
//
// callback for all ToggleButtons.
//
static void
defaultGraphWidgetToggledCB( Widget widget, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    int bit = (long) client_data;
    XmToggleButtonCallbackStruct *toggle_data = (XmToggleButtonCallbackStruct *) call_data;

    if (toggle_data->set == XmSET)
        graphWidgetToggleSetTemp |= (1 << bit);
    else
        graphWidgetToggleSetTemp &= ~(1 << bit);
}


////////////////////////////////////////////////////////////////////////
//
static void
createOptionFrameGuts(Widget frame)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[12];

// create default selections
    n = 0;
    XtSetArg (args[n], XmNpacking, XmPACK_COLUMN); ++n;
    XtSetArg (args[n], XmNnumColumns, 4); ++n;
    Widget toggleBox = XmCreateRowColumn (frame, (char *)"togglebox", args, n);

    for (unsigned int i = 0; i < XtNumber (graphWidgetItems); i++)
    {
        if (!useR3B && (i == OPT_PRIMARY || i == OPT_LIB_POINTS)) continue;
        graphWidgetToggleSetOld = graphWidgetToggleSet;
        graphWidgetToggleSetTemp= graphWidgetToggleSet;
        n = 0;
        if (graphWidgetToggleSet & (1<<i)) XtSetArg(args[n++], XmNset, XmSET);
        Widget w = XmCreateToggleButtonGadget (toggleBox, (char *)graphWidgetItems[i], args, n);
        XtAddCallback (w, XmNvalueChangedCallback, defaultGraphWidgetToggledCB, (XtPointer) i);
        XtManageChild (w);
    }

    XtManageChild (toggleBox);
    XtManageChild (frame);
}

////////////////////////////////////////////////////////////////////////
//
static void
layoutPreferDefaultPageFrames(Widget widgetList[], int num)
//
////////////////////////////////////////////////////////////////////////
{
    Arg args[12];
    int n = 0;

// layout
    XtSetArg(args[n], XmNleftAttachment,        XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment,       XmATTACH_FORM); n++;

    for (int i=0; i<num; i++)
    {
        int m = n;
        if(i==0)
        {
            XtSetArg(args[m], XmNtopAttachment,     XmATTACH_FORM); m++;
            XtSetArg(args[m], XmNtopOffset,         20             ); m++;
        }
        else
        {
            XtSetArg(args[m], XmNtopAttachment,     XmATTACH_WIDGET); m++;
            XtSetArg(args[m], XmNtopWidget,         widgetList[i-1]); m++;
        }

        if (i == (num - 1) )
        {
            XtSetArg(args[m], XmNbottomAttachment,  XmATTACH_FORM); m++;
        }
        XtSetValues(widgetList[i], args, m);
    }

    XtManageChildren(widgetList, num);
}


////////////////////////////////////////////////////////////////////////
//
static void
createGraphCoordPartsFrameGuts(Widget frame)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[12];
    const char *coordItems[]=
    {
        "No Coordinate", "At Origin" ,
        "At Left & Behind", "At Left & Ahead"  
    };

// create default selections
    n = 0;
    XtSetArg (args[n], XmNpacking, XmPACK_COLUMN); ++n;
    XtSetArg (args[n], XmNnumColumns, 4); ++n;
    Widget toggleBox1 = XmCreateRadioBox(frame, (char *)"radio", args, n);

    whichCoordOld  = whichCoord;
    whichCoordTemp = whichCoord;

    for (unsigned int i = 0; i < XtNumber (coordItems); i++)
    {
        n = 0;
        if ((unsigned long)whichCoord == (unsigned long)i) XtSetArg(args[n++], XmNset, XmSET);
        Widget w = XmCreateToggleButtonGadget (toggleBox1, (char *)coordItems[i], args, n);
        XtAddCallback (w, XmNvalueChangedCallback, graphCoordWidgetToggledCB, (XtPointer) i);
        XtManageChild (w);
    }

    XtManageChild (toggleBox1);
    XtManageChild (frame);
}


////////////////////////////////////////////////////////////////////////
//
static void
createPreferDefaultPages(Widget parent)
//
////////////////////////////////////////////////////////////////////////
{
    Widget frameList[6];
    int num;
    const char * frmNames[]=
    {
        "Optional Widgets", "Graph Type", "Graph Style",
        "Coordinate System", "Coordinate Parts", "Others"
    };

    unsigned j=0;
    for(unsigned int i=0; i<XtNumber(frmNames); ++i)
    {
        if(!useR3B && i==3)continue;
        frameList[j] = createPreferDefaultPageFrames(parent, frmNames[i]);
        j++;
    }
    num = 0;
    createOptionFrameGuts(frameList[num++]);
    createGraphTypeFrameGuts(frameList[num++]);
    createGraphStyleFrameGuts(frameList[num++]);
    if(useR3B)
        createGraphCoordinateSystemFrameGuts(frameList[num++]);
    createGraphCoordPartsFrameGuts(frameList[num++]);
    layoutPreferDefaultPageFrames(frameList, num);
}


////////////////////////////////////////////////////////////////////////
//
static void
createLineAttPages(Widget parent)
//
////////////////////////////////////////////////////////////////////////
{
    const char *names[] =
    {
        "DEFAULTS", "BP (ALG)", "LP (ALG)",
        "HB      ", "UZ     4", "UZ    -4",
        "LP (DIF)", "BP (DIF)", "PD      ",
        "TR TORUS", "EP (NOR)", "MX (ABN)",
        "OTHERS  "
    };
    const char *names2[] =
    {
        "Color 1", "Color 2", "Color 3",
        "Color 4", "Color 5", "Color 6",
        "Color 7", "Color 8", "Color 9",
        "Color 10", "Color 11", "Color 12",
        "Color 13"
    };

    Widget widgetList[20];

    int num = 0;
    widgetList[num++] = createColorAndLinePrefSheetHeader(parent);
    widgetList[num++] = createColorAndLinePrefSheetHeader(parent);
    if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
        createLineAttrPrefSheetParts(widgetList, num, parent, names2);
    else
        createLineAttrPrefSheetParts(widgetList, num, parent, names);

    layoutLineAttrPartsOnThePrefSheet(widgetList, num, parent);
}


///////////////////////////////////////////////////////////////////////
//
static void
createPreferNotebookPages(Widget notebook)
//
////////////////////////////////////////////////////////////////////////
{
// create the preference sheet shell and form widget
    Widget pageForm[2], tab;
    int n=0;
    Arg          args[14];
    const char *tabName[] = { "Menu Item Preferences", "Line Attributes" };

// create the first page.
    n = 0;
    XtSetArg(args[n], XmNmarginHeight, 15); ++n;
    XtSetArg(args[n], XmNmarginWidth,  15); ++n;
    pageForm[0] = XmCreateForm(notebook, (char *)"page", args, n);

    n = 0;
    XtSetArg (args[n], XmNnotebookChildType, XmMINOR_TAB); n++;
    tab = XmCreatePushButton (notebook, (char *)tabName[0], args, n);

    createPreferDefaultPages(pageForm[0]);
    XtManageChild (tab);

// create the second page.
    n = 0;
    XtSetArg(args[n], XmNmarginHeight, 15); ++n;
    XtSetArg(args[n], XmNmarginWidth,  15); ++n;
    pageForm[1] = XmCreateForm(notebook, (char *)"page", args, n);

    n=0;
    XtSetArg (args[n], XmNnotebookChildType, XmMINOR_TAB); n++;
    tab = XmCreatePushButton (notebook, (char *)tabName[1], args, n);
    createLineAttPages(pageForm[1]);
    XtManageChild (tab);

    XtManageChildren (pageForm, 2);
}


////////////////////////////////////////////////////////////////////////
//
static void
createPreferNotebookAndActionForm(Widget parentPane, Widget &notebook, Widget &actionForm)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[15];

// create notebook to hold all the pages
    n = 0;
    XtSetArg(args[n], XmNmarginHeight, 15); ++n;
    XtSetArg(args[n], XmNmarginWidth, 10); ++n;
    XtSetArg (args[n], XmNunitType, Xm1000TH_INCHES); n++;
    XtSetArg (args[n], XmNwidth,  8900); n++;
    XtSetArg (args[n], XmNheight, 4800); n++;
    XtSetArg (args[n], XmNbackPagePlacement, XmTOP_RIGHT); n++;
    XtSetArg (args[n], XmNorientation,      XmHORIZONTAL); n++;
    XtSetArg (args[n], XmNresizePolicy,    XmRESIZE_NONE); n++;
    notebook = XmCreateNotebook (parentPane, (char *)"Options", args, n);

    n=0;
    XtSetArg (args[n], XmNpaneMinimum, 25); n++;
    XtSetArg (args[n], XmNpaneMaximum, 35); n++;
    XtSetArg (args[n], XmNresizePolicy,    XmRESIZE_NONE); n++;

    actionForm = XmCreateForm(parentPane, (char *)"action form", args, n);
}


////////////////////////////////////////////////////////////////////////
//
//  This creates the preference sheet in a separate window. It
//  calls other routines to create the actual content of the sheet.
//
static void
createPreferDialog()
//
////////////////////////////////////////////////////////////////////////
{
    static Widget shell;

//    if(!shell)
    {
        Widget notebook, actionForm, panedWin;
        createPreferShellAndPanedWindow(shell, panedWin);
        createPreferNotebookAndActionForm(panedWin, notebook, actionForm);
        createPreferNotebookPages(notebook);
        createPreferActionFormControls(actionForm);

        XtManageChild (notebook);
        XtManageChild (actionForm);
        XtManageChild (panedWin);
    }
    XtManageChild (shell);
}


///////////////////////////////////////////////////////////////////
//         CANCEL CALL BACK
//
static void
closePreferDialogAndGiveUpChange(Widget widget, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    Widget form = (Widget) client_data;
    for(int i=0; i<NUM_SP_POINTS; ++i)
    {
        linePatternTemp[i] = linePatternOld[i];
        linePattern[i]     = linePatternOld[i];
        for(int j=0; j<3; ++j)
        {
            lineColorTemp[i][j] = lineColorOld[i][j];
            lineColor[i][j]     = lineColorOld[i][j];
        }
    }

    whichType     = whichTypeOld;
    whichTypeTemp = whichTypeOld;
    typeMenuItems->which            = whichTypeOld;

    whichStyle     = whichStyleOld;
    whichStyleTemp = whichStyleOld;
    styleMenuItems->which            = whichStyleOld;

    if (useR3B)
    {
        whichCoordSystem     = whichCoordSystemOld;
        whichCoordSystemTemp = whichCoordSystemOld;
        coordSystemMenuItems->which           = whichCoordSystemOld;
    }

    whichCoord     = whichCoordOld;
    whichCoordTemp = whichCoordOld;
    coordMenuItems->which           = whichCoordOld;

// cancel the selections and recover the original values.
    graphWidgetToggleSetTemp = graphWidgetToggleSetOld;
    graphWidgetToggleSet     = graphWidgetToggleSetOld;
    for (unsigned int i = 0; i < XtNumber (graphWidgetItems); i++)
    {
        options[i] = (graphWidgetToggleSetOld & (1<<i)) ? true : false;
    }

    if(options[OPT_SAT_ANI])
        XtVaSetValues (satAniSpeedSlider, XmNeditable, TRUE, NULL);
    else
        XtVaSetValues (satAniSpeedSlider, XmNeditable, FALSE, NULL);

    if(options[OPT_PERIOD_ANI])
    {
        XtVaSetValues (orbitAniSpeedSlider, XmNeditable, TRUE, NULL);
    }
    else
    {
        XtVaSetValues (orbitAniSpeedSlider, XmNeditable, FALSE, NULL);
    }

    XtDestroyWidget (XtParent(XtParent(form)));
    updateScene();
}


///////////////////////////////////////////////////////////////////
//         OK & CLOSE CALL BACK
//
static void
closePreferDialogAndUpdateScene(Widget widget, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    Widget form = (Widget) client_data;
    for(int i=0; i<NUM_SP_POINTS; ++i)
    {
        linePattern[i]    = linePatternTemp[i];
        linePatternOld[i] = linePatternTemp[i];
        for(int j=0; j<3; ++j)
        {
            lineColor[i][j]    = lineColorTemp[i][j];
            lineColorOld[i][j] = lineColorTemp[i][j];
        }
    }

    whichType    = whichTypeTemp;
    whichTypeOld = whichTypeTemp;
    typeMenuItems->which           = whichTypeTemp;

    whichStyle    = whichStyleTemp;
    whichStyleOld = whichStyleTemp;
    styleMenuItems->which           = whichStyleTemp;

    if (useR3B)
    {
        whichCoordSystem    = whichCoordSystemTemp;
        whichCoordSystemOld = whichCoordSystemTemp;
        coordSystemMenuItems->which          = whichCoordSystemTemp;
    }

    whichCoord = whichCoordTemp;
    whichCoordOld = whichCoordTemp;
    coordMenuItems->which        = whichCoordTemp;

    graphWidgetToggleSet    = graphWidgetToggleSetTemp;
    graphWidgetToggleSetOld = graphWidgetToggleSetTemp;
    for (unsigned int i = 0; i < XtNumber (graphWidgetItems); i++)
    {
        options[i] = (graphWidgetToggleSetTemp & (1<<i)) ? true : false;
    }

    if(options[OPT_SAT_ANI])
        XtVaSetValues (satAniSpeedSlider, XmNeditable, TRUE, NULL);
    else
        XtVaSetValues (satAniSpeedSlider, XmNeditable, FALSE, NULL);

    if(options[3])
    {
        XtVaSetValues (orbitAniSpeedSlider, XmNeditable, TRUE, NULL);
    }
    else
    {
        XtVaSetValues (orbitAniSpeedSlider, XmNeditable, FALSE, NULL);
    }

    updateScene();

    XtDestroyWidget (XtParent(XtParent(form)));
}


///////////////////////////////////////////////////////////////////
//         OK & SAVE CALL BACK
//
static void
savePreferAndUpdateScene(Widget widget, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    Widget form = (Widget) client_data;
    for(int i=0; i<NUM_SP_POINTS; ++i)
    {
        linePattern[i]    = linePatternTemp[i];
        linePatternOld[i] = linePatternTemp[i];
        for(int j=0; j<3; ++j)
        {
            lineColor[i][j]    = lineColorTemp[i][j];
            lineColorOld[i][j] = lineColorTemp[i][j];
        }
    }

    whichType    = whichTypeTemp;
    whichTypeOld = whichTypeTemp;
    typeMenuItems->which           = whichTypeTemp;

    whichStyle    = whichStyleTemp;
    whichStyleOld = whichStyleTemp;
    styleMenuItems->which           = whichStyleTemp;

    if (useR3B)
    {
        whichCoordSystem    = whichCoordSystemTemp;
        whichCoordSystemOld = whichCoordSystemTemp;
        coordSystemMenuItems->which          = whichCoordSystemTemp;
    }
    whichCoord = whichCoordTemp;
    whichCoordOld = whichCoordTemp;
    coordMenuItems->which        = whichCoordTemp;

    graphWidgetToggleSet    = graphWidgetToggleSetTemp;
    graphWidgetToggleSetOld = graphWidgetToggleSetTemp;
    for (unsigned int i = 0; i < XtNumber (graphWidgetItems); i++)
    {
        options[i] = (graphWidgetToggleSetTemp & (1<<i)) ? true : false;
    }

    if(options[OPT_SAT_ANI])
        XtVaSetValues (satAniSpeedSlider, XmNeditable, TRUE, NULL);
    else
        XtVaSetValues (satAniSpeedSlider, XmNeditable, FALSE, NULL);

    if(options[3])
    {
        XtVaSetValues (orbitAniSpeedSlider, XmNeditable, TRUE, NULL);
    }
    else
    {
        XtVaSetValues (orbitAniSpeedSlider, XmNeditable, FALSE, NULL);
    }

    updateScene();

    writePreferValuesToFile();
    XtDestroyWidget (XtParent(XtParent(form)));
}


///////////////////////////////////////////////////////////////////
//         APPLY CALL BACK
//
static void
applyPreferDialogChangeAndUpdateScene(
Widget widget, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    for(int i=0; i<NUM_SP_POINTS; ++i)
    {
        linePattern[i]=linePatternTemp[i];
        for(int j=0; j<3; ++j)
            lineColor[i][j]=lineColorTemp[i][j];
    }

    whichType = whichTypeTemp;
    typeMenuItems->which        = whichTypeTemp;

    whichStyle = whichStyleTemp;
    styleMenuItems->which        = whichStyleTemp;

    if (useR3B)
    {
        whichCoordSystem = whichCoordSystemTemp;
        coordSystemMenuItems->which       = whichCoordSystemTemp;
    }

    whichCoord = whichCoordTemp;
    coordMenuItems->which        = whichCoordTemp;

    graphWidgetToggleSet = graphWidgetToggleSetTemp;
    for (unsigned int i = 0; i < XtNumber (graphWidgetItems); i++)
    {
        options[i] = (graphWidgetToggleSetTemp & (1<<i)) ? true : false;
    }

    if(options[OPT_SAT_ANI])
        XtVaSetValues (satAniSpeedSlider, XmNeditable, TRUE, NULL);
    else
        XtVaSetValues (satAniSpeedSlider, XmNeditable, FALSE, NULL);

    if(options[OPT_PERIOD_ANI])
    {
        XtVaSetValues (orbitAniSpeedSlider, XmNeditable, TRUE, NULL);
    }
    else
    {
        XtVaSetValues (orbitAniSpeedSlider, XmNeditable, FALSE, NULL);
    }

    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
//        This routine is called to get a file name using the
// standard file dialog.
//
static void
getFileName(int fileMode)
//
////////////////////////////////////////////////////////////////////////
{
// use a motif file selection dialog
    if (fileDialog == NULL)
    {
        Arg args[5];
        int n = 0;
        XtSetArg(args[n], XmNautoUnmanage, TRUE); n++;
        if(topform== NULL)
        {
            printf("mainWindow is NULL!\n");
            return;
        }
        fileDialog = XmCreateFileSelectionDialog(
            XtParent(topform), (char *)"File Dialog", args, n);
        XtAddCallback(fileDialog, XmNokCallback,
            fileDialogCB, (XtPointer)fileMode);
    }
    XtManageChild(fileDialog);
}


////////////////////////////////////////////////////////////////////////
//
//        Motif file dialog callback.
//
static void
fileDialogCB(Widget, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    int fileMode = (long)client_data;
    char *filename;
    XmFileSelectionBoxCallbackStruct *data =
        (XmFileSelectionBoxCallbackStruct *)call_data;
    XmStringGetLtoR(data->value,
        (XmStringCharSet) XmSTRING_DEFAULT_CHARSET, &filename);

    SbBool okFile = TRUE;
    if(fileMode == SAVE_ITEM)
        writeToFile(filename);
    else if(fileMode == PRINT_ITEM)
    {
        printToPostScript(root,filename,renderArea,100);
    }
    else if(fileMode == OPEN_ITEM)
    {
        deleteScene();
        okFile = readFile(filename);
    }

    XtFree(filename);
}


////////////////////////////////////////////////////////////////////////
//
//        Brings up the "ABOUT" dialog
//
static void
showAboutDialog()
//
////////////////////////////////////////////////////////////////////////
{
    static Widget dialog = (Widget) 0 ;
    XmString      t;
    unsigned char modality = (unsigned char)XmDIALOG_FULL_APPLICATION_MODAL;
    const char *str;
    if (useR3B)
        str = "  AUTO r3bplaut04\n\n"
    "  Zhang, Chenghai, Dr. Eusebius J. Doedel\n\n "
    "  Computer Science Department\n"
    "  Concordia University\n\n"
    "  Montreal, Quebec\n"
    "  CANADA\n\n"
    "  June, 2004 \n";
    else
        str = "  AUTO plaut04\n\n"
    "  Zhang, Chenghai, Dr. Eusebius J. Doedel\n\n "
    "  Computer Science Department\n"
    "  Concordia University\n\n"
    "  Montreal, Quebec\n"
    "  CANADA\n\n"
    "  August, 2004 \n";

    if (!dialog)
    {
        Arg args[5];
        int n = 0;
        XmString ok = XmStringCreateLocalized ((char *)"OK");
        XtSetArg(args[n], XmNautoUnmanage, False); n++;
        XtSetArg(args[n], XmNcancelLabelString, ok); n++;
        dialog = XmCreateInformationDialog (topform, (char *)"About", args, n);
        Widget remove;
        remove = XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON);
        XtUnmanageChild(remove);
        XtAddCallback (dialog, XmNcancelCallback, showAboutCB, NULL);
        XtUnmanageChild (XtNameToWidget (dialog, "OK"));
    }


    t = XmStringCreateLocalized ((char *)str);
    XtVaSetValues (dialog, XmNmessageString, t, XmNdialogStyle, modality, NULL);
    XmStringFree (t);
    XtManageChild (dialog);
}


////////////////////////////////////////////////////////////////////////
//
static void 
xListCallBack(Widget combo, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XmComboBoxCallbackStruct *cbs = (XmComboBoxCallbackStruct *)call_data;
    char *manyChoice = (char *) XmStringUnparse (cbs->item_or_text, XmFONTLIST_DEFAULT_TAG,
        XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);

    if (cbs->reason == XmCR_SELECT && cbs->event != NULL)
    {
        char * tmp;
        tmp = strtok(manyChoice, ",");
        xCoordIndices.clear();
        do
        {
            xCoordIndices.push_back((strcasecmp(tmp,"t")==0) ? 0 : atoi(tmp));
            tmp = strtok(NULL,",");
        }while(tmp != NULL); 
        if(whichType != BIFURCATION)
        {
             dai.solX = xCoordIndices;
        } else {
             dai.bifX = xCoordIndices;
        }
        updateScene();
    }
}


////////////////////////////////////////////////////////////////////////
//
static void 
yListCallBack(Widget combo, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XmComboBoxCallbackStruct *cbs = (XmComboBoxCallbackStruct *)call_data;
    char *manyChoice = (char *) XmStringUnparse (cbs->item_or_text, XmFONTLIST_DEFAULT_TAG,
        XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);

    if (cbs->reason == XmCR_SELECT && cbs->event != NULL)
    {
        char * tmp;
        tmp = strtok(manyChoice, ",");
        yCoordIndices.clear();
        do
        {
            yCoordIndices.push_back((strcasecmp(tmp,"t")==0) ? 0 : atoi(tmp));
            tmp = strtok(NULL,",");
        }while(tmp != NULL);
        if(whichType != BIFURCATION)
        {
             dai.solY = yCoordIndices;
        } else {
             dai.bifY = yCoordIndices;
        }
        updateScene();
    }
}


////////////////////////////////////////////////////////////////////////
//
static void 
zListCallBack(Widget combo, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XmComboBoxCallbackStruct *cbs = (XmComboBoxCallbackStruct *)call_data;
    char *manyChoice = (char *) XmStringUnparse (cbs->item_or_text, XmFONTLIST_DEFAULT_TAG,
        XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);

    if (cbs->reason == XmCR_SELECT && cbs->event != NULL)
    {
        char * tmp;
        tmp = strtok(manyChoice, ",");
        zCoordIndices.clear();
        do
        {
            zCoordIndices.push_back((strcasecmp(tmp,"t")==0) ? 0 : atoi(tmp));
            tmp = strtok(NULL,",");
        }while(tmp != NULL);
        if(whichType != BIFURCATION)
        {
             dai.solZ = zCoordIndices;
        } else {
             dai.bifZ = zCoordIndices;
        }
        updateScene();
    }
}


////////////////////////////////////////////////////////////////////////
//
static void
lblListCallBack(Widget combo, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XmComboBoxCallbackStruct *cbs = (XmComboBoxCallbackStruct *)call_data;
    int choice = (int) cbs->item_position - 1;
    int nItems = (whichType != BIFURCATION) ? mySolNode->totalLabels() : myBifNode->totalLabels();
    char *manyChoice = (char *) XmStringUnparse (cbs->item_or_text, XmFONTLIST_DEFAULT_TAG,
        XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);

    char * tmp;
    static int half = 2;
    tmp = strtok(manyChoice, ",");
    choice -= SP_LBL_ITEMS;
    lblIndices.clear();
    if(choice <= MY_ALL || choice >= nItems)
    {
        do
        {
            lblIndices.push_back((strcasecmp(tmp,"all")==0) ? numLabels + MY_ALL : 
	      atoi(tmp)-myLabels[0]);
            tmp = strtok(NULL,",");
        }while(tmp != NULL);
        half = 2;
    }
    else if(choice == MY_HALF) // -3 
    {
        for(int j = 0; j < numLabels - SP_LBL_ITEMS; j++)
            if(abs(clientData.labelIndex[j][2])!= 4 || (j+1)%half == 0)
                lblIndices.push_back(j);
        half *= 2;
    }
    else if(choice == MY_SPEC) // -2
    {
        for(int j = 0; j < numLabels - SP_LBL_ITEMS; j++)
            if(clientData.labelIndex[j][2] !=  TYPE_UZ  && clientData.labelIndex[j][2] != TYPE_RG
            && clientData.labelIndex[j][2] != TYPE_EP_ODE && clientData.labelIndex[j][2] != TYPE_MX)
                lblIndices.push_back(j);
        half = 2;
    }
    else if(choice == MY_NONE) // -1
    {
        lblIndices.push_back(numLabels + MY_NONE);
        half = 2;
    }
    else
    {
        lblIndices.push_back(choice);
        half = 2;
    }

    lblChoice.clear();
    if(choice < 0)
        lblChoice.push_back(choice);
    else for (std::vector<int>::size_type i = 0; i < lblIndices.size(); i++)
        lblChoice.push_back(lblIndices[i]);

    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
static void
showAboutCB(Widget dialog, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XtUnmanageChild (dialog);
}


////////////////////////////////////////////////////////////////////////
//
static void
hidenDialogShell (Widget widget, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XtUnmanageChild (XtParent(widget));
}


////////////////////////////////////////////////////////////////////////
//
static void 
redrawFloqueMultipliers (Widget fmDrawingArea, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XmDrawingAreaCallbackStruct *cbs = (XmDrawingAreaCallbackStruct *) call_data;
    int numFM = (int)(long)client_data;

    XAllocNamedColor(cbs->event->xexpose.display, colormap, "blue", &blue, &exact);
    XAllocNamedColor(cbs->event->xexpose.display, colormap, "green", &green, &exact);
    XAllocNamedColor(cbs->event->xexpose.display, colormap, "white", &white, &exact);
    XAllocNamedColor(cbs->event->xexpose.display, colormap, "red", &red, &exact);
    XAllocNamedColor(cbs->event->xexpose.display, colormap, "black", &black, &exact);
    XAllocNamedColor(cbs->event->xexpose.display, colormap, "grey", &grey, &exact);

    static XTextItem myText[9] =
    {
      { (char *)"-inf", 4, 0, None }
        ,
        { (char *)"-100", 4, 0, None }
        ,
        { (char *)"-10", 3, 0, None },
        { (char *)"-1", 2, 0, None},
        { (char *)"0", 1, 0, None }
        ,
        { (char *)"1", 1, 0, None },
        { (char *)"10", 2, 0, None},
        { (char *)"100", 3, 0, None }
        ,
        { (char *)"+inf", 4, 0, None }
    };

    XSetForeground(cbs->event->xexpose.display, gc, blue.pixel);

// draw Y
    XSetLineAttributes(cbs->event->xexpose.display, gc, 2, LineSolid, CapRound, JoinRound);
    XDrawLine(cbs->event->xexpose.display, cbs->window, gc, 200, 0, 200, 400);

// draw X
    XSetForeground(cbs->event->xexpose.display, gc, red.pixel);
    XSetLineAttributes(cbs->event->xexpose.display, gc, 2, LineSolid, CapRound, JoinRound);
    XDrawLine(cbs->event->xexpose.display, cbs->window, gc, 0, 200, 400, 200);

// draw grid
    XSetForeground(cbs->event->xexpose.display, gc, grey.pixel);
    XSetLineAttributes(cbs->event->xexpose.display, gc, 1, LineOnOffDash, CapButt, JoinRound);
    for(int i=0; i<9; ++i)
        XDrawLine(cbs->event->xexpose.display, cbs->window, gc, 0, 50*i, 400, 50*i);
    XSetLineAttributes(cbs->event->xexpose.display, gc, 1, LineOnOffDash, CapButt, JoinRound);
    for(int i=0; i<9; ++i)
        XDrawLine(cbs->event->xexpose.display, cbs->window, gc, i*50, 0, i*50, 400);

// draw text
    XSetForeground(cbs->event->xexpose.display, gc, black.pixel);
    for(int i = 0; i < 9; ++i)
        XDrawText(cbs->event->xexpose.display,cbs-> window, gc, i*50-3 , 215, &myText[i], 1);
    for(int i = 0; i < 9; ++i)
        XDrawText(cbs->event->xexpose.display,cbs-> window, gc, 210 , 410-i*50, &myText[i], 1);

    XSetForeground(cbs->event->xexpose.display, gc, green.pixel);

// draw a unit circle.
    if (numFM > 0) {
        XSetLineAttributes(cbs->event->xexpose.display, gc, 1, LineSolid, CapRound, JoinRound);
        XDrawArc(cbs->event->xexpose.display, cbs->window, gc, 150, 150, 100, 100, 360*64, 360*64);
    }

    XSetForeground(cbs->event->xexpose.display, gc, black.pixel);

    int x, y;

    XSetLineAttributes(cbs->event->xexpose.display, gc, 2, LineSolid, CapRound, JoinRound);
    for(int j = 0; j<abs(numFM); ++j)
    {
        float tmp = fmData[2*j];
        if(fabs(tmp) <= 1.1)
            x = (tmp>0.0) ? (int)(200+tmp*50) : (int)(200-fabs(tmp)*50);
        else
            x =(tmp>0.0) ? (int)(250+log10(tmp)*50) : (int)(150-log10(fabs(tmp))*50);

        tmp = fmData[2*j+1];
        if(fabs(tmp) <= 1.1)
            y = (tmp>0.0) ? (int)(200+tmp*50) : (int)(200-fabs(tmp)*50);
        else
            y =(tmp>0.0) ? (int)(150-log10(tmp)*50): (int)(log10(fabs(tmp))*50+250);

        if(x>390) x = 390; if(x<10) y = 10;
        if(y>390) y = 390; if(y<10) y = 10;

        XDrawLine(cbs->event->xexpose.display, cbs->window, gc, x-3, y-3, x+3, y+3);
        XDrawLine(cbs->event->xexpose.display, cbs->window, gc, x-3, y+3, x+3, y-3);

    }
}


////////////////////////////////////////////////////////////////////////
//
void
popupFloquetMultiplierDialog(float data[], int size, int numFM)
//
////////////////////////////////////////////////////////////////////////
{

    static Widget dialog_shell = (Widget) 0 ;
    Widget pane = (Widget) 0 ;              
    XmString      str1;
    char *str, temp[200];

    str = new char[size*50];
    str[0]='\0';

    for(int i=0; i<size; ++i)
    {
        strcat(str," Col[");
        sprintf(temp,"%2d",i+1);
        strcat(str,temp);
        strcat(str," ] = ");

        sprintf(temp,"%+E",data[i]);
        strcat(str,temp);
        if(size<20 || (size>=20 && (i+1)%2==0)) strcat(str,"\n");
        else strcat(str," | ");
    }

    char *tmpstr;
    tmpstr = new char[500];
    tmpstr[0]='\0';
    if (numFM <= 0)
        strcat(tmpstr,"Eigenvalues:\n" );
    else
        strcat(tmpstr,"Floquet multipliers:\n" );
    for(int j=0; j<abs(numFM); ++j)
    {
        strcat(tmpstr," [");
        sprintf(temp,"%2d",j);
        strcat(tmpstr,temp);
        strcat(tmpstr,"] : ");
        sprintf(temp,"%E",fmData[j*2]);
        strcat(tmpstr,temp);
        if(fmData[j*2+1] != 0) {
            strcat(tmpstr,fmData[j*2+1] < 0 ? " - " : " + ");
            sprintf(temp,"%E",fabs(fmData[j*2+1]));
            strcat(tmpstr,temp);
            strcat(tmpstr,"i\n");
        }
    }

    if (!pane)
    {
        Widget fmDrawingArea;
        Arg args[5];
        int n=0;
        XtSetArg(args[n], XmNdeleteResponse,XmDESTROY); ++n;
        dialog_shell = XmCreateDialogShell(topform, (char *)"Dialog", args, n);

        n=0;
        XtSetArg(args[n], XmNsashWidth, 1); ++n;
        XtSetArg(args[n], XmNsashHeight, 2); ++n;
        pane = XmCreatePanedWindow(dialog_shell, (char *)"pane",args,n);

        n = 0;
        XtSetArg (args[n], XmNunitType, Xm1000TH_INCHES); n++;
        XtSetArg (args[n], XmNwidth,  5500); n++;
        XtSetArg (args[n], XmNheight, 5500); n++;
        XtSetArg (args[n], XmNresizePolicy, XmRESIZE_NONE); n++;
        fmDrawingArea = XmCreateDrawingArea (pane, (char *)"fmDrawingArea", args, n);

        XtAddCallback (fmDrawingArea, XmNexposeCallback, redrawFloqueMultipliers, (XtPointer)numFM);

        XtVaSetValues (fmDrawingArea, XmNunitType, XmPIXELS, NULL);

        XGCValues    gcv;
        gcv.foreground = WhitePixelOfScreen (XtScreen (fmDrawingArea));
        gc = XCreateGC (XtDisplay (fmDrawingArea),
            RootWindowOfScreen (XtScreen (fmDrawingArea)), GCForeground, &gcv);
        colormap = DefaultColormapOfScreen(XtScreen(fmDrawingArea));

        XSetForeground (XtDisplay (fmDrawingArea), gc,
            BlackPixelOfScreen (XtScreen (fmDrawingArea)));

        Widget form = XmCreateForm(pane, (char *)"form", NULL,0);
        str1 = XmStringCreateLocalized(str);

        n = 0;
        XtSetArg (args[n], XmNlabelString, str1);  ++n;
        XtSetArg(args[n], XmNtopAttachment,     XmATTACH_FORM); n++;
        XtSetArg(args[n], XmNleftAttachment,    XmATTACH_FORM); n++;
        XtSetArg(args[n], XmNleftOffset,        5            ); n++;
        Widget label3 = XmCreateLabelGadget (form, (char *)"label", args, n);
        XtManageChild (label3);

        str1 = XmStringCreateLocalized(tmpstr);
        n = 0;
        XtSetArg (args[n], XmNlabelString, str1);  ++n;
        XtSetArg(args[n], XmNtopAttachment,     XmATTACH_FORM); n++;
        XtSetArg(args[n], XmNleftAttachment,    XmATTACH_WIDGET); n++;
        XtSetArg(args[n], XmNleftWidget,    label3); n++;
        XtSetArg(args[n], XmNleftOffset,        5            ); n++;
        XtSetArg(args[n], XmNrightAttachment,   XmATTACH_FORM); n++;
        Widget label2 = XmCreateLabelGadget (form, (char *)"label", args, n);
        XtManageChild (label2);

        XtManageChild (form);

        Widget pushButton = XmCreatePushButtonGadget (pane, (char *)"OK", NULL, 0);
        XtAddCallback (pushButton, XmNactivateCallback, hidenDialogShell, dialog_shell);

        XtManageChild (pushButton);
        XtManageChild (fmDrawingArea);

        XmStringFree (str1);
        XtManageChild (pane);
    }
    XtManageChild (dialog_shell);
    delete [] str;
    delete [] tmpstr;
}


void soxtmain(int argc, char *argv[])
{
// Initialize Inventor and Xt.
    Widget  mainWindow;
    mainWindow = SoXt::init(argv[0]);

    if (mainWindow == NULL) return;

    root = new SoSeparator;
    root->ref();
    buildMainWindow(mainWindow, root);

    SoXt::show(mainWindow);
    SoXt::mainLoop();
    root->unref();
}
