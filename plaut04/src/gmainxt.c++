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

#define LBL_OFFSET   4

#ifdef LESSTIF_VERSION
#include <Xm/ComboBoxP.h>
#ifndef XmNwrap
#define XmNwrap ((char *)&_XmStrings[19401])
#endif
#undef XmFONTLIST_DEFAULT_TAG
#define XmFONTLIST_DEFAULT_TAG NULL
#endif

SbBool printToPostScript (SoNode *root, FILE *file,
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
void fileDialogCB(Widget, XtPointer client_data, XtPointer data);
void hidenDialogShell (Widget widget, XtPointer client_data, XtPointer call_data);
void applyPreferDialogChangeAndUpdateScene(Widget widget, XtPointer client_data, XtPointer call_data);
void closePreferDialogAndUpdateScene(Widget widget, XtPointer client_data, XtPointer call_data);
void closePreferDialogAndGiveUpChange(Widget widget, XtPointer client_data, XtPointer call_data);
void savePreferAndUpdateScene(Widget widget, XtPointer client_data, XtPointer call_data);
static void getFileName(int fileMode);
static void showAboutDialog();
static void createPreferDialog();
extern SoSeparator * createBoundingBox();

////////////////////////////////////////////////////////////////////////
//
//  functions
//
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
//
void
orbitSpeedCB(Widget, XtPointer userData, XtPointer callData)
//
////////////////////////////////////////////////////////////////////////
{
    XmScaleCallbackStruct *data = (XmScaleCallbackStruct *) callData;
    orbitSpeed = data->value/50.0;                ///50.0;     ///75.0;
    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
void
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
void
numPeriodAnimatedCB(Widget, XtPointer userData, XtPointer callData)
//
////////////////////////////////////////////////////////////////////////
{
    XmComboBoxCallbackStruct *cbs = (XmComboBoxCallbackStruct *)callData;
    char *myChoice = (char *) XmStringUnparse (cbs->item_or_text, XmFONTLIST_DEFAULT_TAG,
        XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
    int i = 0;

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
void
colorMethodSelectionCB(Widget, XtPointer userData, XtPointer callData)
//
////////////////////////////////////////////////////////////////////////
{
    XmComboBoxCallbackStruct *cbs = (XmComboBoxCallbackStruct *)callData;
    char *myChoice = (char *) XmStringUnparse (cbs->item_or_text, XmFONTLIST_DEFAULT_TAG,
        XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
    int i = 0;
    int choice = (int) cbs->item_position;

    coloringMethod = (strcasecmp(myChoice,"COMP")==0) ?  CL_COMPONENT:
    ((strcasecmp(myChoice,"TYPE")==0) ?  CL_ORBIT_TYPE :
    coloringMethod = (strcasecmp(myChoice,"TYPE")==0) ?  CL_ORBIT_TYPE :
    ((strcasecmp(myChoice,"BRAN")==0) ? CL_BRANCH_NUMBER:
    ((strcasecmp(myChoice,"PONT")==0) ? CL_POINT_NUMBER :
    ((strcasecmp(myChoice,"LABL")==0) ? CL_LABELS:
    ((strcasecmp(myChoice,"STAB")==0) ? CL_STABILITY : choice - specialColorItems)))));

    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
void
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
            cropScene("myfile");
            break;
        case OPEN_ITEM:
            getFileName(OPEN_ITEM);
            break;
        default:
            printf("UNKNOWN file menu item!!!\n"); break;
    }
}


#ifdef R3B
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
#endif


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

#ifdef R3B
    if(graphWidgetToggleSet & (1<<OPT_NORMALIZE_DATA))
    {
        options[OPT_PRIMARY] = false;
        options[OPT_LIB_POINTS] = false;
        options[OPT_REF_PLAN] = false;
        graphWidgetToggleSet &= ~(1 << OPT_REF_PLAN);
        graphWidgetToggleSet &= ~(1 << OPT_LIB_POINTS);
        graphWidgetToggleSet &= ~(1 << OPT_PRIMARY);
    }
#endif

    updateScene();
}


///////////////////////////////////////////////////////////////////////////
//
void
setListValue()
//
///////////////////////////////////////////////////////////////////////////
{
    long nItems = (whichType != BIFURCATION) ? 
	                   mySolNode.totalLabels+SP_LBL_ITEMS : 
					   myBifNode.totalLabels+SP_LBL_ITEMS;
    int count = XtNumber (labels);

    lblList = (XmStringTable) XtMalloc(count * sizeof (XmString *));
    for (int i = 0; i < count; i++)
        lblList[i] = XmStringCreateLocalized (labels[i]);

    if(whichType != BIFURCATION)
    {
        xCoordIdxSize = dai.solXSize;
        yCoordIdxSize = dai.solYSize;
        zCoordIdxSize = dai.solZSize;
        for(int i = 0; i<xCoordIdxSize; ++i)
            xCoordIndices[i] = dai.solX[i];
        for(int i = 0; i<yCoordIdxSize; ++i)
            yCoordIndices[i] = dai.solY[i];
        for(int i = 0; i<zCoordIdxSize; ++i)
            zCoordIndices[i] = dai.solZ[i];

        XtVaSetValues(xAxisList, XmNitems, xList, XmNitemCount, mySolNode.nar, NULL);
        XtVaSetValues(yAxisList, XmNitems, yList, XmNitemCount, mySolNode.nar, NULL);
        XtVaSetValues(zAxisList, XmNitems, zList, XmNitemCount, mySolNode.nar, NULL);

        int sp = 0;
        strcpy(coloringMethodList[0],"STAB"); sp++;
        strcpy(coloringMethodList[1],"PONT"); sp++;
        strcpy(coloringMethodList[2],"BRAN"); sp++;
        strcpy(coloringMethodList[3],"TYPE"); sp++;
        strcpy(coloringMethodList[4],"LABL"); sp++;
        strcpy(coloringMethodList[5],"COMP"); sp++;
        specialColorItems = sp;
        for(int i=sp; i<mySolNode.nar+sp; ++i)
        {
            sprintf(coloringMethodList[i],"%d",i-sp);
        }
        for(int i=mySolNode.nar+sp; i<mySolNode.nar+mySolNode.npar+sp; ++i)
        {
            sprintf(coloringMethodList[i],"PAR(%d)",mySolNode.parID[i-(mySolNode.nar+sp)]+1);
        }

        int count = XtNumber (coloringMethodList);
        for (int i = 0; i < count; ++i)
            clrMethodList[i] = XmStringCreateLocalized (coloringMethodList[i]);

        XtVaSetValues(colorMethodSeletionList, XmNitems, clrMethodList, 
		               XmNitemCount, mySolNode.nar+mySolNode.npar+sp, NULL);
        XtVaSetValues(labelsList, XmNitems, lblList, XmNitemCount, nItems, NULL);
    }
    else
    {
        xCoordIdxSize = dai.bifXSize;
        yCoordIdxSize = dai.bifYSize;
        zCoordIdxSize = dai.bifZSize;
        for(int i = 0; i<xCoordIdxSize; ++i)
            xCoordIndices[i] = dai.bifX[i];
        for(int i = 0; i<yCoordIdxSize; ++i)
            yCoordIndices[i] = dai.bifY[i];
        for(int i = 0; i<zCoordIdxSize; ++i)
            zCoordIndices[i] = dai.bifZ[i];
        XtVaSetValues(xAxisList, XmNitems, xList, XmNitemCount, myBifNode.nar, NULL);
        XtVaSetValues(yAxisList, XmNitems, yList, XmNitemCount, myBifNode.nar, NULL);
        XtVaSetValues(zAxisList, XmNitems, zList, XmNitemCount, myBifNode.nar, NULL);

        int sp = 0;
        strcpy(coloringMethodList[0],"STAB"); sp++;
        strcpy(coloringMethodList[1],"PONT"); sp++;
        strcpy(coloringMethodList[2],"BRAN"); sp++;
        specialColorItems = sp;
        for(int i=sp; i<myBifNode.nar+sp; ++i)
        {
            sprintf(coloringMethodList[i],"%d",i-sp);
        }
        int count = XtNumber (coloringMethodList);
        for (int i = 0; i < count; ++i)
            clrMethodList[i] = XmStringCreateLocalized (coloringMethodList[i]);

        XtVaSetValues(colorMethodSeletionList, XmNitems, clrMethodList, XmNitemCount, myBifNode.nar+sp,NULL);
        XtVaSetValues(labelsList, XmNitems, lblList, XmNitemCount, nItems, NULL);
    }

    if(setShow3D)
        XtSetSensitive (zAxisList, true);
    else
    {
        for(int i=0; i<MAX_LIST; i++)
            zCoordIndices[i] = -1;
        zCoordIdxSize = 1;
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

    if(!blOpenSolFile)
        XtSetSensitive (menuItems->items[SOLUTION], FALSE);

    if(!blOpenBifFile)
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

#ifndef R3B
    if( whichType == BIFURCATION)
#else
    if(whichCoordSystem != ROTATING_F || whichType == BIFURCATION)
#endif
    {
        XtSetSensitive (menuItems->items[SURFACE], false);
        XtSetSensitive (menuItems->items[MESH_POINTS], false);
        XtSetSensitive (menuItems->items[ALL_POINTS], false);
#ifdef R3B
        if(menuItems->which == SURFACE || menuItems->which == MESH_POINTS || menuItems->which == ALL_POINTS)
            menuItems->which = LINE;
#endif
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
        TOGGLE_OFF(menuItems->items[DRAW_TICKER]);

        TOGGLE_ON(menuItems->items[menuItems->which]);
        if(blDrawTicker)
            XmToggleButtonSetState(menuItems->items[DRAW_TICKER], TRUE, FALSE);
    }
}


#ifdef R3B
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
#endif


////////////////////////////////////////////////////////////////////////
//
//  This is called by Xt just before the TYPE menu is displayed.
//
static void 
optMenuDisplay(Widget, void *userData, XtPointer)
//
////////////////////////////////////////////////////////////////////////
{
    Arg args[3];
    int n = 0;
    EditMenuItems *menuItems = (EditMenuItems *) userData;

    for (int i = 0; i < XtNumber (graphWidgetItems); i++)
    {
        n = 0;
        if (graphWidgetToggleSet & (1<<i))
        {
            XmToggleButtonSetState(menuItems->items[i], TRUE, FALSE);
        }
        else
        {
            XmToggleButtonSetState(menuItems->items[i], FALSE, FALSE);
        }
    }

    XmString xString = XmStringCreateLocalized((char *)"draw Label");
#ifdef R3B
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
#endif

    if(whichType == SOLUTION)
    {
        XtSetSensitive (menuItems->items[OPT_SAT_ANI], TRUE);

#ifndef R3B
        XmString xString = XmStringCreateLocalized((char *)"Highlight Orbit");
#else
        XmString xString = XmStringCreateLocalized((char *)"Orbit Animation");
#endif
        XtVaSetValues (menuItems->items[OPT_PERIOD_ANI], XmNlabelString, xString, NULL);
        XmStringFree(xString);
    }
    else
    {
        XtSetSensitive (menuItems->items[OPT_SAT_ANI], FALSE);

        XmString xString = XmStringCreateLocalized((char *)"Draw Labels");
        XtVaSetValues (menuItems->items[OPT_PERIOD_ANI], XmNlabelString, xString, NULL);
        XmStringFree(xString);
        XtSetSensitive (menuItems->items[OPT_LABEL_NUMBERS],
            (graphWidgetToggleSet & (1<<OPT_PERIOD_ANI)) != 0);
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
Widget
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
    const char *openAccel  = "Alt <Key> o";
    const char *saveAccel  = "Alt <Key> s";
    const char *printAccel = "Alt <Key> p";
    const char *quitAccel  = "Alt <Key> q";
    XmString openAccelText  = XmStringCreate((char *)"Alt+o", XmSTRING_DEFAULT_CHARSET);
    XmString saveAccelText  = XmStringCreate((char *)"Alt+s", XmSTRING_DEFAULT_CHARSET);
#ifdef R3B
    XmString printAccelText = XmStringCreate((char *)"Alt+p", XmSTRING_DEFAULT_CHARSET);
#endif
    XmString quitAccelText  = XmStringCreate((char *)"Alt+q", XmSTRING_DEFAULT_CHARSET);
    n = 0;
    XtSetArg(args[n], XmNaccelerator, openAccel); n++;
    XtSetArg(args[n], XmNacceleratorText, openAccelText); n++;
    PUSH_ITEM(items[0], "Open...", OPEN_ITEM, fileMenuPick);

    n = 0;
    XtSetArg(args[n], XmNaccelerator, saveAccel); n++;
    XtSetArg(args[n], XmNacceleratorText, saveAccelText); n++;
    PUSH_ITEM(items[1], "Export...", SAVE_ITEM, fileMenuPick);

    n = 0;
#ifdef R3B
    XtSetArg(args[n], XmNaccelerator, printAccel); n++;
    XtSetArg(args[n], XmNacceleratorText, printAccelText); n++;
    PUSH_ITEM(items[2], "Print...", PRINT_ITEM, fileMenuPick);
#endif

    SEP_ITEM("separator");

    n = 0;
//    XtSetArg(args[n], XmNaccelerator, quitAccel); n++;
//    XtSetArg(args[n], XmNacceleratorText, quitAccelText); n++;
//    PUSH_ITEM(items[3], "Quit",    QUIT_ITEM, fileMenuPick);
#ifndef R3B
    PUSH_ITEM(items[2], "Quit",    QUIT_ITEM, fileMenuPick);
#else
    XtSetArg(args[n], XmNaccelerator, quitAccel); n++;
    XtSetArg(args[n], XmNacceleratorText, quitAccelText); n++;
    PUSH_ITEM(items[3], "Quit",    QUIT_ITEM, fileMenuPick);
#endif

//    XtManageChildren(items, 4);
#ifndef R3B
    XtManageChildren(items, 2);
#else
    XtManageChildren(items, 4);
#endif

    return pulldown;
}


////////////////////////////////////////////////////////////////////////
//
//  This creates the Help menu and all its items.
//
Widget
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
Widget
buildOptionMenu(Widget menubar)
//
////////////////////////////////////////////////////////////////////////
{
    Widget  pulldown;
    Arg     args[13];
    int        n;

    EditMenuItems *menuItems = new EditMenuItems;
#ifndef R3B
    menuItems->items = new Widget[6];
#else
    menuItems->items = new Widget[13];
#endif

// Tell motif to create the menu in the popup plane
    Arg popupargs[13];
    int popupn = 0;

    pulldown = XmCreatePulldownMenu(menubar, (char *)"optionMenu", popupargs, popupn);

    XtAddCallback(pulldown, XmNmapCallback,
        (XtCallbackProc) optMenuDisplay, (XtPointer) menuItems);

    n = 0;
    int mq = 0;
    XtSetArg(args[n], XmNuserData, menuItems); n++;
#ifndef R3B
    TOGGLE_ITEM(menuItems->items[mq], "Hightlight Orbit",     OPT_PERIOD_ANI, optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[mq], "Show Label Numbers",   OPT_LABEL_NUMBERS, optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[mq], "Orbit Animation",      OPT_SAT_ANI,    optMenuPick); ++mq;
#else
    TOGGLE_ITEM(menuItems->items[mq], "Draw Reference Plane", OPT_REF_PLAN,   optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[mq], "Draw Primaries",       OPT_PRIMARY,    optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[mq], "Draw Libration Pts",   OPT_LIB_POINTS, optMenuPick); ++mq;
    SEP_ITEM("separator");
    TOGGLE_ITEM(menuItems->items[mq], "Orbit Animation",      OPT_PERIOD_ANI, optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[mq], "Satellite Animation",  OPT_SAT_ANI,    optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[mq], "Show Label Numbers",   OPT_LABEL_NUMBERS, optMenuPick); ++mq;
#endif
    TOGGLE_ITEM(menuItems->items[mq], "Draw Background",      OPT_BACKGROUND, optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[mq], "Add Legend",           OPT_LEGEND, optMenuPick); ++mq;
    TOGGLE_ITEM(menuItems->items[mq], "Normalize Data",       OPT_NORMALIZE_DATA, optMenuPick); ++mq;
    XtManageChildren(menuItems->items, mq);

    Widget pushitem;
    SEP_ITEM("separator");
    PUSH_ITEM(pushitem, "PREFERENCES", ITEM_ONE, createPreferDialog);
    XtManageChild(pushitem);

    return pulldown;
}


#ifdef R3B
////////////////////////////////////////////////////////////////////////
//
//  This creates the Edit menu and all its items.
//
Widget
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
#endif


////////////////////////////////////////////////////////////////////////
//
//  This creates the STYLE menu and all its items.
//
Widget
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

#ifndef R3B
    Arg popupargs[5];                         
#else
// Tell motif to create the menu in the popup plane
    Arg popupargs[3];
#endif
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
Widget
buildCoordMenu(Widget menubar)
//
////////////////////////////////////////////////////////////////////////
{
    Widget  pulldown;
    Arg     args[3];
    int        n;

    coordMenuItems = new EditMenuItems;
    coordMenuItems->items = new Widget[5];
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
    SEP_ITEM("separator");
    TOGGLE_ITEM(coordMenuItems->items[4], "Draw Scale",   DRAW_TICKER, coordMenuPick);

    XtManageChildren(coordMenuItems->items, 5);

    return pulldown;
}


////////////////////////////////////////////////////////////////////////
//
//  This creates the TYPE menu and all its items.
//
Widget
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
Widget
buildMenu(Widget parent)
//
////////////////////////////////////////////////////////////////////////
{
#ifndef R3B
    Widget  menuButtons[6];
#else
    Widget  menuButtons[7];
    Widget  pulldown2;
#endif
    Widget  pulldown1, pulldown3, pulldown4, pulldown5, pulldown6, pulldown7;
    Arg     args[8];
    int        n, m;

// menu bar
    Widget menubar = XmCreateMenuBar(parent, (char *)"menuBar", NULL, 0);
#ifndef LESSTIF_VERSION
    XtVaSetValues (menubar, XmNshadowThickness, 1, NULL);
#endif
    pulldown1 = buildFileMenu(menubar);
#ifdef R3B
    pulldown2 = buildCenterMenu(menubar);
#endif
    pulldown3 = buildStyleMenu(menubar);
    pulldown4 = buildTypeMenu(menubar);
    pulldown7 = buildCoordMenu(menubar);
    pulldown5 = buildOptionMenu(menubar);
    pulldown6 = buildHelpMenu(menubar);

#ifdef R3B
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
#endif
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

    n = 0;
#ifdef R3B
    XtSetArg(args[n], XmNsubMenuId, pulldown2); n++;
    menuButtons[m] = XtCreateManagedWidget("Center",
        xmCascadeButtonGadgetClass,
        menubar, args, n);
    ++m;

    n = 0;
#endif
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
void 
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
        for(int i=0; i<MAX_LIST; i++)
            zCoordIndices[i] = -1;
        zCoordIdxSize = 1;
        XtSetSensitive (zAxisList, false);
        XmString xString = XmStringCreateLocalized((char *)"2D");
        XtVaSetValues (w, XmNlabelString, xString, NULL);
        XmStringFree(xString);
    }
    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
void 
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
SoXtRenderArea *
buildMainWindow(Widget parent, SoSeparator *sceneGraph)
//
////////////////////////////////////////////////////////////////////////
{
#ifdef USE_EXAM_VIEWER
    SoXtExaminerViewer *renderArea;
#else
    SoXtRenderArea *renderArea;
#endif

    Arg  args[15];
    int  n,i;

// build the toplevel widget
    topform = XtCreateWidget("topform", xmFormWidgetClass, parent,NULL, 0);
// build menubar
    Widget menubar = buildMenu(topform);
#ifdef R3B
#ifdef USE_BK_COLOR
    XtVaSetValues (topform, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
    XtVaSetValues (menubar, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
#endif
#endif

// build carrier for the x, y, z, and label lists.
    Widget listCarrier= XtCreateWidget("ListCarrier",
        xmFormWidgetClass, topform, NULL, 0);
#ifdef R3B
#ifdef USE_BK_COLOR
    XtVaSetValues (listCarrier, XtVaTypedArg,
        XmNbackground, XmRString, "white", 4,
        NULL);
#endif
#endif

//build the xAxis drop down list
    int nItems = (whichType != BIFURCATION) ? mySolNode.nar : myBifNode.nar;
    int count = XtNumber (xAxis);
    xList = (XmStringTable) XtMalloc(count * sizeof (XmString *));
    for (i = 0; i < count; i++)
        xList[i] = XmStringCreateLocalized (xAxis[i]);

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

#ifdef LESSTIF_VERSION
    Widget list = CB_List(xAxisList);
    XmListDeleteAllItems(list);
    for (i = 0; i < nItems; i++)
	XmListAddItem(list,xList[i],0);
    XtVaSetValues(xAxisList,
        XmNvisibleItemCount,   10,
        XmNwidth,              60,
        XmNcolumns,            2,
        NULL);
    XmComboBoxUpdate(xAxisList);
#endif

// Add Callback function for the x-axis drop down list
    XtAddCallback (xAxisList, XmNselectionCallback,
        xListCallBack, NULL);

#ifdef R3B
#ifdef USE_BK_COLOR
    XtVaSetValues (xAxisList, XtVaTypedArg,
        XmNbackground, XmRString, "white", 4,
        NULL);
#endif
#endif

// build the yAxis drop down list
    count = XtNumber (yAxis);
    yList = (XmStringTable) XtMalloc(count * sizeof (XmString *));
    for (i = 0; i < count; i++)
        yList[i] = XmStringCreateLocalized (yAxis[i]);

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

#ifdef LESSTIF_VERSION
    list = CB_List(yAxisList);
    XmListDeleteAllItems(list);
    for (i = 0; i < nItems; i++)
	XmListAddItem(list,yList[i],0);
    XtVaSetValues(yAxisList,
        XmNvisibleItemCount,   10,
        XmNwidth,              60,
        XmNcolumns,            2,
        NULL);
    XmComboBoxUpdate(yAxisList);
#endif

    XtAddCallback (yAxisList, XmNselectionCallback,
        yListCallBack, NULL);

#ifdef R3B
#ifdef USE_BK_COLOR
    XtVaSetValues (yAxisList, XtVaTypedArg,
        XmNbackground, XmRString, "white", 4,
        NULL);
#endif
#endif

// build the zAxis drop down list
    count = XtNumber (zAxis);
    zList = (XmStringTable) XtMalloc(count * sizeof (XmString *));
    for (i = 0; i < count; i++)
        zList[i] = XmStringCreateLocalized (zAxis[i]);

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

#ifdef LESSTIF_VERSION
    list = CB_List(zAxisList);
    XmListDeleteAllItems(list);
    for (i = 0; i < nItems; i++)
	XmListAddItem(list,zList[i],0);
    XtVaSetValues(zAxisList,
        XmNvisibleItemCount,   10,
        XmNwidth,              60,
        XmNcolumns,            2,
        NULL);
    XmComboBoxUpdate(zAxisList);
#endif

// Add Callback function for the z-axis drop down list
    XtAddCallback (zAxisList, XmNselectionCallback,
        zListCallBack, NULL);

#ifdef R3B
#ifdef USE_BK_COLOR
    XtVaSetValues (zAxisList, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
#endif
#endif

// build the LABELs drop down list
    nItems = (whichType != BIFURCATION) ? 
	               mySolNode.totalLabels+SP_LBL_ITEMS : 
	               myBifNode.totalLabels+SP_LBL_ITEMS;
    count = XtNumber (labels);
    lblList = (XmStringTable) XtMalloc(count * sizeof (XmString *));
    for (i = 0; i < count; i++)
        lblList[i] = XmStringCreateLocalized (labels[i]);

    labelsList=XtVaCreateManagedWidget ("Labels",
        xmComboBoxWidgetClass, listCarrier,
        XmNcomboBoxType,       XmDROP_DOWN_COMBO_BOX,
        XmNitemCount,          nItems,
        XmNitems,              lblList,
        XmNcolumns,            6,
        XmNmarginHeight,       1,
        XmNselectedPosition,   lblChoice[0]+LBL_OFFSET-1, //lblIndices[0],
        XmNpositionMode,       XmZERO_BASED,
        NULL);

#ifdef LESSTIF_VERSION
    list = CB_List(labelsList);
    XmListDeleteAllItems(list);
    for (i = 0; i < nItems; i++)
	XmListAddItem(list,lblList[i],0);
    XtVaSetValues(labelsList,
        XmNvisibleItemCount,   10,
        XmNwidth,              86,
        XmNcolumns,            6,
        NULL);
    XmComboBoxUpdate(labelsList);
#endif

    for (i = 0; i < count; i++)
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

#ifndef R3B
    nItems = (whichType != BIFURCATION) ? 
	              mySolNode.nar+mySolNode.npar+specialColorItems : 
				  myBifNode.nar+specialColorItems;
    count = XtNumber (coloringMethodList);
    clrMethodList = (XmStringTable) XtMalloc(count * sizeof (XmString *));

    for (i = 0; i < count; ++i)
        clrMethodList[i] = XmStringCreateLocalized (coloringMethodList[i]);

    colorMethodSeletionList=XtVaCreateManagedWidget ("coloringMethodlist",
        xmComboBoxWidgetClass, listCarrier,
        XmNcomboBoxType,       XmDROP_DOWN_COMBO_BOX,
        XmNitemCount,          nItems,
        XmNitems,              clrMethodList,
        XmNcolumns,            5,
        XmNmarginHeight,       1,
        XmNselectedPosition,   coloringMethod+specialColorItems,
        XmNpositionMode,       XmZERO_BASED,
        NULL);

#ifdef LESSTIF_VERSION
    list = CB_List(colorMethodSeletionList);
    XmListDeleteAllItems(list);
    for (i = 0; i < nItems; i++)
	XmListAddItem(list,clrMethodList[i],0);
    XtVaSetValues(colorMethodSeletionList,
        XmNvisibleItemCount,   10,
        XmNwidth,              80,
        XmNcolumns,            5,
        NULL);
    XmComboBoxUpdate(colorMethodSeletionList);
#endif

    XtAddCallback (colorMethodSeletionList, XmNselectionCallback,
        colorMethodSelectionCB, NULL);

//-----------------------------------------------------Nov 06
#endif
// build the numPeriodAnimated drop down list
    nItems = 7;
    count = nItems;
    char (*numberPList)[5] = new char [count][5];
    XmStringTable numPList = (XmStringTable) XtMalloc(count * sizeof (XmString *));

    int iam = 1;
    sprintf(numberPList[0], "%i", 0);
    for (i = 0; i < count-2; ++i)
    {
        sprintf(numberPList[i+1], "%i", iam);
        iam *= 2;
    }
    sprintf(numberPList[count-1], "%s", "inf");

    for (i = 0; i < count; ++i)
        numPList[i] = XmStringCreateLocalized (numberPList[i]);

    if (numPeriodAnimated > 0)
        i = ((int)(log(numPeriodAnimated)/log(2))) + 1;
    else if (numPeriodAnimated == 0)
        i = 0;
    else
        i = count - 1;

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

#ifdef LESSTIF_VERSION
    list = CB_List(numPeriodAnimatedList);
    XmListDeleteAllItems(list);
    for (i = 0; i < nItems; i++)
	XmListAddItem(list,numPList[i],0);
    XtVaSetValues(numPeriodAnimatedList,
        XmNvisibleItemCount,   10,
        XmNwidth,              70,
        XmNcolumns,            3,
        NULL);
    XmComboBoxUpdate(numPeriodAnimatedList);
#endif

// Add Callback function for the numberPeriodAnimated drop down list
    XtAddCallback (numPeriodAnimatedList, XmNselectionCallback,
        numPeriodAnimatedCB, NULL);

    for (i = 0; i < count; i++)
        XmStringFree(numPList[i]);
    XtFree((char *)numPList);
//    delete []numberPList;
//----------------------------------------------------------------> Nov 06 End

// build the COLORING Method drop down list
#ifdef R3B
    nItems = (whichType != BIFURCATION) ?
        mySolNode.nar+mySolNode.npar+specialColorItems :
    myBifNode.nar+specialColorItems ;
    count = XtNumber (coloringMethodList);
    clrMethodList = (XmStringTable) XtMalloc(count * sizeof (XmString *));

    for (i = 0; i < count; ++i)
        clrMethodList[i] = XmStringCreateLocalized (coloringMethodList[i]);

    colorMethodSeletionList=XtVaCreateManagedWidget ("coloringMethodlist",
        xmComboBoxWidgetClass, listCarrier,
        XmNcomboBoxType,       XmDROP_DOWN_COMBO_BOX,
        XmNitemCount,          nItems,
        XmNitems,              clrMethodList,
        XmNcolumns,            5,
        XmNmarginHeight,       1,
        XmNselectedPosition,   coloringMethod+specialColorItems,
        XmNpositionMode,       XmZERO_BASED,
        NULL);

#ifdef LESSTIF_VERSION
    list = CB_List(colorMethodSeletionList);
    XmListDeleteAllItems(list);
    for (i = 0; i < nItems; i++)
        XmListAddItem(list,clrMethodList[i],0);
    XtVaSetValues(colorMethodSeletionList,
        XmNvisibleItemCount,   10,
        XmNwidth,              80,
        XmNcolumns,            5,
        NULL);
    XmComboBoxUpdate(colorMethodSeletionList);
#endif

// Add Callback function for the coloring method seletion drop down list
    XtAddCallback (colorMethodSeletionList, XmNselectionCallback,
        colorMethodSelectionCB, NULL);

// create labels for the x, y, z, and labels drop down lists
#endif
    Widget xLbl = XtVaCreateManagedWidget("X",xmLabelWidgetClass, listCarrier, NULL);
    Widget yLbl = XtVaCreateManagedWidget("Y",xmLabelWidgetClass, listCarrier, NULL);
    Widget zLbl = XtVaCreateManagedWidget("Z",xmLabelWidgetClass, listCarrier, NULL);
    Widget lLbl = XtVaCreateManagedWidget("Label",xmLabelWidgetClass, listCarrier, NULL);
    Widget colorLbl = XtVaCreateManagedWidget("Color",xmLabelWidgetClass, listCarrier, NULL);
    Widget numPeriodLbl = XtVaCreateManagedWidget("Period",xmLabelWidgetClass, listCarrier, NULL);

    Widget orbitSldLbl = XtVaCreateManagedWidget("Orbit",xmLabelWidgetClass, listCarrier, NULL);
#ifndef R3B
    Widget satSldLbl = XtVaCreateManagedWidget("Anim",xmLabelWidgetClass, listCarrier, NULL);
#else
    Widget satSldLbl = XtVaCreateManagedWidget("Sat ",xmLabelWidgetClass, listCarrier, NULL);
#endif
    Widget spLbl   = XtVaCreateManagedWidget("   Line  ",xmLabelWidgetClass, listCarrier, NULL);
    Widget spLbl2  = XtVaCreateManagedWidget("Thickness",xmLabelWidgetClass, listCarrier, NULL);

#ifdef R3B
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

#ifdef R3B
#ifdef USE_BK_COLOR
    XtVaSetValues (satAniSpeedSlider, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
#endif
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

#ifdef R3B
#ifdef USE_BK_COLOR
    XtVaSetValues (orbitAniSpeedSlider, XtVaTypedArg,
        XmNbackground, XmRString, "white", 6,
        NULL);
#endif
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
    Widget spinBox = XmCreateSpinBox (listCarrier, "spinBox", args, 1);
    Widget tf = XmCreateTextField(spinBox, "tf", args+1, n-1);
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
#ifdef R3B
    renderArea->setTransparencyType(SoGLRenderAction::SORTED_OBJECT_BLEND);
#endif

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

#ifndef R3B
    updateScene();
#endif
// these two lines are the third method for showing in 2D/3D
    renderArea->setSceneGraph(sceneGraph);
    renderArea->show();

    XtManageChild(topform);

    return renderArea;
}


////////////////////////////////////////////////////////////////////////
//
//     When the line color changed, this function will be raised.
//
void
linePatternToggledCB(Widget w, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct*) call_data;
    int value;
    int which = (long)client_data;
    int lineNumber=0, columnNumber =0;

#ifdef LESSTIF_VERSION
    cbs->set == XmSET ? "on" : cbs->set==0 ? "off" : "interminate";
#else
    cbs->set == XmSET ? "on" : cbs->set==XmOFF ? "off" : "interminate";
#endif
}


////////////////////////////////////////////////////////////////////////
//
//     When the line color changed, this function will be raised.
//
void
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
void
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
Widget
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

    Widget lpSpinBox = XmCreateSimpleSpinBox (form, (char *)"lpsimple", args, n);
    XtAddCallback (lpSpinBox, XmNvalueChangedCallback, linePatternValueChangedCB, NULL);

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
Widget 
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
void
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
void
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
void
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
void
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
Widget
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


#ifdef R3B
////////////////////////////////////////////////////////////////////////
//
void
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
void
createGraphCoordinateSystemFrameGuts(Widget frame)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[12];
    Widget label;
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

    for (int i = 0; i < XtNumber (coordSysItems); i++)
    {
        n = 0;
        if (whichCoordSystem == (unsigned long)i) XtSetArg(args[n++], XmNset, XmSET);
        Widget w = XmCreateToggleButtonGadget (toggleBox, (char *)coordSysItems[i], args, n);
        XtAddCallback (w, XmNvalueChangedCallback, graphCoordinateSystemToggledCB, (XtPointer) i);
        XtManageChild (w);
    }

    XtManageChild (toggleBox);
    XtManageChild (frame);
}
#endif

////////////////////////////////////////////////////////////////////////
//
void
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
void
createGraphStyleFrameGuts(Widget frame)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[12];
    Widget label;
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

    for (int i = 0; i < XtNumber (graphStyleItems); i++)
    {
        n = 0;
        if (whichStyle == i) XtSetArg(args[n++], XmNset, XmSET);
        Widget w = XmCreateToggleButtonGadget (toggleBox, (char *)graphStyleItems[i], args, n);
        XtAddCallback (w, XmNvalueChangedCallback, graphStyleWidgetToggledCB, (XtPointer) i);
        XtManageChild (w);
    }

    XtManageChild (toggleBox);
    XtManageChild (frame);
}


////////////////////////////////////////////////////////////////////////
//
void
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
void
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
void
createGraphTypeFrameGuts(Widget frame)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[12];
    Widget label;
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

    for (int i = 0; i < XtNumber (graphTypeItems); i++)
    {
        n = 0;
        if (whichType == i) XtSetArg(args[n++], XmNset, XmSET);
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
void
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
void
createOptionFrameGuts(Widget frame)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[12];
    Widget label;

// create default selections
    n = 0;
    XtSetArg (args[n], XmNpacking, XmPACK_COLUMN); ++n;
    XtSetArg (args[n], XmNnumColumns, 4); ++n;
    Widget toggleBox = XmCreateRowColumn (frame, (char *)"togglebox", args, n);

    for (int i = 0; i < XtNumber (graphWidgetItems); i++)
    {
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
void
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
void
createGraphCoordPartsFrameGuts(Widget frame)
//
////////////////////////////////////////////////////////////////////////
{
    int n;
    Arg args[12];
    Widget label;
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

    for (int i = 0; i < XtNumber (coordItems); i++)
    {
        n = 0;
        if (whichCoord == (unsigned long)i) XtSetArg(args[n++], XmNset, XmSET);
        Widget w = XmCreateToggleButtonGadget (toggleBox1, (char *)coordItems[i], args, n);
        XtAddCallback (w, XmNvalueChangedCallback, graphCoordWidgetToggledCB, (XtPointer) i);
        XtManageChild (w);
    }

    XtManageChild (toggleBox1);
    XtManageChild (frame);
}


////////////////////////////////////////////////////////////////////////
//
void
createPreferDefaultPages(Widget parent)
//
////////////////////////////////////////////////////////////////////////
{
    Widget frameList[6];
    int num;
    const char * frmNames[]=
    {
        "Optional Widgets", "Graph Type", "Graph Style",
#ifndef R3B
        "Coordinate Parts", "Others"
#else
        "Coordinate System", "Coordinate Parts", "Others"
#endif
    };

    for(int i=0; i<XtNumber(frmNames); ++i)
        frameList[i] = createPreferDefaultPageFrames(parent, frmNames[i]);
    num = 0;
    createOptionFrameGuts(frameList[num++]);
    createGraphTypeFrameGuts(frameList[num++]);
    createGraphStyleFrameGuts(frameList[num++]);
#ifdef R3B
    createGraphCoordinateSystemFrameGuts(frameList[num++]);
#endif
    createGraphCoordPartsFrameGuts(frameList[num++]);
    layoutPreferDefaultPageFrames(frameList, num);
}


////////////////////////////////////////////////////////////////////////
//
void
createLineAttPages(Widget parent)
//
////////////////////////////////////////////////////////////////////////
{
    const char *tabName[] = { "Line Attributes", "Other Preferences" };
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
    if(coloringMethod == CL_BRANCH_NUMBER)
        createLineAttrPrefSheetParts(widgetList, num, parent, names2);
    else
        createLineAttrPrefSheetParts(widgetList, num, parent, names);

    layoutLineAttrPartsOnThePrefSheet(widgetList, num, parent);
}


///////////////////////////////////////////////////////////////////////
//
void
createPreferNotebookPages(Widget notebook)
//
////////////////////////////////////////////////////////////////////////
{
// create the preference sheet shell and form widget
    Widget pageForm[2], tab, form;
    int n=0;
    char         buffer[32];
    XmString     xms;
    Arg          args[14];
    const char *tabName[] = { "Menu Item Preferences", "Line Attributes" };

// create the first page.
    n = 0;
    XtSetArg(args[n], XmNmarginHeight, 15); ++n;
    XtSetArg(args[n], XmNmarginWidth,  15); ++n;
    pageForm[0] = XmCreateForm(notebook, (char *)"page", args, n);

    n = 0;
    XtSetArg (args[n], XmNnotebookChildType, XmMINOR_TAB); n++;
    sprintf (buffer, "%s", tabName[0] );
    tab = XmCreatePushButton (notebook, buffer, args, n);

    createPreferDefaultPages(pageForm[0]);
    XtManageChild (tab);

// create the second page.
    n = 0;
    XtSetArg(args[n], XmNmarginHeight, 15); ++n;
    XtSetArg(args[n], XmNmarginWidth,  15); ++n;
    pageForm[1] = XmCreateForm(notebook, (char *)"page", args, n);

    n=0;
    XtSetArg (args[n], XmNnotebookChildType, XmMINOR_TAB); n++;
    sprintf (buffer, "%s", tabName[1] );
    tab = XmCreatePushButton (notebook, buffer, args, n);
    createLineAttPages(pageForm[1]);
    XtManageChild (tab);

    XtManageChildren (pageForm, 2);
}


////////////////////////////////////////////////////////////////////////
//
void
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
void
createPreferDialog()
//
////////////////////////////////////////////////////////////////////////
{
    static Widget shell;

//    if(!shell)
    {
        Widget notebook, actionForm, tab, panedWin;
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
void
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

#ifdef R3B
    whichCoordSystem     = whichCoordSystemOld;
    whichCoordSystemTemp = whichCoordSystemOld;
    coordSystemMenuItems->which           = whichCoordSystemOld;

#endif
    whichCoord     = whichCoordOld;
    whichCoordTemp = whichCoordOld;
    coordMenuItems->which           = whichCoordOld;

// cancel the selections and recover the original values.
    graphWidgetToggleSetTemp = graphWidgetToggleSetOld;
    graphWidgetToggleSet     = graphWidgetToggleSetOld;
    for (int i = 0; i < XtNumber (graphWidgetItems); i++)
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
void
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

#ifdef R3B
    whichCoordSystem    = whichCoordSystemTemp;
    whichCoordSystemOld = whichCoordSystemTemp;
    coordSystemMenuItems->which          = whichCoordSystemTemp;

#endif
    whichCoord = whichCoordTemp;
    whichCoordOld = whichCoordTemp;
    coordMenuItems->which        = whichCoordTemp;

    graphWidgetToggleSet    = graphWidgetToggleSetTemp;
    graphWidgetToggleSetOld = graphWidgetToggleSetTemp;
    for (int i = 0; i < XtNumber (graphWidgetItems); i++)
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
void
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

#ifdef R3B
    whichCoordSystem    = whichCoordSystemTemp;
    whichCoordSystemOld = whichCoordSystemTemp;
    coordSystemMenuItems->which          = whichCoordSystemTemp;

#endif
    whichCoord = whichCoordTemp;
    whichCoordOld = whichCoordTemp;
    coordMenuItems->which        = whichCoordTemp;

    graphWidgetToggleSet    = graphWidgetToggleSetTemp;
    graphWidgetToggleSetOld = graphWidgetToggleSetTemp;
    for (int i = 0; i < XtNumber (graphWidgetItems); i++)
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
void
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

#ifdef R3B
    whichCoordSystem = whichCoordSystemTemp;
    coordSystemMenuItems->which       = whichCoordSystemTemp;

#endif
    whichCoord = whichCoordTemp;
    coordMenuItems->which        = whichCoordTemp;

    graphWidgetToggleSet = graphWidgetToggleSetTemp;
    for (int i = 0; i < XtNumber (graphWidgetItems); i++)
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
void
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
void
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
#ifdef R3B
    else if(fileMode == PRINT_ITEM)
    {
        cropScene(filename);
    }
#endif
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
    void          showAboutCB(Widget, XtPointer, XtPointer);
    unsigned char modality = (unsigned char)XmDIALOG_FULL_APPLICATION_MODAL;
    char str[600];
#ifndef R3B
    strcpy(str,"  AUTO plaut04\n\n");
#else
    strcpy(str,"  AUTO r3bplaut04\n\n");
#endif
    strcat(str,"  Zhang, Chenghai, Dr. Eusebius J. Doedel\n\n ");
    strcat(str,"  Computer Science Department\n");
    strcat(str,"  Concordia University\n\n");
#ifndef R3B
    strcat(str,"  Montreal, Quebec\n");
    strcat(str,"  CANADA\n\n");
    strcat(str,"  August, 2004 \n");

#else
    strcat(str,"  Montreal, CA\n\n");
    strcat(str,"  June, 2004 \n");
#endif

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


    t = XmStringCreateLocalized (str);
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
        int i = 0;
        for(i=0; i<MAX_LIST; i++)
            xCoordIndices[i] = -1;

        i = 0;
        char * tmp;
        tmp = strtok(manyChoice, ",");
        do
        {
            xCoordIndices[i++] = (strcasecmp(tmp,"t")==0) ? 0 : atoi(tmp);
            tmp = strtok(NULL,",");
        }while(tmp != NULL && i < MAX_LIST); 
        xCoordIdxSize = i;
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
        int i = 0;
        for(i=0; i<MAX_LIST; i++)
            yCoordIndices[i] = -1;

        i = 0;
        char * tmp;
        tmp = strtok(manyChoice, ",");
        do
        {
            yCoordIndices[i++] = (strcasecmp(tmp,"t")==0) ? 0 : atoi(tmp);
            tmp = strtok(NULL,",");
        }while(tmp != NULL && i < MAX_LIST);
        yCoordIdxSize = i;
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
        int i = 0;
        for(i=0; i<MAX_LIST; i++)
            zCoordIndices[i] = -1;

        i = 0;
        char * tmp;
        tmp = strtok(manyChoice, ",");
        do
        {
            zCoordIndices[i++] = (strcasecmp(tmp,"t")==0) ? 0 : atoi(tmp);
            tmp = strtok(NULL,",");
        }while(tmp != NULL && i < MAX_LIST);
        zCoordIdxSize = i;
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
    int choice = (int) cbs->item_position;
    char *manyChoice = (char *) XmStringUnparse (cbs->item_or_text, XmFONTLIST_DEFAULT_TAG,
        XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);

    int i = 0;
    char * tmp;
    static int half = 2;
    tmp = strtok(manyChoice, ",");
    if(choice == 0)
    {
        do
        {
            lblIndices[i++] = (strcasecmp(tmp,"all")==0) ? 0 : atoi(tmp)-myLabels[1]+1;
            tmp = strtok(NULL,",");
        }while(tmp != NULL && i < MAX_LABEL);
        half = 2;
    }
    else if(choice == 1) 
    {
        int j = 1;
        do
        {
#ifdef R3B
            if(abs(clientData.labelIndex[j][2])!= 4 || j%half == 0)
#else
            if(abs(clientData.labelIndex[j-1][2])!= 4 || j%half == 0)
#endif
                lblIndices[i++] = j;
            j++;
        } while( j < numLabels-2 );

        half *= 2;
    }
    else if(choice == 2)
    {
        int j = 1;
        do
        {
#ifndef R3B
            if(clientData.labelIndex[j-1][2] !=  TYPE_UZ  && clientData.labelIndex[j-1][2] != TYPE_RG
            ) // &&
            // clientData.labelIndex[j-1][2] != TYPE_EP_ODE && clientData.labelIndex[j-1][2] != TYPE_MX)
#else
            if(clientData.labelIndex[j][2] !=  TYPE_UZ  && clientData.labelIndex[j][2] != TYPE_RG
            &&
                clientData.labelIndex[j][2] != TYPE_EP_ODE && clientData.labelIndex[j][2] != TYPE_MX)
#endif
                lblIndices[i++] = j;
            j++;
        } while( j < numLabels-2 );
        half = 2;
    }
    else if(choice == 3)
    {
        lblIndices[i++] = numLabels;
        half = 2;
    }
    else
    {
        lblIndices[i++] = choice-3;
        half = 2;
    }
    lblIdxSize = i;

    updateScene();
}


////////////////////////////////////////////////////////////////////////
//
void
showAboutCB(Widget dialog, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XtUnmanageChild (dialog);
}


////////////////////////////////////////////////////////////////////////
//
void 
hidenDialogShell (Widget widget, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XtUnmanageChild (XtParent(widget));
}


////////////////////////////////////////////////////////////////////////
//
void 
redrawFloqueMultipliers (Widget fmDrawingArea, XtPointer client_data, XtPointer call_data)
//
////////////////////////////////////////////////////////////////////////
{
    XmDrawingAreaCallbackStruct *cbs = (XmDrawingAreaCallbackStruct *) call_data;
    bool eigenvalue = (bool)(long)client_data;

    XPoint points[8]={ 0,0, 100, 0, 0,100, 100,100};

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
    if (!eigenvalue) {
        XSetLineAttributes(cbs->event->xexpose.display, gc, 1, LineSolid, CapRound, JoinRound);
        XDrawArc(cbs->event->xexpose.display, cbs->window, gc, 150, 150, 100, 100, 360*64, 360*64);
    }

    XSetForeground(cbs->event->xexpose.display, gc, black.pixel);

    int x, y;

    XSetLineAttributes(cbs->event->xexpose.display, gc, 2, LineSolid, CapRound, JoinRound);
    for(int j = 0; j<clientData.numFM; ++j)
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
popupFloquetMultiplierDialog(float data[], int size, bool eigenvalue)
//
////////////////////////////////////////////////////////////////////////
{

    static Widget dialog_shell = (Widget) 0 ;
    Widget pane = (Widget) 0 ;              
    XmString      str1;
    void          showAboutCB(Widget, XtPointer, XtPointer);
    unsigned char modality = (unsigned char)XmDIALOG_FULL_APPLICATION_MODAL;
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

    char *tmpstr, tempchar[500];
    tmpstr = new char[500];
    tmpstr[0]='\0';
    if (eigenvalue)
        strcat(tmpstr,"Eigenvalues:\n" );
    else
        strcat(tmpstr,"Floquet multipliers:\n" );
    for(int j=0; j<clientData.numFM; ++j)
    {
        strcat(tmpstr," [");
        sprintf(temp,"%2d",j);
        strcat(tmpstr,temp);
        strcat(tmpstr,"] : ");
        sprintf(temp,"%E",fmData[j*2]);
        strcat(tmpstr,temp);
        if(fmData[j*2+1] != 0) {
            strcat(tmpstr,fmData[j*2+1] < 0 ? " - " : " + ");
            sprintf(temp,"%E",abs(fmData[j*2+1]));
            strcat(tmpstr,temp);
            strcat(tmpstr,"i\n");
        }
    }

    if (!pane)
    {
        Widget fmDrawingArea;
        Arg args[5];
        Pixel fg,bg;
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

        XtAddCallback (fmDrawingArea, XmNexposeCallback, redrawFloqueMultipliers, (XtPointer)eigenvalue);

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


void soxtmain(char *argv[])
{
// Initialize Inventor and Xt.
    Widget  mainWindow;
    mainWindow = SoXt::init(argv[0]);

    if (mainWindow != NULL)
    {
#ifndef R3B
        root = new SoSeparator;

        SoSeparator * rootroot = new SoSeparator;
        rootroot->ref();
#else
        root = new SoSelection;
        SoSeparator * myroot = new SoSeparator;
        myroot->ref();
#endif

#ifndef R3B
        root->ref();
#endif
        SoEventCallback *mouseEventCB = new SoEventCallback;
#ifndef R3B
        rootroot->addChild(mouseEventCB);
        rootroot->addChild(root);
#else
        myroot->addChild(mouseEventCB);
        myroot->addChild(root);
#endif

#ifndef R3B
        SoXtRenderArea *ra = buildMainWindow(mainWindow, rootroot);
#else
#ifdef USE_BK_COLOR
        XtVaSetValues (mainWindow, XtVaTypedArg,
            XmNbackground, XmRString, "white", 6, NULL);
#endif

        updateScene();

        SoXtRenderArea *ra = buildMainWindow(mainWindow, myroot);
#endif

        mouseEventCB->addEventCallback(
            SoMouseButtonEvent::getClassTypeId(),
            myMousePressCB,
            ra->getSceneManager()->getSceneGraph());

        SoXt::show(mainWindow);
        SoXt::mainLoop();
    }
}
