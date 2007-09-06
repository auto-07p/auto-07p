
#ifndef AGE_H
#define AGE_H

#include <ctype.h>
#include <time.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h> // for access()

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


#include <Inventor/So.h>
#include <Inventor/SoOffscreenRenderer.h>
#include <Inventor/SbViewportRegion.h>
#include <Inventor/SbLinear.h>
#include <Inventor/Xt/SoXt.h>
#include <Inventor/Xt/viewers/SoXtExaminerViewer.h>
#include <Inventor/Xt/SoXtMaterialEditor.h>
#include <Inventor/SoPickedPoint.h>
#include <Inventor/actions/SoRayPickAction.h>
#include <Inventor/actions/SoWriteAction.h>
#include <Inventor/actions/SoBoxHighlightRenderAction.h>
#include <Inventor/actions/SoLineHighlightRenderAction.h>

#include <Inventor/engines/SoTimeCounter.h>
#include <Inventor/engines/SoSelectOne.h>
//#include <Inventor/engines/SoInterpolate.h>
//#include <Inventor/engines/SoElapsedTime.h>
//#include <Inventor/engines/SoOneShot.h>
#include <Inventor/engines/SoCalculator.h>


#include <Inventor/events/SoMouseButtonEvent.h>
#include <Inventor/fields/SoField.h>
//#include <Inventor/manips/SoTrackballManip.h>
#include <Inventor/nodes/SoEventCallback.h>
#include <Inventor/nodes/SoSelection.h>

#include <Inventor/nodes/SoFont.h>
#include <Inventor/nodes/SoText2.h>

#define SCREEN(w) XScreenNumberOfScreen(XtScreen(w))

#define NDEBUG

// ENABLE SOME SPECIAL FUNCTION FOR THE GRAPH.
#define LIST_UNDER_MENUBAR
#define USE_EXAM_VIEWER 

//
//  Macros to set Motif toggle buttons on or off
//
#define TOGGLE_ON(BUTTON) \
    XmToggleButtonSetState((Widget) BUTTON, TRUE, FALSE)
#define TOGGLE_OFF(BUTTON) \
    XmToggleButtonSetState((Widget) BUTTON, FALSE, FALSE)

//
//  Menu item constants - each item in a menu gets a unique id
//
#define OPEN_ITEM     0
#define SAVE_ITEM     1
#define PRINT_ITEM    2
#define QUIT_ITEM     3

#define ITEM_ONE      0
#define ITEM_TWO      1
#define ITEM_THREE    2
#define ITEM_FOUR     3

#define max(a,b) (a>b?a:b)
#define min(a,b) (a<b?a:b)
#define MAX_BUF  256   // length of a line
#define MAX_PAR  100    // Max Number of parameters in AUTO 
#define MAX_BRANCH 100    // Max Number of branches in AUTO DATA

#define WIN_WIDTH  1000
#define WIN_HEIGHT 1000 

#ifdef R3B
#define MAX_LIST  3000  // Max number for x-coor, y-coord, z-coord, coloringMethod,  lists.
#define MAX_LABEL 50000  // maxinum number of labels in a solution file
#else
#define MAX_LIST  1000  // Max number for x-coor, y-coord, z-coord, coloringMethod,  lists.
#define MAX_LABEL 10000  // maxinum number of labels in a solution file
#endif

#ifndef M_PI
    #define M_PI 3.1415926
#endif

#ifndef M_PI_2
    #define M_PI_2 1.5707963
#endif

#ifdef R3B
#define MY_NONE -1
#define MY_HALF -2
#define MY_SPEC -3
#else
#define MY_NONE -3
#define MY_SPEC -2
#define MY_HALF -1
#define MY_ALL  0
#endif

#define SOLUTION 0
#define BIFURCATION 1

// define constant for the coloringMethod
#ifdef R3B
#define CL_STABILITY     -5
#define CL_POINT_NUMBER  -4
#define CL_BRANCH_NUMBER -3
#define CL_ORBIT_TYPE    -2 
#define CL_LABELS -1
#else
#define CL_STABILITY     -6
#define CL_POINT_NUMBER  -5
#define CL_BRANCH_NUMBER -4
#define CL_ORBIT_TYPE    -3 
#define CL_LABELS        -2
#define CL_COMPONENT     -1
#endif

#define LINE 0
#define TUBE 1
#define SURFACE 2
#define MESH_POINTS 3
#define ALL_POINTS 4
#define NURBS 5

#ifdef R3B
#define ROTATING_F  0  //CR3BP 0
#define INERTIAL_B  1
#define INERTIAL_S	2  // big primary centered, sun
#define INERTIAL_E	3  // small primary centered, earth.

#define OPT_DRAW_COORD 0

#define OPT_REF_PLAN   0
#define OPT_PRIMARY    1
#define OPT_LIB_POINTS  2
#define OPT_PERIOD_ANI  3
#define OPT_SAT_ANI    4
#define OPT_BACKGROUND   5
#define OPT_LEGEND   6
#define OPT_NORMALIZE_DATA  7
#define OPT_EARTH_MOVE  8 
#define OPT_ANIMATE_CALCULATION  9
#else
#define OPT_PERIOD_ANI  0
#define OPT_SAT_ANI    1
#define OPT_BACKGROUND   2
#define OPT_LEGEND   3
#define OPT_NORMALIZE_DATA  4
#endif

#define NO_COORD  0 
#define COORDORIGIN    1
#define LEFTBACK  2
#define LEFTAHEAD 3
#define DRAW_TICKER 4
#define GEOCENTER 5

#define TYPE_BP_ALG   1
#define TYPE_LP_ALG   2
#define TYPE_HB_ALG   3
#define TYPE_RG       4
#define TYPE_UZ       -4
#define TYPE_LP_ODE   5
#define TYPE_BP_ODE   6
#define TYPE_PD_ODE   7
#define TYPE_TR_ODE   8
#define TYPE_EP_ODE   9
#define TYPE_MX       -9


struct solution{
  long position;
  int nrowpr;
  int branch;
  int point;
  int type;
  int label;
  int new_label;
  struct solution *next;
};
                                                                                                                       
typedef struct solution *solutionp;


struct SolNode {
#ifdef R3B
    float mass[MAX_LABEL];
#endif
    double *time; // this time is useless?
    float (*xyzCoords)[3];
    int32_t numVerticesEachBranch[MAX_BRANCH];  // index start from 0 
    int32_t numVerticesEachPeriod[MAX_LABEL];  // index start from 0
    int32_t numOrbitsInEachBranch[MAX_BRANCH];  // index start from 0
    int *xAxisItems;
    int *yAxisItems;
    int *zAxisItems;
    int nar;
	int npar;

    long int totalNumPoints; // like 10012.
    long int numBranches;    // like 4.
    long int branchID[MAX_BRANCH]; // for each label/solution
    long int numOrbits;      // like 43 == UserData.totalLabels == BifNode.totalLabels == totalLabels.
	long int labels[MAX_LABEL];   // this real lenght should equal to numOrbits; 
							// labels[0]==0, HERE, we make an assumption that no label equal to 0.
	long int ntst[MAX_LABEL];   
	long int ncol[MAX_LABEL];   

    int      numAxis;        // number of groups of axis. 3 is a group.
	int      totalLabels;
	double par[MAX_LABEL][MAX_PAR];     // keep the parameter values for each orbit.
	double parMax[MAX_BRANCH][MAX_PAR];     // keep the max parameter values in each branch.
	double parMin[MAX_BRANCH][MAX_PAR];     // keep the min parameter values.
	double parMid[MAX_BRANCH][MAX_PAR];     // keep the mid parameter values.
	int      parID[MAX_PAR];    // keep the id for the par, namely the x for par(x)

    // max saves the maximum value of the coordinate.
    // min saves the minimum value of the coordinate.
    float max[3], min[3];
};


struct BifNode {
    float (*xyzCoords)[3];
    int32_t numVerticesEachBranch[MAX_BRANCH]; 
    int32_t numVerticesEachLabelInterval[MAX_LABEL]; 
    int *xAxisItems;
    int *yAxisItems;
    int *zAxisItems;
    int nar;

	int * ptStability;       // start from 0...totalLines-1
    long int totalNumPoints; // like 10012.
    long int numBranches;    // like 4.
    long int branchID[MAX_BRANCH]; // branch ID of each branch.
	long int labels[MAX_LABEL];   // this real lenght should equal to totalLabels;
    int      numAxis;        // number of groups of axis. 3 is a group.
	int totalLabels;

    // max saves the maximum value of the coordinate.
    // min saves the minimum value of the coordinate.
    float max[3], min[3];
};

////////////////////////////////////////////////////////////////////////
//
// This struct stores the userdata by using a 
// two dismensional dynamic array.
struct UserData{
    long int maxRowSize;
    int maxColSize;
    int totalLabels;
    int numFM;

    float **solData;
    float **bifData;

    float solPeriod[MAX_LABEL];        // from the solution file par[10];
    float (*multipliers)[6][2];   // from the diagonstic file.
    long int labelIndex[MAX_LABEL][4]; // [0] --- saves the start row number in the solData, 
	                              // [1] --- saves the row number in the bifData.
	                              // [2] --- saves the type of the label.
								  // [3] --- saves the stability of the solution.
    float *solMax, *solMin;								
};
//
////////////////////////////////////////////////////////////////////////


struct coord3{
    float x ;
    float y ;
    float z ;
};
    
struct TubeNode{
    coord3 translation;
    coord3 axis;
    float angle;
    float height;
    float radius;
};

void pointsToAxisAngle(/*in*/ float *a, /*in*/ float *b, /*out*/TubeNode& cnode);
void pointToPoints(float a[], float b[], float c[][3]);

solutionp parseSolution(const char * sFileName, bool &bl, long int &total,long int &totalNumPoints);
bool readSolution(/*in*/ solutionp current,
                  /*in*/ const char* sFileName, 
                  /*in SolNode& mySolNode,*/
                  /*in*/ int varIndices[]);
bool parseBifurcation(const char *bFileName);
bool readBifurcation(const char *bFileName, int varIndices[]);
int readFM(const char *bFileName ,const int);

void normalizeSolData();//SolNode);
void normalizeBifData();//BifNode);
#ifdef R3B
void toInertialFrame(int , SolNode);
#endif


SoSeparator * createAxis(float red, float green, float blue);
SoSeparator * drawCoords(int where, float pos[], SbVec3f colors[], float height); //in/SolNode& mySolNode);
#ifdef R3B
SoSeparator * createDisk(float where[], float scaler);
 
/************************************************************************
//
//  The edit menu has radio buttons. Rather than use the radio
//  buttons which Motif supplies, we handle things ourselves.
//
typedef struct EditMenuItems {
    Widget *items;  // all the items in this menu
    int       which;   // specifies which radio button is on
} EditMenuItems;
************************************************************************/
#endif

SoSeparator * createSolutionScene();//in/SolNode& mySolNode);//float (*xyzCoords)[3]);
SoSeparator * createBifurcationScene();///in/SolNode& mySolNode)//float (*xyzCoords)[3])
SoSeparator * createDisk();
SoSeparator * renderSolution();//in/SolNode& mySolNode);//float (*xyzCoords)[3]);
SoSeparator * renderBifurcation(); //BifNode &myBifNode)// float (*xyzCoords)[3])

#ifdef R3B
void smallPrimaryMovingOrbit(float R, float T, float t,
                      float position[], float veloctiy[]);
void satelliteMovingOrbit(int whichcenter, float xyzCoords[],
                     float t, float mu, float R, float Ts, float T, float g,
                     float r[], float v[]);
#endif

#endif 
