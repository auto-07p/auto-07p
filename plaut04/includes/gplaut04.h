
#ifndef PLAUT04_H
#define PLAUT04_H

#include <ctype.h>
#include <time.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h> // for access()

#include <Inventor/So.h>
#include <Inventor/SoOffscreenRenderer.h>
#include <Inventor/SbViewportRegion.h>
#include <Inventor/SbLinear.h>
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
#define MAX_PAR  100    // Max Number of parameters in AUTO 

#define WIN_WIDTH  1000
#define WIN_HEIGHT 1000 

#define MAX_LIST  3000  // Max number for x-coor, y-coord, z-coord, coloringMethod,  lists.
#define MAX_LABEL 50000  // maximum number of labels in a solution file

#ifndef M_PI
    #define M_PI 3.1415926
#endif

#ifndef M_PI_2
    #define M_PI_2 1.5707963
#endif

#define MY_NONE -1
#define MY_SPEC -2
#define MY_HALF -3
#define MY_ALL -4

#define SOLUTION 0
#define BIFURCATION 1

// define constant for the coloringMethod
#define CL_STABILITY     -6
#define CL_POINT_NUMBER  -5
#define CL_BRANCH_NUMBER -4
#define CL_ORBIT_TYPE    -3 
#define CL_LABELS        -2
#define CL_COMPONENT     -1

#define LINE 0
#define TUBE 1
#define SURFACE 2
#define MESH_POINTS 3
#define ALL_POINTS 4
#define NURBS 5

#define ROTATING_F  0  //CR3BP 0
#define INERTIAL_B  1
#define INERTIAL_S	2  // big primary centered, sun
#define INERTIAL_E	3  // small primary centered, earth.

#ifdef R3B
#include "r3bplaut04.h"
#else
#define GRAPH_WIDGET_ITEMS 7
#define OPT_PERIOD_ANI  0
#define OPT_SAT_ANI    1
#define OPT_DRAW_LABELS  2
#define OPT_LABEL_NUMBERS  3
#define OPT_BACKGROUND   4
#define OPT_LEGEND   5
#define OPT_NORMALIZE_DATA  6
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
    float mass[MAX_LABEL];
    double *time; // this time is useless?
    float (*xyzCoords)[3];
    int32_t *numVerticesEachBranch;  // index start from 0 
    int32_t numVerticesEachPeriod[MAX_LABEL];  // index start from 0
    int32_t *numOrbitsInEachBranch;  // index start from 0
    int *xAxisItems;
    int *yAxisItems;
    int *zAxisItems;
    int nar;
	int npar;

    long int totalNumPoints; // like 10012.
    long int numBranches;    // like 4.
    long int *branchID; // for each label/solution
    long int numOrbits;      // like 43 == UserData.totalLabels == BifNode.totalLabels == totalLabels.
	long int labels[MAX_LABEL];   // this real lenght should equal to numOrbits; 
							// labels[0]==0, HERE, we make an assumption that no label equal to 0.
	long int ntst[MAX_LABEL];   
	long int ncol[MAX_LABEL];   

    int      numAxis;        // number of groups of axis. 3 is a group.
	int      totalLabels;
	double par[MAX_LABEL][MAX_PAR];     // keep the parameter values for each orbit.
	double (*parMax)[MAX_PAR];     // keep the max parameter values in each branch.
	double (*parMin)[MAX_PAR];     // keep the min parameter values.
	double (*parMid)[MAX_PAR];     // keep the mid parameter values.
	int      parID[MAX_PAR];    // keep the id for the par, namely the x for par(x)

    // max saves the maximum value of the coordinate.
    // min saves the minimum value of the coordinate.
    float max[3], min[3];
};


struct BifNode {
    int varIndices[3];
    int32_t *numVerticesEachBranch;
    int32_t numVerticesEachLabelInterval[MAX_LABEL]; 
    int *xAxisItems;
    int *yAxisItems;
    int *zAxisItems;
    int nar;
    int maxndim;

    unsigned char *ptStability;       // start from 0...totalLines-1
    long int totalNumPoints; // like 10012.
    long int numBranches;    // like 4.
    long int *branchID; // branch ID of each branch.
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

    float **solData;
    float *bifData;

    float solPeriod[MAX_LABEL];        // from the solution file par[10];
    float (*multipliers)[2];           // from the diagnostic file.
    int maxndim;
    int *numFM;                        // #multipliers (+) or #eigenvalues (-)?
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
double fortranatof(char* word);

void normalizeSolData();//SolNode);
void normalizeBifData(long int idx, float xyzCoords[3]);//BifNode);

SoGroup * setLineAttributesByStability(int stability, float scaler);
SoGroup * setLineAttributesByParameterValue(double parValue, double parMax, double parMid, double parMin, int stability, float scaler);
SoGroup * setLineAttributesByBranch(int iBranch, int stability, float scaler);
SoGroup * setLineAttributesByType(int stability, int type, float scaler);
SoGroup * setLineColorBlending(float * vertices, long int size, int stability, float scaler);
SoSeparator * createAxis(float red, float green, float blue);
SoSeparator * drawCoords(int where, float pos[], SbVec3f colors[], float height); //in/SolNode& mySolNode);
SoSeparator * createLegend(SbVec3f pos, double val[5]);
SoSeparator * createDiscreteLegend(SbVec3f pos, SbColor lineColors[13]);
SoSeparator * createBranchLegend(SbVec3f pos, SbColor lineColors[13]);
SoSeparator * createStabilityLegend(SbVec3f pos, SbColor lineColors[2]);
SoSeparator * createCoordinates(bool, int type, float mx[3], float mn[3], int tk[3], int where);
SoSeparator * drawASphere(float ptb[], float size);

SoSeparator * createSolutionScene();//in/SolNode& mySolNode);//float (*xyzCoords)[3]);
SoSeparator * createDisk();

struct DefaultAxisItems
{
    int solXSize, solYSize, solZSize;
    int bifXSize, bifYSize, bifZSize;
    int bifX[MAX_PAR], bifY[MAX_PAR], bifZ[MAX_PAR];
    int solX[MAX_PAR], solY[MAX_PAR], solZ[MAX_PAR];
};

#define CL_SP_ITEMS  6
#define LBL_OFFSET   4
#define SP_LBL_ITEMS 4
#define NUM_SP_POINTS 13
void popupFloquetMultiplierDialog(float data[], int size, int numFM);
void soxtmain(char *argv[]);
void updateScene();
void postDeals();
void cropScene(const char* filename);
void setListValue();
void showHelpDialog();
int writePreferValuesToFile();
void writeToFile(const char * fileName);
void deleteScene();
SbBool readFile(const char *filename);
void myMousePressCB(void *userData, SoEventCallback *eventCB);
extern float orbitSpeed, satSpeed, numPeriodAnimated, lineWidthScaler;
extern float fmData[12];
extern int xCoordIndices[MAX_LIST], xCoordIdxSize;
extern int yCoordIndices[MAX_LIST], yCoordIdxSize;
extern int zCoordIndices[MAX_LIST], zCoordIdxSize;
extern int coloringMethod, specialColorItems, coloringMethodType[2];
extern int lblIndices[], lblChoice[], lblIdxSize;
extern bool options[];
extern long int numLabels;
extern bool setShow3D, setShow3DSol, setShow3DBif;
extern int whichType, whichTypeOld, whichTypeTemp;
extern int whichStyle, whichStyleOld, whichStyleTemp;
extern int whichCoord, whichCoordOld, whichCoordTemp;
extern int winWidth, winHeight;
extern unsigned long linePattern[], linePatternTemp[], linePatternOld[];
extern SbColor lineColor[], lineColorTemp[], lineColorOld[];
extern SbColor envColors[];
extern unsigned long graphWidgetToggleSet, graphWidgetToggleSetTemp,
  graphWidgetToggleSetOld;
extern SolNode mySolNode;
extern BifNode myBifNode;
extern UserData clientData;
extern int MIN_ORBIT_SPEED, MAX_ORBIT_SPEED;
extern int MIN_SAT_SPEED, MAX_SAT_SPEED;
extern bool blDrawTicker;
extern struct DefaultAxisItems dai;
extern char coloringMethodList[MAX_LIST+CL_SP_ITEMS][8];
extern int myLabels[MAX_LABEL+SP_LBL_ITEMS];
extern char xAxis[MAX_LIST][5], yAxis[MAX_LIST][5], zAxis[MAX_LIST][5];
extern bool optBif[11], optSol[11];
extern float satRadius, aniLineScaler;
extern solutionp solHead;
extern char labels[MAX_LABEL][8];
extern long int animationLabel;
extern char *autoDir;
extern int whichCoordSystem, whichCoordSystemOld, whichCoordSystemTemp;
extern SoSeparator *root;
extern const char * graphWidgetItems[GRAPH_WIDGET_ITEMS];

#endif 
