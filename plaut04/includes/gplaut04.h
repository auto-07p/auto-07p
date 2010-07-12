
#ifndef PLAUT04_H
#define PLAUT04_H

#include <algorithm>
#include <string>
#include <vector>

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

#define WIN_WIDTH  1000
#define WIN_HEIGHT 1000 

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
#define CL_BRANCH_NUMBER -7
#define CL_STABILITY     -6
#define CL_POINT_NUMBER  -5
#define CL_CURVE_NUMBER  -4
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

#include "r3bplaut04.h"

#define NO_COORD  0 
#define COORDORIGIN    1
#define LEFTBACK  2
#define LEFTAHEAD 3
#define COORD_AT_ORIGIN    4
#define DRAW_TICKER 5
#define GEOCENTER 6

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

#define TIME_IS_OFF  0 
#define TIME_ON_X    1
#define TIME_ON_Y    2
#define TIME_ON_Z    3

////////////////////////////////////////////////////////////////////////
//
struct UserData{
    float (*multipliers)[2];           // from the diagnostic file.
    int maxndim;
    int *numFM;                        // #multipliers (+) or #eigenvalues (-)?
    long int (*labelIndex)[4];        // [0] --- saves the start row number in the solData, 
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

// gmain.c++
SoGroup * setLineAttributesByStability(int stability, float scaler);
SoGroup * setLineAttributesByParameterValue(double parValue, double parMax, double parMid, double parMin, int stability, float scaler);
SoGroup * setLineAttributesByBranch(int iBranch, int stability, float scaler);
SoGroup * setLineAttributesByType(int stability, int type, float scaler);
SoGroup * setLineColorBlending(float * vertices, long int size, int stability, float scaler);
SoGroup * setLineColorBlendingByStability(float * vertices, long int size, int stab, float scaler);
SoSeparator * drawASphere(float ptb[], float size);
void updateScene();
void postDeals();
void showHelpDialog();
int writePreferValuesToFile();
void writeToFile(const char * fileName);
void deleteScene();
SbBool readFile(const char *filename);
void myMousePressCB(void *userData, SoEventCallback *eventCB);
double fortranatof(char* word);

// gmainqt.c++ & gmainxt.c++
void popupFloquetMultiplierDialog(float data[], int size, int numFM);
void soxtmain(int argc, char *argv[]);
void setListValue();

struct DefaultAxisItems
{
    std::vector<int> bifX, bifY, bifZ, solX, solY, solZ;
};

#define CL_SP_ITEMS  7
#define LBL_OFFSET   4
#define SP_LBL_ITEMS 4
#define NUM_SP_POINTS 13

extern float orbitSpeed, satSpeed, numPeriodAnimated, lineWidthScaler;
extern float fmData[12];
extern std::vector<int> xCoordIndices, yCoordIndices, zCoordIndices;
extern int coloringMethod, specialColorItems, coloringMethodType[2];
extern std::vector<int> lblIndices, lblChoice;
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
extern UserData clientData;
extern int MIN_ORBIT_SPEED, MAX_ORBIT_SPEED;
extern int MIN_SAT_SPEED, MAX_SAT_SPEED;
extern bool blDrawTicker;
extern struct DefaultAxisItems dai;
extern std::vector<std::string> coloringMethodList;
extern int *myLabels;
extern std::vector<std::string> xAxis, yAxis, zAxis;
extern bool optBif[11], optSol[11];
extern float satRadius, aniLineScaler;
extern std::vector<std::string> labels;
extern long int animationLabel;
extern const char *autoDir;
extern int whichCoordSystem, whichCoordSystemOld, whichCoordSystemTemp;
extern SoSeparator *root;
extern const char * graphWidgetItems[GRAPH_WIDGET_ITEMS];
extern float diskPosition[3];
extern float diskRadius;
extern float labelRadius;
extern bool useR3B;

#endif
