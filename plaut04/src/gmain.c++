#define XtNumber(arr) ((sizeof(arr) / sizeof(arr[0])))

#include "gplaut04.h"
#include "gVarNames.h"
#include "tube.h"

#ifndef R3B
#define TIME_IS_OFF  0 
#define TIME_ON_X    1
#define TIME_ON_Y    2
#define TIME_ON_Z    3
#endif

float STATIONARY_POINT_RADIUS = 0.01;
int specialColorItems = CL_SP_ITEMS;

extern void  rounding(double &, double &);

float fmData[12];
char autoDir[256];

#ifndef R3B
static int iiii = 0;
#endif
SbColor lineColor[NUM_SP_POINTS];
SbColor lineColorTemp[NUM_SP_POINTS];
SbColor lineColorOld[NUM_SP_POINTS];
#ifndef R3B
SbColor envColors[10];
#else
SbColor envColors[12];
#endif

#ifndef R3B
int bifDefaultAxis[3], solDefaultAxis[3];
#else
extern float libPtMax[3], libPtMin[3];
#endif

struct DefaultAxisItems dai;

unsigned long linePattern[NUM_SP_POINTS];
unsigned long linePatternTemp[NUM_SP_POINTS];
unsigned long linePatternOld[NUM_SP_POINTS];

unsigned long stabilityLinePattern[2];
unsigned long stabilityLinePatternTemp[2];

bool blOpenSolFile = TRUE;
bool blOpenBifFile = TRUE;
#ifdef R3B
bool blMassDependantOption = false;
#endif

#ifndef R3B
bool blDrawTicker = false;
#else
bool blDrawTicker = true;
#endif

int whichType= 0 ;
int whichTypeTemp= 0 ;
int whichTypeOld = 0 ;

int whichStyle= 0 ;
int whichStyleTemp= 0 ;
int whichStyleOld = 0 ;

#ifndef R3B
int whichCoord = 0 ;
#else
int whichCoordSystem = 0 ;
int whichCoordSystemTemp = 0 ;
int whichCoordSystemOld = 0 ;

int whichCoord = 3 ;
#endif
int whichCoordTemp = 0 ;
int whichCoordOld = 0 ;
#ifndef R3B
int time_on = 0;
#endif

unsigned long graphWidgetToggleSet     = (unsigned long) 0 ;
unsigned long graphWidgetToggleSetTemp = (unsigned long) 0 ;
unsigned long graphWidgetToggleSetOld  = (unsigned long) 0 ;

SolNode mySolNode;
BifNode myBifNode;
UserData clientData;

bool options[]=
{
    TRUE,   TRUE, FALSE, TRUE,  FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE
};
bool optionsTemp[11];
bool optionsOld[11];
bool optBif[11], optSol[11];
bool setShow3D, setShow3DSol, setShow3DBif;

char sFileName[256];
char bFileName[256];
char dFileName[256];

solutionp solHead = NULL;
long int animationLabel = 0;
int maxComponent = 1;
int curComponent = 1;

int winWidth, winHeight;

int xCoordIndices[MAX_LIST], xCoordIdxSize;
int yCoordIndices[MAX_LIST], yCoordIdxSize;
int zCoordIndices[MAX_LIST], zCoordIdxSize;
int lblIndices[MAX_LABEL], lblChoice[MAX_LABEL], lblIdxSize;

#ifndef R3B
int MAX_SAT_SPEED = 1000;
#else
int MAX_SAT_SPEED = 100;
#endif
int MIN_SAT_SPEED = 0;
#ifndef R3B
int MAX_ORBIT_SPEED = 1000;
#else
int MAX_ORBIT_SPEED = 100;
#endif
int MIN_ORBIT_SPEED = 0;

float orbitSpeed = 1.0;
float satSpeed   = 0.5;
float satRadius  = 1.0;
float lineWidthScaler = 1.0;
float aniLineScaler = 2.0;
static float sphereRadius = 1.0;

int   coloringMethod = -1;
int   coloringMethodType[2] = {-1, -1};
float bgTransparency = 0;
float numPeriodAnimated = 1.0;

long int numLabels;

#ifdef R3B
float libPtScaler = 1.0;
float smallPrimRadius=1.0;
float largePrimRadius=1.0;
int   numOfStars = 100;
float diskTransparency = 0;
bool diskFromFile = false;
float distance = 1;
float sPrimPeriod  = 31558118.4;
float gravity = 9.18;
double mass = 0.0;
#endif

double legendScaleValues[2];

SoSeparator *userScene;
#ifndef R3B
SoSeparator *root;
#else
SoSelection *root;
#endif
SoSeparator *starryBackground;

char xAxis[MAX_LIST][5];
char zAxis[MAX_LIST][5];
char yAxis[MAX_LIST][5];
char labels[MAX_LABEL][8];

char coloringMethodList[MAX_LIST+CL_SP_ITEMS][8];
int myLabels[MAX_LABEL+SP_LBL_ITEMS];

///////////////////////////////////////////////////////////////
//
//  Function prototypes
//

SoSeparator * drawAStrip(float stripSet[][3], int size);
SoSeparator * drawATube(TubeNode cnode);
SoSeparator * drawASphere(float ptb[], float size);
SoSeparator * MyFileRead(const char *filename, SbString &errorMessage);
void setLegendValues(double* values);

void lookForThePoint(float position[],long int &bIdx, long int &sIdx);

SoSeparator * createStarryBackground(int total,float diameter);
SoGroup * setLineAttributesByStability(int stability, float scaler);
SoGroup * setLineColorBlendingByStability(float * vertices, long int size, int stab, float scaler);
SoGroup * setLineAttributesByParameterValue(double parValue, double parMax, double parMid, double parMin, int stability, float scaler);
SoGroup * setLineAttributesByBranch(int iBranch, int stability, float scaler);
SoGroup * setLineAttributesByType(int stability, int type, float scaler);
SoGroup * setLineColorBlending(float * vertices, long int size, int stability, int type, float scaler);

SoSeparator * createCoordinates(bool, int type, float mx[3], float mn[3], int tk[3], int where);
SoSeparator * createLegend(SbVec3f pos, double val[5]);
SoSeparator * createDiscreteLegend(SbVec3f pos, SbColor lineColors[13]);
SoSeparator * createBranchLegend(SbVec3f pos, SbColor lineColors[13]);
SoSeparator * createStabilityLegend(SbVec3f pos, SbColor lineColors[2]);
SoSeparator * addLegend();

SoSeparator * createSolutionSceneWithWidgets();
#ifdef R3B
SoSeparator * createSolutionInertialFrameScene(float dis);
#endif
SoSeparator * createBifurcationScene();
#ifndef R3B
SoSeparator * renderSolution();
SoSeparator * renderBifurcation();
#else
SoSeparator * renderSolution(double mu);
SoSeparator * renderBifurcation(double mu);
#endif
SoSeparator * animateSolutionUsingTubes(bool aniColoring);
SoSeparator * animateSolutionUsingLines(bool aniColoring);
SoSeparator * animateSolutionUsingPoints(int style, bool aniColoring);
#ifdef R3B
SoSeparator * animateOrbitCalSteps(long int n,long int si);
SoSeparator * animateIner2(long int j, long int si);
#endif
SoSeparator * animateOrbitMovement(long int n, long int si);
SoSeparator * animateOrbitWithTail(int iBranch, long int j,long  int si);
#ifdef R3B
SoSeparator * animateOrbitWithNurbsCurveTail(long int j,long int si);
#endif
SoSeparator * drawAnOrbitUsingLines(int iBranch, long int l, long int si, float scaler, int stability, int type, bool coloring);
SoSeparator * drawAnOrbitUsingPoints(int style, int iBranch,  long int l, long int si, float scaler, int stability, int type, bool aniColoring);
SoSeparator * drawAnOrbitUsingNurbsCurve(int iBranch, long int l, long int si, float scaler, int stability, int type);
SoSeparator * drawAnOrbitUsingTubes(int iBranch, long int l, long int si, float scaler, int stability, int type);
SoSeparator * drawABifBranchUsingLines(int iBranch, long int l, long int si, float scaler, int stability, int type);
SoSeparator * drawABifBranchUsingNurbsCurve(int iBranch,long int l, long int si, float scaler, int stability, int type);
SoSeparator * drawABifLabelInterval(long int l, long int si, float scaler, int stability, int type);
SoSeparator * drawABifLabelIntervalUsingNurbsCurve(long int l, long int si, float scaler, int stability, int type);
#ifdef R3B
SoSeparator * createPrimary(double mass, double pos, float scale, char *txtureFileName);
SoSeparator * createLibrationPoint(float mu, float dis, float scale, char *txtureFileName);
SoSeparator * drawEarthMovement(int k);
#endif
SoSeparator * drawASphereWithColor(float color[], float position[], float size);
SoSeparator * drawASolBranchUsingSurface(long obStart, long obEnd, long numVert);
#ifdef R3B
void computePrimaryPositionInInertialSystem(int coordSys, float mass, float R, float T, float t,
float bigPosition[], float smallPosition[], float velocity[]);
#endif

SoSeparator * createAxis(float red, float green, float blue);
SoMaterial  * setLabelMaterial(int lblType);
SoSeparator * drawStarryBackground(char * bgFileName);

#ifdef R3B
SoSeparator * inertialSystemAnimation(int coordSys, SolNode mySolNode, \
float numAnimatedPeriod, int kth, int idx, float mass, \
float distance, float sPrimPeriod, float  gravity);

#endif
int readResourceParameters();
void copySolDataToWorkArray(int varIndices[]);
#ifndef R3B
void searchForMaxMin(int component, int  varIndices[]);
#endif
void copyBifDataToWorkArray(int varIndices[]);

static void processPrinting(char* filename );

extern char * strrighttrim(char*);
extern char * strlefttrim(char*);

//
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
//
//  functions
//
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
//
void 
cropScene(const char* filename)
//
////////////////////////////////////////////////////////////////////////
{
    char command[100];
    system("import my_temp.gif");
    system("convert -crop 1024x768 my_temp.gif my_temp_cropped.gif");
    system("convert my_temp_cropped.gif my_temp.eps");
    system("rm my_temp.gif my_temp_cropped.gif");
}


////////////////////////////////////////////////////////////////////////
//
// Write the graph to iv file.
//
void
writeToFile(const char * fileName)
//
////////////////////////////////////////////////////////////////////////
{
    SoWriteAction myWrite;
    myWrite.getOutput()->openFile(fileName);
    myWrite.getOutput()->setBinary(FALSE);
    myWrite.apply(root);
    myWrite.getOutput()->closeFile();
}


////////////////////////////////////////////////////////////////////////
//
static void
processPrinting(char* filename )
//
////////////////////////////////////////////////////////////////////////
{
    FILE *myFile = fopen(filename, "w");

    if (myFile == NULL)
    {
        fprintf(stderr, "Cannot open output file\n");
        exit(1);
    }

    printf("Printing scene... ");
    fflush(stdout);

    fclose(myFile);

    printf("  ...done printing.\n");
    fflush(stdout);
}


////////////////////////////////////////////////////////////////////////
//
// Description:
//        Brings up the "HELP INFORMATIO"
//
//
void
showHelpDialog()
//
////////////////////////////////////////////////////////////////////////
{

    char command[256];
    strcpy(command, autoDir);
    strcat(command,"/plaut04/doc/userguide.pdf");
    if (access(command, R_OK) != 0)
    {
        system("xmessage 'Sorry, could not find "
            "userguide.pdf' > /dev/null");
        return;
    }

    sprintf(command, "which xpdf> /dev/null");

    int err = system(command);
    if (err)
    {
        system("xmessage 'You must install xpdf"
            " for this function to work' > /dev/null");
        return;
    }

    strcpy(command, "xpdf  ");
    strcat(command, autoDir);
    strcat(command, "/plaut04/doc/userguide.pdf & ");
    system(command);
}


////////////////////////////////////////////////////////////////////////
//
void 
updateScene()
//
////////////////////////////////////////////////////////////////////////
{
    int varIndices[3];

    SoSeparator * newScene = new SoSeparator;

    int mx = max(max(xCoordIdxSize, yCoordIdxSize), max(yCoordIdxSize, zCoordIdxSize));
    maxComponent = mx;

#ifndef R3B
    // look for the maximum/minum value in the x-axis, y-axis, z-axis
    if(whichType != BIFURCATION)
    {
        for(int i=0; i<mx; i++)
        {
            int component = i+1;
            varIndices[0]=xCoordIndices[(i>=xCoordIdxSize)?(i%xCoordIdxSize):(i)];
            varIndices[1]=yCoordIndices[(i>=yCoordIdxSize)?(i%yCoordIdxSize):(i)];
            varIndices[2]=zCoordIndices[(i>=zCoordIdxSize)?(i%zCoordIdxSize):(i)];
            searchForMaxMin(component, varIndices);
        }
    }
 
    time_on = TIME_IS_OFF;

#endif
    for(int i=0; i<mx; i++)
    {
        curComponent = i+1;
        varIndices[0]=xCoordIndices[(i>=xCoordIdxSize)?(i%xCoordIdxSize):(i)];
        varIndices[1]=yCoordIndices[(i>=yCoordIdxSize)?(i%yCoordIdxSize):(i)];
        varIndices[2]=zCoordIndices[(i>=zCoordIdxSize)?(i%zCoordIdxSize):(i)];

#ifndef R3B
        if (varIndices[0] == 0) time_on = TIME_ON_X; 
        if (varIndices[1] == 0) time_on = TIME_ON_Y; 
        if (varIndices[2] == 0) time_on = TIME_ON_Z; 

#else
        animationLabel = myLabels[lblIndices[0]];
#endif
        if(whichType != BIFURCATION)
        {
#ifndef R3B
            animationLabel = myLabels[lblIndices[0]];
#endif
            copySolDataToWorkArray(varIndices);
            newScene->addChild( createSolutionSceneWithWidgets() );
        }
        else 
        {
            copyBifDataToWorkArray(varIndices);
            newScene->addChild(createBifurcationScene());
        }
    }

    if(userScene == NULL)
    {
        userScene = newScene;
        root->addChild(userScene);
    }
    else
    {
        root->removeAllChildren();
        root->addChild(newScene);
        userScene = newScene;
    }
#ifndef R3B
    iiii++;
#endif
}

////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
//
// Routine to create a scene graph representing an auto solution
//
SoSeparator *
createSolutionSceneWithWidgets()
//
////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
{
    float dis = max(max((mySolNode.max[0]-mySolNode.min[0]),
        (mySolNode.max[1]-mySolNode.min[1])),
        (mySolNode.max[2]-mySolNode.min[2]));

    SoSeparator *result = new SoSeparator;

    if(options[OPT_NORMALIZE_DATA])
    {
        normalizeSolData();
    }

#ifdef R3B
    if(whichCoordSystem != ROTATING_F)
    {
        if(options[OPT_REF_PLAN])
        {
            float position[3], radius =1;
            position[0]=position[1]=position[2]=0;
            if(whichCoordSystem == INERTIAL_B ) radius = 1-mass;
            SoSeparator *diskSep = createDisk(position,radius);
            result->addChild(diskSep);
        }
        result->addChild(createSolutionInertialFrameScene(dis));
    }
    else
    {
#endif
    char txtureFileName[256];

    strcpy(txtureFileName, autoDir);
    strcat(txtureFileName,"/widgets/large.rgb");
#ifdef R3B
        if(options[OPT_LIB_POINTS])
        {
            dis = max(max( dis                     , (libPtMax[0]-libPtMin[0])),
                max((libPtMax[1]-libPtMin[1]), (libPtMax[2]-libPtMin[2])));
            SoSeparator * libPtsSep = createLibrationPoint(mass, dis, libPtScaler, txtureFileName);
            result->addChild(libPtsSep);
        }
#endif

    if(whichCoord != NO_COORD)
    {
        int cdtype = 0;
        if(whichCoord==LEFTBACK)
            cdtype = 2;
        else if(whichCoord==LEFTAHEAD)
            cdtype = 1;
        else if (whichCoord==COORDORIGIN)
            cdtype = 0;

        SoSeparator * coordSep = new SoSeparator;

        SoTransform * coordXform = new SoTransform;
        coordXform->rotation.setValue(SbVec3f(1.0, 0.0, 0.0), M_PI_2);

        coordSep->addChild(coordXform);
        static int tickers[3];
        if(blDrawTicker)
        {
            tickers[0]=tickers[1]=tickers[2]=5;
        }
        else
        {
            tickers[0]=tickers[1]=tickers[2]=-1;
        }

        float asMax[3], asMin[3];
        if(options[OPT_NORMALIZE_DATA])
        {
            asMin[0]=mySolNode.min[0]; asMin[1]=mySolNode.min[1]; asMin[2]=mySolNode.min[2];
            asMax[0]=mySolNode.max[0]; asMax[1]=mySolNode.max[1]; asMax[2]=mySolNode.max[2];
#ifndef R3B
            if (time_on == TIME_ON_X) asMax[0] = 1;
            else if (time_on == TIME_ON_Y) asMax[1] = 1;
            else if (time_on == TIME_ON_Z) asMax[2] = 1;
#endif
        }
        else
        {
            asMax[0] = asMax[1] = asMax[2] = 1;
            asMin[0] = asMin[1] = asMin[2] = -1;
        }

        coordSep->addChild(createCoordinates(setShow3D, cdtype, asMax, asMin, tickers, whichCoord));
        result->addChild(coordSep);
    }

#ifndef R3B
    result->addChild(renderSolution());
#else
// create reference DISK
        if(options[OPT_REF_PLAN])
        {
            float position[3];
            position[0]=-mass;
            position[1]=position[2]=0;
            SoSeparator *diskSep = createDisk(position,1.0);
            result->addChild(diskSep);
        }
#endif

// create the primaries
#ifdef R3B
        if(options[OPT_PRIMARY])
        {
            double pos1 = 1-mass;
            double pos2 = -mass;
            strcpy(txtureFileName, autoDir);
            strcat(txtureFileName,"/plaut04/widgets/large.rgb");
            result->addChild(createPrimary(1-mass+1e-9, pos2, 0.25*largePrimRadius, txtureFileName));
            strcpy(txtureFileName, autoDir);
            strcat(txtureFileName,"/plaut04/widgets/small.rgb");
            result->addChild(createPrimary(mass-1e-9, pos1, 0.25*smallPrimRadius, txtureFileName));
        }

//  create solution scene
        result->addChild(renderSolution(mass));

    }

    static int iiii = 0;
//  create starry background
#endif
    if(iiii && options[OPT_BACKGROUND])
    {
        char bgFileName[256];
        strcpy(bgFileName, autoDir);
        strcat(bgFileName, "/plaut04/widgets/background.rgb");
        result->addChild(drawStarryBackground(bgFileName));
    }

#ifndef R3B
    static SoSeparator *leg = new SoSeparator;
#else
//  add legend
#endif
    if(iiii && options[OPT_LEGEND])
    {
#ifndef R3B
        leg = addLegend();
        result->addChild(leg);
#else
        result->addChild(addLegend());
#endif
    }
#ifdef R3B
    iiii++;
#endif

    return result;
}


////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawStarryBackground(char * bgFileName)
//
////////////////////////////////////////////////////////////////////////
{
    SoSeparator *starryBg = new SoSeparator;
    SoTexture2 * bgTxture = new SoTexture2;
    bgTxture->filename.setValue(bgFileName);
    SoMaterial *bgMaterial = new SoMaterial;
    bgMaterial->transparency = bgTransparency;
    starryBg->addChild(bgMaterial);
    starryBg->addChild(bgTxture);
    SoTransform *bgXform = new SoTransform;
    bgXform->translation.setValue(0,0,-9);

    SoOrthographicCamera * bgCamera = new SoOrthographicCamera;

    SoCube * bgPlane = new SoCube();
    bgPlane->width  = 3.0;
    bgPlane->height = 3.0;
    bgPlane->depth  = 0.01;

    SoDirectionalLight * bgLight = new SoDirectionalLight;
    bgLight->intensity = 0.05;
    bgLight->direction.setValue(0,0, -10);
    starryBg->addChild(bgLight);
    starryBg->addChild(bgCamera);
    starryBg->addChild(bgXform);
    starryBg->addChild(bgPlane);
    return starryBg;
}


////////////////////////////////////////////////////////////////////////
//
SoSeparator *
addLegend()
//
////////////////////////////////////////////////////////////////////////
{
    SoSeparator * result = new SoSeparator;
    SoOrthographicCamera *legendCamera = new SoOrthographicCamera;
    result->addChild(legendCamera);
    SbVec3f lgPos;
    lgPos.setValue(0.85, 0.65, -5.9);

    if(coloringMethod == CL_BRANCH_NUMBER)
    {
        result->addChild(createBranchLegend(lgPos, lineColor));
    }
    else if(coloringMethod == CL_STABILITY)
    {
        SbColor color[2];
        color[0].setValue(1,0,0);
#ifndef R3B
        color[1].setValue(0,0,1);
#else
        color[1].setValue(0,1,0);
#endif
        result->addChild(createStabilityLegend(lgPos, color));
    }
    else if( coloringMethod == CL_ORBIT_TYPE)
    {
        result->addChild(createDiscreteLegend(lgPos, lineColor));
    }
    else
    {
        double values[5];
        setLegendValues(values);
        result->addChild(createLegend(lgPos, values));
    }
    result->addChild(new SoDirectionalLight);

    return result;
}


////////////////////////////////////////////////////////////////////////
//
void 
setLegendValues(double* values)
//
////////////////////////////////////////////////////////////////////////
{
    for(int i=0; i<5; ++i)
        values[i] = legendScaleValues[0] + i*(legendScaleValues[1]-legendScaleValues[0])/4.0;
}


#ifdef R3B
///////////////////////////////////////////////////////////////////////////
//
//   Using a red ball to simulate the movement of a sattelite and using
//   white lines to simulate the trace of the sattelite.
//
SoSeparator *
animateOrbitInertialSysUsingLine(int iBranch,  int iOrbit,
float (*vertices)[3], float (*largePrimPos)[3], float (*smallPrimPos)[3],
float * myColorBase, float period, int size,
float scaler, int stability, int type)
//
///////////////////////////////////////////////////////////////////////////
{
    SoSeparator *result = new SoSeparator;
    SoSeparator *satGroup = new SoSeparator;

    SoDrawStyle *satStyle = new SoDrawStyle;
    satStyle->style = SoDrawStyle::FILLED;
    satGroup->addChild(satStyle);

    SoSeparator * satSep = new SoSeparator;

    float maxV[3], minV[3];
    maxV[0]=minV[0]=vertices[0][0];
    maxV[1]=minV[1]=vertices[0][1];
    maxV[2]=minV[2]=vertices[0][2];
    float (*vertices1)[3] = new float[size][3];

    for(int j=0; j<3; ++j)
    {
        for(int i=1; i<size; i++)
        {
            if(maxV[j] < vertices[i][j])     maxV[j] = vertices[i][j] ;
            if(maxV[j] < largePrimPos[i][j]) maxV[j] = largePrimPos[i][j] ;
            if(maxV[j] < smallPrimPos[i][j]) maxV[j] = smallPrimPos[i][j] ;

            if(minV[j] > vertices[i][j])     minV[j] = vertices[i][j] ;
            if(minV[j] > largePrimPos[i][j]) minV[j] = largePrimPos[i][j] ;
            if(minV[j] > smallPrimPos[i][j]) minV[j] = smallPrimPos[i][j] ;
            vertices1[i-1][j]= vertices[i][j];
        }
    }

    float dis = fabs(max(max((maxV[0]-minV[0]), (maxV[1]-minV[1])), (maxV[2]-minV[2])));

// animate the orbit
    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, size, vertices);
    satGroup->addChild(myCoords);

    SoTimeCounter *myCounter = new SoTimeCounter;
    myCounter->max = size-1;
    myCounter->min = 0;
    myCounter->frequency = (numPeriodAnimated !=0) ? 0.1*satSpeed/numPeriodAnimated : 0.1*satSpeed;

// define the solution line set
    if(numPeriodAnimated!=0)
    {
        SoLineSet *myLine= new SoLineSet;
        myLine->numVertices.connectFrom(&myCounter->output);

        if(coloringMethod == CL_BRANCH_NUMBER)
            satGroup->addChild(setLineAttributesByBranch(iBranch,stability,scaler));
        else if(coloringMethod == CL_STABILITY)
            satGroup->addChild(setLineAttributesByStability(stability, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            satGroup->addChild(setLineAttributesByType(stability, type, scaler));
        else if(coloringMethod == CL_LABELS)
        {
            double bMin = 0;
            for(int ib = 0; ib< iBranch; ++ib)
                bMin +=  mySolNode.numOrbitsInEachBranch[ib];
            double bMax = bMin+mySolNode.numOrbitsInEachBranch[iBranch]-1;
            satGroup->addChild(setLineAttributesByParameterValue(
                iOrbit-1, bMax, (bMax+bMin)/2.0, bMin,
                stability, scaler));
        }
        else if(coloringMethod >= mySolNode.nar)
            satGroup->addChild(setLineAttributesByParameterValue(
                    mySolNode.par[iOrbit-1][mySolNode.parID[coloringMethod-mySolNode.nar]],
                    mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                    mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                    mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                    stability, scaler));
        else
            satGroup->addChild(setLineColorBlending(myColorBase, size,
                stability, type, scaler));
        satGroup->addChild(myLine);
    }

    SoMaterial * satMtl = new SoMaterial;
    SoSphere * mySat = new SoSphere;
    mySat->radius = dis*0.005*satRadius;

    SoTranslation * satTrans = new SoTranslation;
    satMtl->diffuseColor.setValue(envColors[4]);
    satGroup->addChild(satMtl);
    satGroup->addChild(satTrans);
    satGroup->addChild(mySat);

    SoSelectOne *mysel = new SoSelectOne(SoMFVec3f::getClassTypeId());
    mysel->index.connectFrom(&myCounter->output);
    mysel->input->enableConnection(TRUE);
    mysel->input->connectFrom(&myCoords->point);
    satTrans->translation.connectFrom(mysel->output);

    result->addChild(satGroup);

// animate the primary movement.
    SoSeparator * smallPrimLineSep = new SoSeparator;
    SoCoordinate3 *smallPrimCoords = new SoCoordinate3;
    smallPrimCoords->point.setValues(0, size, smallPrimPos);
    smallPrimLineSep->addChild(smallPrimCoords);

    SoLineSet *smallPrimLine= new SoLineSet;
    smallPrimLine->numVertices.connectFrom(&myCounter->output);
    SoMaterial * smallPrimLineMtl = new SoMaterial;
    smallPrimLineMtl->diffuseColor.setValue(envColors[8]);
    smallPrimLineSep->addChild(smallPrimLineMtl);
    smallPrimLineSep->addChild(smallPrimLine);

    SoTranslation * smallPrimTrans = new SoTranslation;

    SoSelectOne *smallPrimSel = new SoSelectOne(SoMFVec3f::getClassTypeId());
    smallPrimSel->index.connectFrom(&myCounter->output);
    smallPrimSel->input->enableConnection(TRUE);
    smallPrimSel->input->connectFrom(&smallPrimCoords->point);
    smallPrimTrans->translation.connectFrom(smallPrimSel->output);

    result->addChild(smallPrimLineSep);

    SoSeparator * largePrimLineSep = new SoSeparator;

    SoCoordinate3 *largePrimCoords = new SoCoordinate3;
    largePrimCoords->point.setValues(0, size, largePrimPos);
    largePrimLineSep->addChild(largePrimCoords);

    SoLineSet *largePrimLine= new SoLineSet;
    largePrimLine->numVertices.connectFrom(&myCounter->output);
    SoMaterial * largePrimLineMtl = new SoMaterial;
    largePrimLineMtl->diffuseColor.setValue(envColors[6]);
    largePrimLineSep->addChild(largePrimLineMtl);
    largePrimLineSep->addChild(largePrimLine);

    SoTranslation * largePrimTrans = new SoTranslation;

    SoSelectOne *largePrimSel = new SoSelectOne(SoMFVec3f::getClassTypeId());
    largePrimSel->index.connectFrom(&myCounter->output);
    largePrimSel->input->enableConnection(TRUE);
    largePrimSel->input->connectFrom(&largePrimCoords->point);
    largePrimTrans->translation.connectFrom(largePrimSel->output);

    result->addChild(largePrimLineSep);

    delete []vertices1;
    return result;
}


SoSeparator *
drawAnSolOrbitInertialSysUsingLines(int iBranch,  int iOrbit, float (*myVertices)[3], float *myColorBase,
long int arrSize, float scaler, int stability, int type)
{
    int32_t  myint[10];

    SoSeparator * anOrbit = new SoSeparator;

    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, arrSize, myVertices);
    myint[0]=-1;

// define the solution line set
    SoLineSet *myLine= new SoLineSet;
    myLine->numVertices.setValues(0,1,myint);

    if(coloringMethod == CL_BRANCH_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
    else if(coloringMethod == CL_LABELS)
    {
        double bMin = 0;
        for(int ib = 0; ib< iBranch; ++ib)
            bMin +=  mySolNode.numOrbitsInEachBranch[ib];
        double bMax = bMin+mySolNode.numOrbitsInEachBranch[iBranch]-1;
        anOrbit->addChild(setLineAttributesByParameterValue(
            iOrbit-1, bMax, (bMax+bMin)/2.0, bMin,
            stability, scaler));
    }
    else if(coloringMethod >= mySolNode.nar)
        anOrbit->addChild(setLineAttributesByParameterValue(
                mySolNode.par[iOrbit-1][mySolNode.parID[coloringMethod-mySolNode.nar]],
                mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                stability, scaler));
    else
        anOrbit->addChild(setLineColorBlending(myColorBase, arrSize,
            stability, type, scaler));
    anOrbit->addChild(myCoords);
    anOrbit->addChild(myLine);

    return anOrbit;
}


////////////////////////////////////////////////////////////////////////
//
void
convertDataToInertialSystem(float (*myVertices)[3], float *timeEqualDiv, float *myColorBase,
long int arrSize, long int orbitSize, long int kth, long int sumX)
//
////////////////////////////////////////////////////////////////////////
{
    float (*workArray)[3]  = new float [arrSize][3];
    float *time         = new float [arrSize];
    float satPeriod = clientData.solPeriod[kth];
    float rpp[3], vpp[3];
    float massCurLabeledOrbit = mySolNode.mass[kth];

    for(int i=0; i<arrSize; ++i)
    {
        workArray[i][0]=mySolNode.xyzCoords[sumX+i%orbitSize][0];
        workArray[i][1]=mySolNode.xyzCoords[sumX+i%orbitSize][1];
        workArray[i][2]=mySolNode.xyzCoords[sumX+i%orbitSize][2];
        if(whichStyle==TUBE && !options[OPT_SAT_ANI] )
        {
            if(coloringMethod>=0 && coloringMethod < mySolNode.nar)
                for(int k=0; k<11; ++k)
                    myColorBase[i*11+k]  = clientData.solData[sumX+i%orbitSize][coloringMethod];
            else if(coloringMethod==CL_POINT_NUMBER )
                for(int k=0; k<11; ++k)
                    myColorBase[i*11+k]  = i;
        }
        else
        {
            if(coloringMethod>=0 && coloringMethod < mySolNode.nar)
                myColorBase[i]=clientData.solData[sumX+i%orbitSize][coloringMethod];
            else if(coloringMethod==CL_POINT_NUMBER )
                myColorBase[i]=i;
        }

        time[i] = mySolNode.time[sumX+i%orbitSize]+i/orbitSize;

        satelliteMovingOrbit(whichCoordSystem,
            workArray[i], time[i],  massCurLabeledOrbit, distance, satPeriod, sPrimPeriod, gravity, rpp, vpp );
        for(int jk=0; jk<3; ++jk) workArray[i][jk]=rpp[jk];
    }

    float Tr3b = 1;

    Tr3b = (numPeriodAnimated==0) ? Tr3b/arrSize : numPeriodAnimated * Tr3b/arrSize;
    for(int i=0; i <arrSize; ++i)
        timeEqualDiv[i] = i * Tr3b;

    myVertices[0][0]= workArray[0][0];
    myVertices[0][1]= workArray[0][1];
    myVertices[0][2]= workArray[0][2];
    for(int i=1; i<arrSize; i++)
    {
        float tTemp = timeEqualDiv[i];
        int m = 0;
        while(tTemp > time[m] && m < arrSize) ++m;
        if( fabs(tTemp-time[m]) <= 1.0e-9 || fabs(time[m]-time[m-1]<=1.0e-8))
        {
            myVertices[i][0]=workArray[m][0];
            myVertices[i][1]=workArray[m][1];
            myVertices[i][2]=workArray[m][2];
        }
        else
        {
            float dt =  (tTemp-time[m-1])/(time[m]-time[m-1]);
            myVertices[i][0]= workArray[m-1][0]+(workArray[m][0]-workArray[m-1][0])*dt;
            myVertices[i][1]= workArray[m-1][1]+(workArray[m][1]-workArray[m-1][1])*dt;
            myVertices[i][2]= workArray[m-1][2]+(workArray[m][2]-workArray[m-1][2])*dt;
        }
    }
    delete [] time;
    delete [] workArray;
}



//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawAnSolOrbitInertialSysUsingTubes(int iBranch,  int iOrbit,
float (*myVertices)[3], float *myColorBase, const long int arrSize,
const float tubeRadiusScaler, const int stability, const int type)
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator * anOrbit = new SoSeparator;

    Tube tube;

    if(coloringMethod == CL_BRANCH_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, tubeRadiusScaler));
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, tubeRadiusScaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        anOrbit->addChild(setLineAttributesByType(stability, type, tubeRadiusScaler));
    else if(coloringMethod == CL_LABELS)
    {
        double bMin = 0;
        for(int ib = 0; ib< iBranch; ++ib)
            bMin +=  mySolNode.numOrbitsInEachBranch[ib];
        double bMax = bMin+mySolNode.numOrbitsInEachBranch[iBranch]-1;
        anOrbit->addChild(setLineAttributesByParameterValue(
            iOrbit-1, bMax, (bMax+bMin)/2.0, bMin,
            stability,  tubeRadiusScaler));
    }
    else if(coloringMethod >= mySolNode.nar)
        anOrbit->addChild(setLineAttributesByParameterValue(
                mySolNode.par[iOrbit-1][mySolNode.parID[coloringMethod-mySolNode.nar]],
                mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                stability, tubeRadiusScaler));
    else
        anOrbit->addChild(setLineColorBlending(myColorBase, arrSize*11,
            stability, type, tubeRadiusScaler));

    tube = Tube(arrSize, myVertices, tubeRadiusScaler*0.005, 10);
    anOrbit->addChild(tube.createTube());

    return anOrbit;
}


//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawAnSolOrbitInertialSysUsingNurbsCurve(int iBranch, int iOrbit,
float (*myVertices)[3], const long int arrSize,
const float scaler, const int stability, const int type)
//
//////////////////////////////////////////////////////////////////////////
{
    int32_t  myint[10];
    SoSeparator * anOrbit = new SoSeparator;
    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, arrSize, myVertices);
    myint[0] = -1;

// define a nurbs curve
    int number = arrSize;
    float * knots = new float[number+4];
    for (int i=0; i<4; ++i) knots[i]=0, knots[i+number]=number-3;
    for(int i=4; i<number; ++i) knots[i]=i-3;
    SoNurbsCurve *myCurve = new SoNurbsCurve;
    myCurve->numControlPoints = arrSize;
    myCurve->knotVector.setValues(0, number+4, knots);

    if(coloringMethod == CL_BRANCH_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
    else if(coloringMethod == CL_LABELS)
    {
        double bMin = 0;
        for(int ib = 0; ib< iBranch; ++ib)
            bMin +=  mySolNode.numOrbitsInEachBranch[ib];
        double bMax = bMin+mySolNode.numOrbitsInEachBranch[iBranch]-1;
        anOrbit->addChild(setLineAttributesByParameterValue(
            iOrbit-1, bMax, (bMax+bMin)/2.0, bMin,
            stability, scaler));
    }
    else if(coloringMethod >= mySolNode.nar)
        anOrbit->addChild(setLineAttributesByParameterValue(
                mySolNode.par[iOrbit-1][mySolNode.parID[coloringMethod-mySolNode.nar]],
                mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                stability, scaler));
    else
        anOrbit->addChild(setLineAttributesByType(stability, type, scaler));

    anOrbit->addChild(myCoords);
    anOrbit->addChild(myCurve);
    delete [] knots;
    return anOrbit;
}


//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
createSolutionInertialFrameScene(float dis)
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator * solGroup = new SoSeparator;
    SoCoordinate3 *solCoords = new SoCoordinate3;
    int stability, type;
    float satPeriod = 1;
    long int  arrSize;

    if(animationLabel == 0)
    {
        long int si = 0;
        long int orbitSize;
        float (*myVertices)[3];
        float *myColorBase;
        float *time;
        int iBranch = 0;
        int curBranchID = mySolNode.branchID[iBranch];
        int sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];
        for(int k=0; k<mySolNode.numOrbits; ++k)
        {
            if(k >= sumOrbit)
            {
                curBranchID = mySolNode.branchID[++iBranch];
                sumOrbit   += mySolNode.numOrbitsInEachBranch[iBranch];
            }
            orbitSize = mySolNode.numVerticesEachPeriod[k];
            arrSize = (numPeriodAnimated==0) ? orbitSize : (int)(numPeriodAnimated * orbitSize);

            myVertices = new float [arrSize][3];
            myColorBase= new float [arrSize*11];
            time  = new float [arrSize];

            convertDataToInertialSystem(myVertices, time, myColorBase, arrSize, orbitSize, k+1, si);
            stability = clientData.labelIndex[k][3];
            type = clientData.labelIndex[k][2];

            if(whichStyle==TUBE)
            {
                solGroup->addChild(drawAnSolOrbitInertialSysUsingTubes(
                    iBranch,  k, myVertices,myColorBase, arrSize, lineWidthScaler,
                    stability, type));
            }
            else if(whichStyle==SURFACE)
            {
                solGroup->addChild(drawAnSolOrbitInertialSysUsingLines(
                    iBranch,  k, myVertices, myColorBase, arrSize, lineWidthScaler,
                    stability, type));
            }
            else if(whichStyle==NURBS)
            {
                solGroup->addChild(drawAnSolOrbitInertialSysUsingNurbsCurve(
                    iBranch, k, myVertices, arrSize, lineWidthScaler,
                    stability, type));
            }
            else
            {
                solGroup->addChild(drawAnSolOrbitInertialSysUsingLines(
                    iBranch,  k, myVertices, myColorBase, arrSize, lineWidthScaler,
                    stability, type));
            }

            si += mySolNode.numVerticesEachPeriod[k];
            delete [] myVertices;
            delete [] time;
        }
    }
    else if(animationLabel != MY_NONE)
    {
        float vpp[3];
        for(int n=0; n<lblIdxSize; ++n)
        {
            animationLabel=myLabels[lblIndices[n]];
            int si = 0, kno = 0;
            int iBranch = 0;
            int curBranchID = mySolNode.branchID[iBranch];
            int sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];
            while(kno<mySolNode.numOrbits && myLabels[++kno]<animationLabel)
            {
                si += mySolNode.numVerticesEachPeriod[kno-1];
                if(kno >= sumOrbit)
                {
                    curBranchID = mySolNode.branchID[++iBranch];
                    sumOrbit   += mySolNode.numOrbitsInEachBranch[iBranch];
                }
            }

            satPeriod = clientData.solPeriod[kno];
            long int orbitSize = mySolNode.numVerticesEachPeriod[kno-1];
            arrSize = (numPeriodAnimated==0) ? orbitSize : (int)(numPeriodAnimated * orbitSize);

            float (*myVertices)[3] = new float [arrSize][3];
            float *myColorBase = new float [arrSize*11];
            float *time  = new float [arrSize];

            convertDataToInertialSystem(myVertices, time, myColorBase, arrSize, orbitSize, kno, si);

            float (*largePrimPos)[3]   = new float [arrSize][3];
            float (*smallPrimPos)[3]   = new float [arrSize][3];

            for(int i=0; i<arrSize; ++i)
            {
                computePrimaryPositionInInertialSystem(whichCoordSystem,
                    mass, distance, sPrimPeriod, time[i]*satPeriod,
                    largePrimPos[i], smallPrimPos[i], vpp);
            }

            stability = clientData.labelIndex[kno][3];
            type = clientData.labelIndex[kno][2];
            if(options[OPT_SAT_ANI])
            {
                solGroup->addChild(animateOrbitInertialSysUsingLine(
                    iBranch,  kno,  myVertices, largePrimPos, smallPrimPos, myColorBase,
                    satPeriod, arrSize, lineWidthScaler, stability, type));
            }
            else
            {
                if(whichStyle==TUBE)
                {
                    solGroup->addChild(drawAnSolOrbitInertialSysUsingTubes(
                        iBranch,  kno, myVertices, myColorBase, arrSize, lineWidthScaler,
                        stability, type));
                }
                else if(whichStyle==SURFACE)
                {
                    solGroup->addChild(drawAnSolOrbitInertialSysUsingLines(
                        iBranch,  kno, myVertices,myColorBase,  arrSize, lineWidthScaler,
                        stability, type));
                }
                else if(whichStyle==NURBS)
                {
                    solGroup->addChild(drawAnSolOrbitInertialSysUsingNurbsCurve(
                        iBranch,  kno, myVertices, arrSize, lineWidthScaler,
                        stability, type));
                }
                else
                {
                    solGroup->addChild(drawAnSolOrbitInertialSysUsingLines(
                        iBranch,  kno, myVertices, myColorBase, arrSize, lineWidthScaler,
                        stability, type));
                }
            }
            delete [] myVertices;
            delete [] myColorBase;
            delete [] smallPrimPos;
            delete [] largePrimPos;
            delete [] time;
        }
    }
    SoSeparator *aSep = new SoSeparator;
    SoTimeCounter *myCounter = new SoTimeCounter;
    myCounter->ref();
    myCounter->max = 10*(arrSize - 1);
    myCounter->min = 0;
    myCounter->frequency = (numPeriodAnimated !=0) ? 0.1*satSpeed/numPeriodAnimated : 0.1*satSpeed;

    float pseudoPeriod = 0.1*satPeriod*numPeriodAnimated/(arrSize-1);
    SoCalculator *aCalc = new SoCalculator;
    aCalc->a.connectFrom(&myCounter->output);
    aCalc->b.setValue(pseudoPeriod);
    aCalc->expression.setValue("oa = fmod(2.0*M_PI*a*b, 2*M_PI)");

    SoRotationXYZ *aRotation = new SoRotationXYZ;
    aRotation->axis = SoRotationXYZ::Z;
    aRotation->angle.connectFrom(&aCalc->oa);
    aSep->addChild(aRotation);

    static char txtureFileName[256];

// create the primaries
    if(options[OPT_PRIMARY])
    {
        double pos1 = -mass;
        double pos2 = 1-mass;
        if(whichCoordSystem == INERTIAL_B )
            pos1 = -mass , pos2=1-mass;
        else if(whichCoordSystem == INERTIAL_S )
            pos1 = 0, pos2= 1;
        else if(whichCoordSystem == INERTIAL_E )
            pos1 = -1, pos2= 0;
        strcpy(txtureFileName, autoDir);
        strcat(txtureFileName,"/plaut04/widgets/large.rgb");
        aSep->addChild(createPrimary(1-mass+1e-9, pos1, 0.25*largePrimRadius, txtureFileName));
        strcpy(txtureFileName, autoDir);
        strcat(txtureFileName,"/plaut04/widgets/small.rgb");
        aSep->addChild(createPrimary(mass-1e-9, pos2, 0.25*smallPrimRadius, txtureFileName));
    }

// create the libration points
    SoSeparator * libPtsSep = createLibrationPoint(mass, dis, libPtScaler, txtureFileName);
    if(options[OPT_LIB_POINTS])
    {
        aSep->addChild(libPtsSep);
    }

// create solution coordinate axis
    dis = fabs(max(max(dis,(libPtMax[0]-libPtMin[0])),
        max((libPtMax[1]-libPtMin[1]), (libPtMax[2]-libPtMin[2]))));
    if(whichCoord != NO_COORD)
    {
        int cdtype = 0;
        if(whichCoord==LEFTBACK)
            cdtype = 2;
        else if(whichCoord==LEFTAHEAD)
            cdtype = 1;
        else if (whichCoord==COORDORIGIN)
            cdtype = 0;

        SoSeparator * coordSep = new SoSeparator;

        SoTransform * coordXform = new SoTransform;
        coordXform->rotation.setValue(SbVec3f(1.0, 0.0, 0.0),M_PI_2);

        coordSep->addChild(coordXform);
        static int tickers[3];
        if(blDrawTicker)
        {
            tickers[0]=tickers[1]=tickers[2]=5;
        }
        else
        {
            tickers[0]=tickers[1]=tickers[2]=-1;
        }
        float asMax[3], asMin[3];
        if(options[OPT_NORMALIZE_DATA])
        {
            asMax[0]=mySolNode.max[0]; asMax[1]=mySolNode.max[1];asMax[2]=mySolNode.max[2];
            asMin[0]=mySolNode.min[0]; asMin[1]=mySolNode.min[1];asMin[2]=mySolNode.min[2];
        }
        else
        {
            asMax[0] = asMax[1] = asMax[2] = 1;
            asMin[0] = asMin[1] = asMin[2] = -1;
        }

        coordSep->addChild(createCoordinates(setShow3D, cdtype, asMax, asMin, tickers, whichCoord));

        aSep->addChild(coordSep);
    }

    solGroup->addChild(aSep);
    if(options[OPT_PERIOD_ANI])
    {
        int iBranch = 0;
        int curBranchID = mySolNode.branchID[iBranch];
        int sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];
        long int si = 0;
        SoBlinker * solBlinker = new SoBlinker;
        solBlinker->speed = orbitSpeed;
        solBlinker->on = TRUE;
        for(int k=0; k<mySolNode.numOrbits; ++k)
        {
            if(k >= sumOrbit)
            {
                curBranchID = mySolNode.branchID[++iBranch];
                sumOrbit   += mySolNode.numOrbitsInEachBranch[iBranch];
            }
            long int orbitSize = mySolNode.numVerticesEachPeriod[k];
            long int arrSize = (numPeriodAnimated==0) ?
                orbitSize : (int)(numPeriodAnimated * orbitSize);

            float (*myVertices)[3] = new float [arrSize][3];
            float *myColorBase = new float [arrSize*11];
            float *time   = new float [arrSize];

            convertDataToInertialSystem(myVertices, time, myColorBase, arrSize, orbitSize, k+1, si);

            solBlinker->addChild(drawAnSolOrbitInertialSysUsingLines(
                iBranch,  k, myVertices, myColorBase, arrSize, aniLineScaler*lineWidthScaler,
                clientData.labelIndex[k][3],clientData.labelIndex[k][2]));
            si += mySolNode.numVerticesEachPeriod[k];
            delete [] myVertices;
            delete [] myColorBase;
            delete [] time;
        }
        solGroup->addChild(solBlinker);
    }
    return solGroup;

}
#endif

//////////////////////////////////////////////////////////////////////////
//
// Routine to create a scene graph representing an auto bifurcation
//
SoSeparator *
createBifurcationScene()
//
//////////////////////////////////////////////////////////////////////////
{
    float dis = fabs(max(max((myBifNode.max[0]-myBifNode.min[0]),
        (myBifNode.max[1]-myBifNode.min[1])),
        (myBifNode.max[2]-myBifNode.min[2])));

    SoSeparator *result = new SoSeparator;
    result->ref();

    if(options[OPT_NORMALIZE_DATA])
    {
        normalizeBifData();
    }

    if(whichCoord != NO_COORD)
    {
#ifndef R3B
        int type = 0;
#else
        int cdtype = 0;
#endif
        if(whichCoord==LEFTBACK)
#ifndef R3B
            type = 2;
#else
            cdtype = 2;
#endif
        else if(whichCoord==LEFTAHEAD)
#ifndef R3B
            type = 1;
#else
            cdtype = 1;
#endif
        else if (whichCoord==COORDORIGIN)
#ifndef R3B
            type = 0;
#else
            cdtype = 0;
#endif

        SoSeparator * coordSep = new SoSeparator;

        SoTransform * coordXform = new SoTransform;
        coordXform->rotation.setValue(SbVec3f(1.0, 0.0, 0.0),M_PI_2);

        coordSep->addChild(coordXform);
        static int tickers[3];
        if(blDrawTicker)
        {
            tickers[0]=tickers[1]=tickers[2]=5;
        }
        else
        {
            tickers[0]=tickers[1]=tickers[2]=-1;
        }

        float asMax[3], asMin[3];
        if(options[OPT_NORMALIZE_DATA])
        {
#ifndef R3B
            asMax[0]=myBifNode.max[0]; asMax[1]=myBifNode.max[1];asMax[2]=myBifNode.max[2];
            asMin[0]=myBifNode.min[0]; asMin[1]=myBifNode.min[1];asMin[2]=myBifNode.min[2];
#else
            asMax[0]=mySolNode.max[0]; asMax[1]=mySolNode.max[1];asMax[2]=mySolNode.max[2];
            asMin[0]=mySolNode.min[0]; asMin[1]=mySolNode.min[1];asMin[2]=mySolNode.min[2];
#endif
        }
        else
        {
            asMax[0] = asMax[1] = asMax[2] = 1;
            asMin[0] = asMin[1] = asMin[2] = -1;
        }

#ifndef R3B
        coordSep->addChild(createCoordinates(setShow3D, type, asMax, asMin, tickers, whichCoord));
#else
        coordSep->addChild(createCoordinates(setShow3D, cdtype, asMax, asMin, tickers, whichCoord));
#endif

        result->addChild(coordSep);
    }

#ifndef R3B
    SoSeparator * bifBranchSep = renderBifurcation();
#else
    if(options[OPT_REF_PLAN])
    {
        float position[3];
        position[0]=position[1]=position[2]=0;
        dis = 1.0;
        SoSeparator *diskSep = createDisk(position, dis);
        result->addChild(diskSep);
    }

// create the primaries
    char txtureFileName[256];
    strcpy(txtureFileName, autoDir);
    strcat(txtureFileName,"/plaut04/widgets/large.rgb");
    if(options[OPT_PRIMARY])
    {
        double pos1 = 1-mass;
        double pos2 = -mass;
        result->addChild(createPrimary(1-mass,pos2, 0.25*largePrimRadius, txtureFileName));
        strcpy(txtureFileName, autoDir);
        strcat(txtureFileName,"/plaut04/widgets/small.rgb");
        result->addChild(createPrimary(mass, pos1, 0.25*smallPrimRadius, txtureFileName));
    }

// create the libration points
    if(options[OPT_LIB_POINTS])
    {
        strcpy(txtureFileName, autoDir);
        strcat(txtureFileName,"/plaut04/widgets/small.rgb");
        result->addChild(createLibrationPoint(mass, dis, libPtScaler,  txtureFileName));
    }

// create bifurcation graph
    SoSeparator * bifBranchSep = renderBifurcation(mass);
#endif
    result->addChild(bifBranchSep);

// create starry background
    static int iiii = 0;
    if(iiii && options[OPT_BACKGROUND])
    {
        char bgFileName[256];
        strcpy(bgFileName, autoDir);
        strcat(bgFileName, "/plaut04/widgets/background.rgb");
        result->addChild(drawStarryBackground(bgFileName));
    }

// add legend
    if(iiii && options[OPT_LEGEND])
    {
        result->addChild(addLegend());
    }
    iiii++;

    return result;
}

///////////////////////////////////////////////////////////////////////////
//
static SoSeparator *
drawALabel(float (*xyzCoords)[3], int row, float xoffset, long int label)
//
///////////////////////////////////////////////////////////////////////////
{
    SoSeparator *result = new SoSeparator;
    SoTranslation *labelTranslate = new SoTranslation;
    SoText2 *labelMsg = new SoText2;
    SoFont *labelFont = new SoFont;
    labelFont->name.setValue("Helvetica");
    labelFont->size.setValue(12);

    float x, y, z;

    x = myBifNode.xyzCoords[row][0];
    y = myBifNode.xyzCoords[row][1];
    z = myBifNode.xyzCoords[row][2];

    //pick a good direction for the label
    int first = row;
    int second = row + 1;
    if (second >= myBifNode.totalNumPoints)
    {
        first--;
        second--;
    }
    if (first < 0) first = 0;
    float yoffset = xoffset;
    if ((xyzCoords[second][1] - xyzCoords[first][1]) *
        (xyzCoords[second][0] - xyzCoords[first][0]) >= 0)
    {
        xoffset = -xoffset;
        labelMsg->justification = SoText2::RIGHT;
    }
    labelTranslate->translation.setValue(x + xoffset, y + yoffset, z);
    char a[30];
#ifdef R3B
    label--;
#endif
    sprintf(a, "%d", label);
    labelMsg->string.setValue(a);
    result->addChild(labelTranslate);
    result->addChild(labelFont);
    result->addChild(labelMsg);
    return result;
}


//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawLabelPtsInBifurcationScene()
//
//////////////////////////////////////////////////////////////////////////
{
    float dis = !options[OPT_NORMALIZE_DATA] ? fabs(max(max((myBifNode.max[0]-myBifNode.min[0]),
        (myBifNode.max[1]-myBifNode.min[1])),
        (myBifNode.max[2]-myBifNode.min[2]))) : 2.0;

    SoSeparator * result = new SoSeparator;
    int lbl = myLabels[lblIndices[0]], row = 0;
    float position[3];
    int lbType;

    if(lbl == 0)
    {
        int k = 0;
        do
        {
            SoMaterial *lblMtl;
#ifndef R3B
            row = clientData.labelIndex[k][1] - 1;
#else
            row = clientData.labelIndex[k][1];
#endif
            lbType = clientData.labelIndex[k][2];
    
            lblMtl = setLabelMaterial(lbType);
            result->addChild(lblMtl);
    
            position[0] = myBifNode.xyzCoords[row][0];
            position[1] = myBifNode.xyzCoords[row][1];
            position[2] = myBifNode.xyzCoords[row][2];

            float size = dis*0.005*sphereRadius;
#ifndef R3B
            size *= lineWidthScaler;
#endif
            result->addChild( drawASphere(position, size));
            if(options[OPT_LABEL_NUMBERS])
                result->addChild( drawALabel(myBifNode.xyzCoords, row, size,
                                             myBifNode.labels[k]));

            ++k;
#ifndef R3B
        } while( k < clientData.totalLabels);
#else
        } while( k <= clientData.totalLabels);
#endif
    }
    else if(lbl != MY_NONE)
    {
        for(int i=0; i<lblIdxSize; ++i)
        {
            SoMaterial *lblMtl;

#ifndef R3B
            int k = lblIndices[i]-1;
            row = clientData.labelIndex[k][1] -1;
#else
            int k = lblIndices[i];
            row = clientData.labelIndex[k][1];
#endif
            lbType = clientData.labelIndex[k][2];

            lblMtl = setLabelMaterial(lbType);
            result->addChild(lblMtl);

            position[0] = myBifNode.xyzCoords[row][0];
            position[1] = myBifNode.xyzCoords[row][1];
            position[2] = myBifNode.xyzCoords[row][2];

	    float size = dis*0.005*sphereRadius;
#ifndef R3B
            size *= lineWidthScaler;
#endif
            result->addChild( drawASphere(position, size));
            if(options[OPT_LABEL_NUMBERS])
                result->addChild( drawALabel(myBifNode.xyzCoords, row, size,
                                             myBifNode.labels[k]));
        }
    }
    return result;
}


/////////////////////////////////////////////////////////////////
//
//                 Set Material for Labels
//  This function sets the material(color) for each different
//    type of labels.
//
SoMaterial *
setLabelMaterial(int lblType)
//
/////////////////////////////////////////////////////////////////
{
    SoMaterial *lblMtl = new SoMaterial;
    lblMtl->transparency = 0.0;
    switch(lblType)
    {
        case 0 :
            lblMtl->diffuseColor.setValue(lineColor[0]);
            break;
        case 1 : 
            lblMtl->diffuseColor.setValue(lineColor[1]);
            break;
        case 2 :
            lblMtl->diffuseColor.setValue(lineColor[2]);
            break;
        case 3 :
            lblMtl->diffuseColor.setValue(lineColor[3]);
            break;
        case 4 :
            lblMtl->diffuseColor.setValue(lineColor[4]);
            break;
        case -4 :
            lblMtl->diffuseColor.setValue(lineColor[5]);
            break;
        case 5 : 
            lblMtl->diffuseColor.setValue(lineColor[6]);
            break;
        case 6 : 
            lblMtl->diffuseColor.setValue(lineColor[7]);
            break;
        case 7 :
            lblMtl->diffuseColor.setValue(lineColor[8]);
            break;
        case 8 :
            lblMtl->diffuseColor.setValue(lineColor[9]);
            break;
        case 9 :
            lblMtl->diffuseColor.setValue(lineColor[10]);
            break;
        case -9 :
            lblMtl->diffuseColor.setValue(lineColor[11]);
            break;
        default :
            lblMtl->diffuseColor.setValue(lineColor[12]);
    }
    return lblMtl;
}


/////////////////////////////////////////////////////////////////
//
SoSeparator *
drawABifBranchUsingTubes(int iBranch, long int l,
long int sumX, float scaler, int stability, int type)
//
//
/////////////////////////////////////////////////////////////////
{
    SoSeparator * tSep = new SoSeparator;
    long int upperlimit = myBifNode.numVerticesEachBranch[l-1];

#ifndef R3B
    if(upperlimit <= 1)
    {
        return tSep;
    }

#endif
    float (*path)[3] = new float[upperlimit][3];
    float *colorBase = new float[upperlimit*11];
    Tube tube;

#ifdef R3B
    if(upperlimit <= 1) return tSep;

#endif
    for(long int i=0; i<upperlimit; i++)
    {
        long int idx = i+sumX;
        path[i][0]=myBifNode.xyzCoords[idx][0];
        path[i][1]=myBifNode.xyzCoords[idx][1];
        path[i][2]=myBifNode.xyzCoords[idx][2];
        if(coloringMethod>=0)
            for(int k=0; k<11; ++k)
                colorBase[i*11+k]  = clientData.bifData[idx][coloringMethod];
        else if(coloringMethod==CL_POINT_NUMBER)
            for(int k=0; k<11; ++k)
                colorBase[i*11+k]  = i;
        else if(coloringMethod == CL_STABILITY)
            for(int k=0; k<11; ++k)
                colorBase[i*11+k] = myBifNode.ptStability[idx];
    }

    if(coloringMethod == CL_BRANCH_NUMBER)
#ifndef R3B
        tSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
#else
        tSep->addChild(setLineAttributesByBranch(myBifNode.branchID[iBranch], stability, scaler));
#endif
    else if(coloringMethod == CL_STABILITY)
        tSep->addChild(setLineColorBlendingByStability(colorBase, upperlimit*11, stability, scaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        tSep->addChild(setLineAttributesByType(stability, type, scaler));
    else
        tSep->addChild(setLineColorBlending(colorBase,
            upperlimit*11, stability, type, scaler));

    tube = Tube(upperlimit, path, lineWidthScaler*0.005, 10);
    tSep->addChild(tube.createTube());
    delete [] path;
    delete [] colorBase;
    return tSep;
}


/////////////////////////////////////////////////////////////////
//
//                  create bifurcation scene
//
SoSeparator *
#ifndef R3B
renderBifurcation()
#else
renderBifurcation(double mass)
#endif
//
//////////////////////////////////////////////////////////////////////////
{
//    SoDrawStyle *drawStyle = new SoDrawStyle;
//    drawStyle->lineWidth = 1.0;
#ifdef R3B
    SoDrawStyle *drawStyle = new SoDrawStyle;
    drawStyle->lineWidth = 1.0;
#endif

    SoSeparator *bifSep = new SoSeparator;
    SoMaterial *bifMtl= new SoMaterial;

    bifMtl->ambientColor.setValue(0.5,0.5,0.5);
    bifMtl->diffuseColor.setValue(0.9,1.0,0.9);
    bifMtl->specularColor.setValue(0.0,0.5,0.5);
    bifMtl->shininess = 0.5;
    bifMtl->transparency = 0.0;

    SoGroup *bifGroup = new SoGroup;

//    SoLineSet *bifLines = new SoLineSet;
//    SoCoordinate3 *bifCoords = new SoCoordinate3;
#ifdef R3B
    SoLineSet *bifLines = new SoLineSet;
    SoCoordinate3 *bifCoords = new SoCoordinate3;
#endif

    if(whichStyle == TUBE )
    {
        long int si = 0, k = 0;
        for(int ka=0; ka<myBifNode.numBranches; ka++)
        {
            k = k+1;
            bifGroup->addChild(drawABifBranchUsingTubes(ka, k, si, 1*lineWidthScaler,
#ifdef R3B
                clientData.labelIndex[k][3], clientData.labelIndex[k][2]));
#else
                clientData.labelIndex[k-1][3], clientData.labelIndex[k-1][2]));
#endif
            si += myBifNode.numVerticesEachBranch[ka];
        }
    }
    else if (whichStyle == NURBS)
    {
        long int si = 0, k = 0;
        for(int ka=0; ka<myBifNode.numBranches; ka++)
        {
            k = k+1;
            bifGroup->addChild(drawABifBranchUsingNurbsCurve(ka, k, si, 1*lineWidthScaler,
#ifdef R3B
                clientData.labelIndex[k][3], clientData.labelIndex[k][2]));
#else
                clientData.labelIndex[k-1][3], clientData.labelIndex[k-1][2]));
#endif
            si += myBifNode.numVerticesEachBranch[ka];
        }

    }
    else 
    {
        long int si = 0, k = 0;
        for(int ka=0; ka<myBifNode.numBranches; ka++)
        {
            k = k+1;
            bifGroup->addChild(drawABifBranchUsingLines(ka, k, si, 1*lineWidthScaler,
#ifdef R3B
                clientData.labelIndex[k][3], clientData.labelIndex[k][2]));
#else
                clientData.labelIndex[k-1][3], clientData.labelIndex[k-1][2]));
#endif
            si += myBifNode.numVerticesEachBranch[ka];
        }
    }

    if(options[OPT_DRAW_LABELS])
        bifSep->addChild(drawLabelPtsInBifurcationScene());

    bifSep->addChild(bifMtl);
    bifSep->addChild(bifGroup);

    return bifSep;
}


//////////////////////////////////////////////////////////////////////////
//
// Description: Draw the whole solution family using TUBES.
// Draw the whole solution family using TUBES.
//
SoSeparator *
drawSolUsingTubes()
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator * tubeSep = new SoSeparator;

    float dis = (!options[OPT_NORMALIZE_DATA]) ? (max(max(fabs(mySolNode.max[0]-mySolNode.min[0]),
        fabs(mySolNode.max[1]-mySolNode.min[1])),
        fabs(mySolNode.max[2]-mySolNode.min[2]))) : 2;

#ifndef R3B
    int32_t myint[10];
#endif
    long int sumX = 0;
    int iBranch = 0;
    int curBranchID = mySolNode.branchID[iBranch];
    int sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];
    float scaler = 0.1;

    for(long int j=0; j<mySolNode.numOrbits; ++j)
    {
        if(j >= sumOrbit)
        {
            curBranchID = mySolNode.branchID[++iBranch];
            sumOrbit   += mySolNode.numOrbitsInEachBranch[iBranch];
        }
        long int upperlimit = mySolNode.numVerticesEachPeriod[j];

        if(upperlimit >0 )
        {
            if(upperlimit == 1)
            {
                int idx = sumX;
                SoSeparator * ptSep = new SoSeparator;
                SoSphere * aPoint = new SoSphere;
                aPoint->radius = dis*STATIONARY_POINT_RADIUS;

#ifdef R3B
                int stability=clientData.labelIndex[j+1][3];
                int type =clientData.labelIndex[j+1][2];
#else
                int stability=clientData.labelIndex[j][3];
                int type =clientData.labelIndex[j][2];
#endif
                float scaler = lineWidthScaler;

                if(coloringMethod == CL_BRANCH_NUMBER)
                    ptSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
                else if(coloringMethod == CL_STABILITY)
                    ptSep->addChild(setLineAttributesByStability(stability, scaler));
                else if(coloringMethod == CL_ORBIT_TYPE)
                    ptSep->addChild(setLineAttributesByType(stability, type, scaler));
                else if(coloringMethod == CL_LABELS)
                {
                    ptSep->addChild(setLineAttributesByParameterValue(
                        j, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                        stability, scaler));
                }
                else if(coloringMethod == CL_COMPONENT)
                {
                    ptSep->addChild(setLineAttributesByParameterValue(
                        curComponent, maxComponent, maxComponent/2.0, 0,
                        stability, scaler));
                }
                else 
                {
                    SoMaterial * ptMtl = new SoMaterial;
                    ptMtl->diffuseColor.setValue(1,0,0);
                    ptSep->addChild(ptMtl);
                }

#ifndef R3B
        
                float ver[2][3];
        
                if(time_on != TIME_IS_OFF)
                {
                    if(time_on == TIME_ON_X)
                    {
                        ver[0][0] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0; //-(mySolNode.max[0]-mySolNode.min[0]) : 0;
                        ver[0][1] = mySolNode.xyzCoords[idx][1];
                        ver[0][2] = 0;
                        ver[1][0] = (mySolNode.max[0] <= 1.0) ? 1.0 : mySolNode.max[0];
                        ver[1][1] = mySolNode.xyzCoords[idx][1];
                        ver[1][2] = 0;
                    }
                    else if(time_on == TIME_ON_Y)
                    {
                        ver[0][0] = mySolNode.xyzCoords[idx][0];
                        ver[0][1] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0; //-(mySolNode.max[1]-mySolNode.min[1]) : 0;
                        ver[0][2] = 0;
                        ver[1][0] = mySolNode.xyzCoords[idx][0]; 
                        ver[1][1] = ( mySolNode.max[1] <= 1.0 ) ? 1.0 : mySolNode.max[1];
                        ver[1][2] = 0;
                    }
                    else if(time_on == TIME_ON_Z)
                    {
                        ver[0][0] = mySolNode.xyzCoords[idx][0];
                        ver[0][1] = 0; 
                        ver[0][2] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0; //-(mySolNode.max[2]-mySolNode.min[2]) : 0; 
                        ver[1][0] = mySolNode.xyzCoords[idx][0];
                        ver[1][1] = 0; 
                        ver[1][2] = ( mySolNode.max[2] <= 1.0 ) ? 1.0 : mySolNode.max[2]; 
                    }
        
                    SoCoordinate3 *myC = new SoCoordinate3;
                    myC->point.setValues(0, 2, ver);
                    myint[0]=2;
                    SoLineSet *myL = new SoLineSet;
                    myL->numVertices.setValues(0, 1, myint);
                    ptSep->addChild(myC);
                    ptSep->addChild(myL);
                    tubeSep->addChild(ptSep);
                }
                else
                {
#endif
                    SoTransform * aTrans = new SoTransform;
#ifndef R3B
                    aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], 
        		           mySolNode.xyzCoords[idx][1], mySolNode.xyzCoords[idx][2]);
                    ptSep->addChild(aTrans);
                    SoSphere *aPoint = new SoSphere;
                    aPoint->radius = dis * STATIONARY_POINT_RADIUS;
                    ptSep->addChild(aPoint);
                    tubeSep->addChild(ptSep);
                }
#else
                aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], 
                                            mySolNode.xyzCoords[idx][1], mySolNode.xyzCoords[idx][2]);
                ptSep->addChild(aTrans);
                ptSep->addChild(aPoint);
#endif
                tubeSep->addChild(ptSep);
            }
            else
            {

                float (*path)[3] = new float[upperlimit][3];
                float *colorBase = new float[upperlimit*11];
                Tube tube;
                for(int i=0; i<upperlimit; i++)
                {
                    int idx = i+sumX;
                    path[i][0]=mySolNode.xyzCoords[idx][0];
                    path[i][1]=mySolNode.xyzCoords[idx][1];
                    path[i][2]=mySolNode.xyzCoords[idx][2];
                    if(coloringMethod>=0)
                        for(int k=0; k<11; ++k)
                            colorBase[i*11+k]  = clientData.solData[idx][coloringMethod];
                    if(coloringMethod==CL_POINT_NUMBER)
                        for(int k=0; k<11; ++k)
                            colorBase[i*11+k]  = i;

                }
#ifdef R3B
                int stability=clientData.labelIndex[j+1][3];
                int type =clientData.labelIndex[j+1][2];
#else
                int stability=clientData.labelIndex[j][3];
                int type =clientData.labelIndex[j][2];
#endif
                float scaler = lineWidthScaler;

//                if(maxComponent == 1)
//                {
#ifdef R3B
                if(maxComponent == 1)
                {
#endif
                    if(coloringMethod == CL_BRANCH_NUMBER)
#ifndef R3B
                        tubeSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
#else
                        tubeSep->addChild(setLineAttributesByBranch(mySolNode.branchID[iBranch], stability, scaler));
#endif
                    else if(coloringMethod == CL_STABILITY)
                        tubeSep->addChild(setLineAttributesByStability(stability, scaler));
                    else if(coloringMethod == CL_ORBIT_TYPE)
                        tubeSep->addChild(setLineAttributesByType(stability, type, scaler));
                    else if(coloringMethod == CL_LABELS)
                    {
#ifdef R3B
                        double bMin = 0;
                        for(int ib = 0; ib< iBranch; ++ib)
                            bMin +=  mySolNode.numOrbitsInEachBranch[ib];
                        double bMax = bMin+mySolNode.numOrbitsInEachBranch[iBranch]-1;
                        tubeSep->addChild(setLineAttributesByParameterValue(
                            j, bMax, (bMax+bMin)/2.0, bMin,
                            stability, scaler));
#else
                        tubeSep->addChild(setLineAttributesByParameterValue(
                            j, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                            stability, scaler));
#endif
                    }
#ifndef R3B
                    else if(coloringMethod == CL_COMPONENT)
                        tubeSep->addChild(setLineAttributesByParameterValue(
                                curComponent, maxComponent, maxComponent/2.0, 0,
                                stability, scaler));
#endif
                    else if(coloringMethod >= mySolNode.nar)
                        tubeSep->addChild(setLineAttributesByParameterValue(
                                mySolNode.par[j][mySolNode.parID[coloringMethod-mySolNode.nar]],
                                mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                                mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                                mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                                stability, scaler));
                    else
                        tubeSep->addChild(setLineColorBlending(colorBase,
                            upperlimit*11,stability, type, scaler));
//                }
//                else
//                {
//                    tubeSep->addChild(setLineAttributesByParameterValue(
//                        curComponent, maxComponent, maxComponent/2.0, 0,
//                        stability, scaler));
//                }

#ifdef R3B
                }
                else
                {
                    tubeSep->addChild(setLineAttributesByParameterValue(
                        curComponent, maxComponent, maxComponent/2.0, 0,
                        stability, scaler));
                }
#endif
                tube = Tube(upperlimit, path, lineWidthScaler*0.005, 10);
                tubeSep->addChild(tube.createTube());
                delete [] path;
                delete [] colorBase;
            }
        }
        sumX += upperlimit;
    }
    return tubeSep;
}


//////////////////////////////////////////////////////////////////////////
//
// draw the diagram with surface
//
SoSeparator *
drawASolBranchUsingSurface(long obStart, long obEnd, long numVert)
//
//////////////////////////////////////////////////////////////////////////
{
    float dis = (!options[OPT_NORMALIZE_DATA]) ? (max(max(fabs(mySolNode.max[0]-mySolNode.min[0]),
        fabs(mySolNode.max[1]-mySolNode.min[1])),
        fabs(mySolNode.max[2]-mySolNode.min[2]))) : 2.0;

    SoSeparator *solSurface = new SoSeparator;
    float (*strip)[3];
    long int npt = numVert;
    long int sum = 0;

    strip = new float[2*npt][3];

//write the strip set
    if((obEnd-obStart)>1)
    {
        if(npt>1)
        {
            for(int i=0; i<npt; i++)
            {
                strip[i*2][0] = mySolNode.xyzCoords[i][0];
                strip[i*2][1] = mySolNode.xyzCoords[i][1];
                strip[i*2][2] = mySolNode.xyzCoords[i][2];
            }
        }
        else
        {
#ifndef R3B
	    printf(" Only one point in the period, no surface can be drawn!\n");
#else
            printf("Only one point in the period, no surface can be drawn!\n");
#endif
            long int idx = obStart;
            SoSeparator * ptSep = new SoSeparator;
            SoTransform * aTrans = new SoTransform;
            aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], 
			            mySolNode.xyzCoords[idx][1], mySolNode.xyzCoords[idx][2]);
            ptSep->addChild(aTrans);

            SoSphere *aPoint = new SoSphere;
            aPoint->radius = dis * STATIONARY_POINT_RADIUS;
            ptSep->addChild(aPoint);
            solSurface->addChild(ptSep);
            return solSurface;
        }

        sum += npt;
        for(int j=obStart; j<obEnd; ++j)
        {
            npt= mySolNode.numVerticesEachPeriod[j];
            if(npt>1)
            {
                for(int i=0; i<npt; i++)
                {
                    strip[i*2+1][0] = mySolNode.xyzCoords[i+sum][0];
                    strip[i*2+1][1] = mySolNode.xyzCoords[i+sum][1];
                    strip[i*2+1][2] = mySolNode.xyzCoords[i+sum][2];
                }
                solSurface->addChild(drawAStrip(strip,npt*2));
                for(int i=0; i<npt; i++)
                {
                    strip[i*2][0] = strip[i*2+1][0];
                    strip[i*2][1] = strip[i*2+1][1];
                    strip[i*2][2] = strip[i*2+1][2];
                }
                sum += npt;
            }
            else
            {
                printf(" Only one point in the period, no surface can be drawn!\n");
                long int idx = sum;
                SoSeparator * ptSep = new SoSeparator;
                SoTransform * aTrans = new SoTransform;
                aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], 
				           mySolNode.xyzCoords[idx][1], mySolNode.xyzCoords[idx][2]);
                ptSep->addChild(aTrans);

                SoSphere *aPoint = new SoSphere;
                aPoint->radius = dis * STATIONARY_POINT_RADIUS;
                ptSep->addChild(aPoint);
                solSurface->addChild(ptSep);
                return solSurface;

            }
        }
    }
    else
    {
        printf(" Only one solution! No surface is able to be drawn!\n");
        printf(" Choose LINE/TUBE to view it.\n");
        return NULL;
    }
    delete [] strip;
    return solSurface;
}


///////////////////////////////////////////////////////////////////
//
SoGroup *
renderSolutionTubes()
//
//////////////////////////////////////////////////////////////////////////
{
    SoGroup * solGroup = new SoGroup;
    SoCoordinate3 *solCoords = new SoCoordinate3;

// draw every orbit by using giving tube thickness and color.
    if(animationLabel == 0)
        solGroup->addChild(drawSolUsingTubes());
    else if(animationLabel != MY_NONE)
    {
        for(int n=0; n<lblIdxSize; ++n)
        {
            animationLabel=myLabels[lblIndices[n]];

            int si = 0, k = 0;
            int iBranch = 0;
            int curBranchID = mySolNode.branchID[iBranch];
            int sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];

            for(int ka=0; ka<mySolNode.numOrbits; ka++)
            {
                if(ka >= sumOrbit)
                {
                    curBranchID = mySolNode.branchID[++iBranch];
                    sumOrbit   += mySolNode.numOrbitsInEachBranch[iBranch];
                }
                k = k+1;
                if(myLabels[ka+1]>=animationLabel) break;
                si += mySolNode.numVerticesEachPeriod[ka];
            }

            if(options[OPT_SAT_ANI])
            {
                solGroup->addChild(animateOrbitWithTail(iBranch, k, si));
            }
            else
            {
                solGroup->addChild(drawAnOrbitUsingTubes(iBranch, k, si, 1*lineWidthScaler,
#ifdef R3B
                 clientData.labelIndex[k][3], clientData.labelIndex[k][2]));
#else
                 clientData.labelIndex[k-1][3], clientData.labelIndex[k-1][2]));
#endif
            }
        }
    }

    if(options[OPT_PERIOD_ANI])
    {
        bool aniColoring;
        aniColoring = (animationLabel == 0) ? false : true;
        solGroup->addChild(animateSolutionUsingTubes(aniColoring));
    }
    return solGroup;
}


//////////////////////////////////////////////////////////////////////////
//
SoGroup *
renderSolutionSurface()
//
//////////////////////////////////////////////////////////////////////////
{
    SoGroup *solGroup = new SoGroup;
    SoMaterial *solMtl = new SoMaterial;
#ifndef R3B
    solMtl->diffuseColor.setValue(envColors[5]);
#else
    solMtl->diffuseColor.setValue(envColors[9]);
#endif
    solGroup->addChild(solMtl);
    SoMaterialBinding *myMtlBinding = new SoMaterialBinding;
    myMtlBinding->value= SoMaterialBinding::PER_PART;
    solGroup->addChild(myMtlBinding);

    long start = 0;
    long end = 0;

    int sumOrbit = 0;
    long numVert = 0;
    for(int iBranch = 0; iBranch < mySolNode.numBranches; ++iBranch)
    {
        end += mySolNode.numOrbitsInEachBranch[iBranch];
        numVert = mySolNode.numVerticesEachPeriod[sumOrbit];
        SoSeparator * as = drawASolBranchUsingSurface(start+1, end, numVert);
        if(as !=NULL)
            solGroup->addChild(as);
        start += mySolNode.numOrbitsInEachBranch[iBranch];
        sumOrbit += mySolNode.numOrbitsInEachBranch[iBranch];
    }

    if(options[OPT_PERIOD_ANI])
        solGroup->addChild(animateSolutionUsingTubes(true));

    int si = 0, k = 0;
    int iBranch = 0;
    int curBranchID = mySolNode.branchID[iBranch];
    sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];
    for(int ka=0; ka<mySolNode.numOrbits; ka++)
    {
        if(ka >= sumOrbit)
        {
            curBranchID = mySolNode.branchID[++iBranch];
            sumOrbit   += mySolNode.numOrbitsInEachBranch[iBranch];
        }
        k = k+1;
        if(myLabels[ka+1]>=animationLabel) break;
        si += mySolNode.numVerticesEachPeriod[ka];
    }

    if(options[OPT_SAT_ANI])
        solGroup->addChild(animateOrbitWithTail(iBranch, k, si));
    else
        solGroup->addChild(drawAnOrbitUsingTubes(iBranch, k, si, 1*lineWidthScaler,
#ifdef R3B
            clientData.labelIndex[k][3], clientData.labelIndex[k][2]));
#else
            clientData.labelIndex[k-1][3], clientData.labelIndex[k-1][2]));
#endif
    return solGroup;
}


///////////////////////////////////////////////////////////////////////
//
//           draw the solutions by Mesh points
//
//           draw the solutions by Points
SoGroup *
renderSolutionPoints(int style)
//
//////////////////////////////////////////////////////////////////////////
{
    SoGroup       *solGroup  = new SoGroup;
    SoCoordinate3 *solCoords = new SoCoordinate3;

    if(animationLabel == 0)
    {
        long int si = 0, k = 0;
        int iBranch = 0;
        int curBranchID = mySolNode.branchID[iBranch];
        int sumOrbit = mySolNode.numOrbitsInEachBranch[iBranch];

        for(long int ka=0; ka<mySolNode.numOrbits; ka++)
        {
            if(ka >= sumOrbit)
            {
                curBranchID = mySolNode.branchID[++iBranch];
                sumOrbit+= mySolNode.numOrbitsInEachBranch[iBranch];
            }

            k = k+1;
            solGroup->addChild(drawAnOrbitUsingPoints(style, iBranch, /*curBranchID,*/ k, si, lineWidthScaler,
#ifdef R3B
                clientData.labelIndex[k][3], clientData.labelIndex[k][2], true));
#else
                clientData.labelIndex[k-1][3], clientData.labelIndex[k-1][2], true));
#endif
            si += mySolNode.numVerticesEachPeriod[ka];
        }
    }
    else if(animationLabel != MY_NONE)
    {
        for(int n=0; n<lblIdxSize; ++n)
        {
            animationLabel=myLabels[lblIndices[n]];
            int si = 0, k = 0;
            int iBranch = 0;
            int curBranchID = mySolNode.branchID[iBranch];
            int sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];
            for(int ka=0; ka<mySolNode.numOrbits; ka++)
            {
                if(ka >= sumOrbit)
                {
                    curBranchID = mySolNode.branchID[++iBranch];
                    sumOrbit+= mySolNode.numOrbitsInEachBranch[iBranch];
                }

                k = k+1;
                if(myLabels[ka+1]>=animationLabel) break;
                si += mySolNode.numVerticesEachPeriod[ka];
            }
            if(options[OPT_SAT_ANI])
            {
                solGroup->addChild(animateOrbitWithTail(iBranch, k, si));
            }
            else
            {
                solGroup->addChild(drawAnOrbitUsingPoints(style, iBranch, k, si, lineWidthScaler,
#ifdef R3B
                    clientData.labelIndex[k][3], clientData.labelIndex[k][2], true));
#else
                    clientData.labelIndex[k-1][3], clientData.labelIndex[k-1][2], true));
#endif
            }
        }
    }

    if(options[OPT_PERIOD_ANI])
    {
        bool coloring;
        coloring = (animationLabel == 0) ? false : true;
        solGroup->addChild(animateSolutionUsingPoints(style, coloring));
    }

    return solGroup;
}


///////////////////////////////////////////////////////////////////////
//
//           draw the solutions by lines
//
SoGroup *
renderSolutionLines()
//
//////////////////////////////////////////////////////////////////////////
{
    SoGroup       *solGroup  = new SoGroup;
    SoCoordinate3 *solCoords = new SoCoordinate3;

    if(animationLabel == 0)
    {
        long int si = 0, k = 0;
        int iBranch = 0;
        int curBranchID = mySolNode.branchID[iBranch];
        int sumOrbit = mySolNode.numOrbitsInEachBranch[iBranch];

        for(long int ka=0; ka<mySolNode.numOrbits; ka++)
        {
            if(ka >= sumOrbit)
            {
                curBranchID = mySolNode.branchID[++iBranch];
                sumOrbit+= mySolNode.numOrbitsInEachBranch[iBranch];
            }

            k = k+1;
            solGroup->addChild(drawAnOrbitUsingLines(iBranch, k, si, lineWidthScaler,
#ifdef R3B
                clientData.labelIndex[k][3], clientData.labelIndex[k][2], true));
#else
                clientData.labelIndex[k-1][3], clientData.labelIndex[k-1][2], true));
#endif
            si += mySolNode.numVerticesEachPeriod[ka];
        }
    }
    else if(animationLabel != MY_NONE)
    {
        for(int n=0; n<lblIdxSize; ++n)
        {
            animationLabel=myLabels[lblIndices[n]];
            int si = 0, k = 0;
            int iBranch = 0;
            int curBranchID = mySolNode.branchID[iBranch];
            int sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];
            for(int ka=0; ka<mySolNode.numOrbits; ka++)
            {
                if(ka >= sumOrbit)
                {
                    curBranchID = mySolNode.branchID[++iBranch];
                    sumOrbit+= mySolNode.numOrbitsInEachBranch[iBranch];
                }

                k = k+1;
                if(myLabels[ka+1]>=animationLabel) break;
                si += mySolNode.numVerticesEachPeriod[ka];
            }

            if(options[OPT_SAT_ANI])
            {
                solGroup->addChild(animateOrbitWithTail(iBranch, k, si));
            }
            else
            {
                solGroup->addChild(drawAnOrbitUsingLines(iBranch, k, si, lineWidthScaler,
#ifdef R3B
                    clientData.labelIndex[k][3], clientData.labelIndex[k][2], true));
#else
                    clientData.labelIndex[k-1][3], clientData.labelIndex[k-1][2], true));
#endif
            }
        }
    }

    if(options[OPT_PERIOD_ANI])
    {
        bool coloring;
        coloring = (animationLabel == 0) ? false : true;
        solGroup->addChild(animateSolutionUsingLines(coloring));
    }

    return solGroup;
}


///////////////////////////////////////////////////////////////////////
//
//           draw the solutions using NURBS CURVE lines
//
SoGroup *
renderSolutionNurbsCurve()
//
//////////////////////////////////////////////////////////////////////////
{
    SoGroup * solGroup = new SoGroup;

    SoCoordinate3 *solCoords = new SoCoordinate3;

    if(animationLabel == 0)
    {
        long int si = 0, k = 0;
        int iBranch = 0;
        int curBranchID = mySolNode.branchID[iBranch];
        int sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];
        for(long int ka=0; ka<mySolNode.numOrbits; ka++)
        {
            if(ka >= sumOrbit)
            {
                curBranchID = mySolNode.branchID[++iBranch];
                sumOrbit   += mySolNode.numOrbitsInEachBranch[iBranch];
            }
            k = k+1;
            solGroup->addChild(drawAnOrbitUsingNurbsCurve(iBranch, k, si, lineWidthScaler,
#ifdef R3B
                clientData.labelIndex[k][3], clientData.labelIndex[k][2]));
#else
                clientData.labelIndex[k-1][3], clientData.labelIndex[k-1][2]));
#endif
            si += mySolNode.numVerticesEachPeriod[ka];
        }
    }
    else if(animationLabel != MY_NONE)
    {
        for(int n=0; n<lblIdxSize; ++n)
        {
            animationLabel=myLabels[lblIndices[n]];
            int si = 0, k = 0;
            int iBranch = 0;
            int curBranchID = mySolNode.branchID[iBranch];
            int sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];
            for(int ka=0; ka<mySolNode.numOrbits; ka++)
            {
                if(ka >= sumOrbit)
                {
                    curBranchID = mySolNode.branchID[++iBranch];
                    sumOrbit   += mySolNode.numOrbitsInEachBranch[iBranch];
                }
                k = k+1;
                if(myLabels[ka+1]>=animationLabel) break;
                si += mySolNode.numVerticesEachPeriod[ka];
            }

            if(options[OPT_SAT_ANI])
            {
                solGroup->addChild(animateOrbitWithTail(iBranch, k, si));
            }
            else
            {
                solGroup->addChild(drawAnOrbitUsingNurbsCurve(iBranch, k, si, lineWidthScaler,
#ifdef R3B
                    clientData.labelIndex[k][3], clientData.labelIndex[k][2]));
#else
                    clientData.labelIndex[k-1][3], clientData.labelIndex[k-1][2]));
#endif
            }
        }
    }

    if(options[OPT_PERIOD_ANI])
    {
        bool coloring = (animationLabel == 0) ? true : false;
        solGroup->addChild(animateSolutionUsingLines(coloring));
    }
    return solGroup;
}


//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
animateSolutionUsingPoints(int style, bool aniColoring)
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator *solGroup = new SoSeparator;
    SoSeparator *solStand = new SoSeparator;
    int si = 0;
    SoBlinker * solBlinker = new SoBlinker;
    solBlinker->speed = orbitSpeed;
    solBlinker->on = TRUE;
    int iBranch = 0;
    int curBranchID = mySolNode.branchID[iBranch];
    int sumOrbit = mySolNode.numOrbitsInEachBranch[iBranch];
    for(int l=0; l<mySolNode.numOrbits; l++)
    {
        long numVertices = mySolNode.numVerticesEachPeriod[l];
        if(l >= sumOrbit)
        {
            curBranchID = mySolNode.branchID[++iBranch];
            sumOrbit+= mySolNode.numOrbitsInEachBranch[iBranch];
        }

        if(numVertices == 1)
            solStand->addChild(drawAnOrbitUsingPoints(style, iBranch,/* curBranchID,*/ l+1, si, aniLineScaler*lineWidthScaler,
#ifdef R3B
                clientData.labelIndex[l+1][3], clientData.labelIndex[l+1][2], aniColoring));
#else
                clientData.labelIndex[l][3], clientData.labelIndex[l][2], aniColoring));
#endif
        else
            solBlinker->addChild(drawAnOrbitUsingPoints(style, iBranch,/* curBranchID,*/ l+1, si, aniLineScaler*lineWidthScaler,
#ifdef R3B
                clientData.labelIndex[l+1][3], clientData.labelIndex[l+1][2], aniColoring));
#else
                clientData.labelIndex[l][3], clientData.labelIndex[l][2], aniColoring));
#endif
        si+=mySolNode.numVerticesEachPeriod[l];
    }
    solGroup->addChild(solStand);
    solGroup->addChild(solBlinker);
    return solGroup;
}


//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
animateSolutionUsingLines(bool aniColoring)
//
//////////////////////////////////////////////////////////////////////////
{
// the following is the line version
    SoSeparator *solGroup = new SoSeparator;
    SoSeparator *solStand = new SoSeparator;
    int si = 0;
    SoBlinker * solBlinker = new SoBlinker;
    solBlinker->speed = orbitSpeed;
    solBlinker->on = TRUE;
    int iBranch = 0;
    int curBranchID = mySolNode.branchID[iBranch];
    int sumOrbit = mySolNode.numOrbitsInEachBranch[iBranch];
    for(int l=0; l<mySolNode.numOrbits; l++)
    {
        long numVertices = mySolNode.numVerticesEachPeriod[l];
        if(l >= sumOrbit)
        {
            curBranchID = mySolNode.branchID[++iBranch];
            sumOrbit+= mySolNode.numOrbitsInEachBranch[iBranch];
        }

        if(numVertices == 1)
            solStand->addChild(drawAnOrbitUsingLines(iBranch, l+1, si, aniLineScaler*lineWidthScaler,
#ifdef R3B
                clientData.labelIndex[l+1][3], clientData.labelIndex[l+1][2], aniColoring));
#else
                clientData.labelIndex[l][3], clientData.labelIndex[l][2], aniColoring));
#endif
        else
            solBlinker->addChild(drawAnOrbitUsingLines(iBranch, l+1, si, aniLineScaler*lineWidthScaler,
#ifdef R3B
                clientData.labelIndex[l+1][3], clientData.labelIndex[l+1][2], aniColoring));
#else
                clientData.labelIndex[l][3], clientData.labelIndex[l][2], aniColoring));
#endif
        si+=mySolNode.numVerticesEachPeriod[l];
    }
    solGroup->addChild(solStand);
    solGroup->addChild(solBlinker);
    return solGroup;
}


//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
animateSolutionUsingNurbsCurve()
//
/////////////////////////////////////////////////////////////////
{
    SoSeparator *solGroup = new SoSeparator;
    long int si = 0;
    SoBlinker * solBlinker = new SoBlinker;
    solBlinker->speed = orbitSpeed;
    solBlinker->on = TRUE;
    int iBranch = 0;
    int curBranchID = mySolNode.branchID[iBranch];
    int sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];
    for(long int l=0; l<mySolNode.numOrbits; l++)
    {
        if(l >= sumOrbit)
        {
            curBranchID = mySolNode.branchID[++iBranch];
            sumOrbit   += mySolNode.numOrbitsInEachBranch[iBranch];
        }
        solBlinker->addChild(drawAnOrbitUsingNurbsCurve(iBranch, l+1, si, lineWidthScaler,
#ifdef R3B
            clientData.labelIndex[l+1][3], clientData.labelIndex[l+1][2]));
#else
            clientData.labelIndex[l][3], clientData.labelIndex[l][2]));
#endif
        si+=mySolNode.numVerticesEachPeriod[l];
    }
    solGroup->addChild(solBlinker);
    return solGroup;
}


/////////////////////////////////////////////////////////////////
//                  create solution orbits scene
SoSeparator *
#ifndef R3B
renderSolution()
#else
renderSolution(double mu)
#endif
//
/////////////////////////////////////////////////////////////////
{
    SoSeparator *solSep = new SoSeparator;
    SoGroup *solGroup = new SoGroup;

    SoCoordinate3 *solCoords = new SoCoordinate3;
    if(whichStyle==TUBE)
    {
        solGroup->addChild(renderSolutionTubes());
    }
    else if(whichStyle==SURFACE)
    {
        solGroup->addChild(renderSolutionSurface());
    }
    else if(whichStyle==NURBS) 
    {
        solGroup->addChild(renderSolutionNurbsCurve());
    }
    else if(whichStyle==MESH_POINTS || whichStyle== ALL_POINTS)
    {
        solGroup->addChild(renderSolutionPoints(whichStyle));
    }
    else 
    {
        solGroup->addChild(renderSolutionLines());
    }
    solSep->addChild(solGroup);
    return solSep;
}



#ifdef R3B
///////////////////////////////////////////////////////////////////////////
//
// calculate the current position of the primary at time t in the inertial frame
//
void
calPrimPos(float t, float pos[])
//
///////////////////////////////////////////////////////////////////////////
{
    pos[0] = cos(t);
    pos[1] = sin(t);
    pos[2] = 0;
}


///////////////////////////////////////////////////////////////////////////
//
//         Given time t and the position of the moving primary in the inertial frame
//     to calculate the current position of the satllite position in the intertial frame.
//
void
calSatPos(int center, float mu, float t, float primPos[], float satPos[])
//
///////////////////////////////////////////////////////////////////////////
{
    float a1, a0, b1, b0;
    float c = 0;

// c = 1-mu  if the center of the inertial system is
//           the Earth(smaller primary).
// c = -mu   if the center is the Sun (bigger primary).
// c = 0     if the center is the barycenter.
    if(center == 0)      c = 0;
    else if(center == 1) c = -mu;
    else                 c = 1-mu;

// calculate the current position of the satllite
    a1 = primPos[1];
    a0 = primPos[0];
    b1 = satPos[1];
    b0 = satPos[0];
    satPos[0] = b0*a0-b1*a1;
    satPos[1] = b1*a0+b0*a1;
}


///////////////////////////////////////////////////////////////////////////
//
//
SoSeparator *
animateIner2(long int lblJ,long int si)
//
///////////////////////////////////////////////////////////////////////////
{
    float ptb[3], pts[4][3];
    int center = 0;
    float mu = 0.01215;
    SoSeparator *satGroup = new SoSeparator;
    SoMaterial *satMtl = new SoMaterial;
    satMtl->diffuseColor.setValue(envColors[4]);
    satMtl->transparency = 0.0;

    SoDrawStyle *satStyle = new SoDrawStyle;
    satStyle->style = SoDrawStyle::FILLED;
    satGroup->addChild(satStyle);

    SoSeparator * satSep = new SoSeparator;
    SoBlinker *satBlker = new SoBlinker;

    int upperlimit = mySolNode.numVerticesEachPeriod[lblJ-1];
    int idx = si;
    satBlker->speed = 0.5;

// go thr the whole dataset to find the max and min in this set
// so that we can decide the size of the ball.
    float maxV[3], minV[3];
    maxV[0]=minV[0]=mySolNode.xyzCoords[idx+0][0];
    maxV[1]=minV[1]=mySolNode.xyzCoords[idx+0][0];
    maxV[2]=minV[2]=mySolNode.xyzCoords[idx+0][0];
    for(int i=1; i<upperlimit; i++)
    {
        if(maxV[0]<mySolNode.xyzCoords[idx+i][0]) maxV[0]=mySolNode.xyzCoords[idx+i][0] ;
        if(minV[0]>mySolNode.xyzCoords[idx+i][0]) minV[0]=mySolNode.xyzCoords[idx+i][0] ;
        if(maxV[1]<mySolNode.xyzCoords[idx+i][1]) maxV[1]=mySolNode.xyzCoords[idx+i][1] ;
        if(minV[1]>mySolNode.xyzCoords[idx+i][1]) minV[1]=mySolNode.xyzCoords[idx+i][1] ;
        if(maxV[2]<mySolNode.xyzCoords[idx+i][2]) maxV[2]=mySolNode.xyzCoords[idx+i][2] ;
        if(minV[2]>mySolNode.xyzCoords[idx+i][2]) minV[2]=mySolNode.xyzCoords[idx+i][2] ;
    }
    float dis = fabs(max(max((maxV[0]-minV[0]), (maxV[1]-minV[1])), (maxV[2]-minV[2])));
    float pta[3], vertices[2][3];
    float rgb1[3], rgb2[3];
    rgb1[0]=0; rgb1[2]=0; rgb1[1]=1;
    rgb2[0]=1; rgb2[1]=0; rgb2[2]=0;

// animate the orbit
    ptb[0]=mySolNode.xyzCoords[idx+0][0];
    ptb[1]=mySolNode.xyzCoords[idx+0][1];
    ptb[2]=mySolNode.xyzCoords[idx+0][2];
    float primPos[3];
    float t ;

    float satPeriod ;
    satPeriod = clientData.solPeriod[lblJ];
    float wholePeriod = satPeriod*2.0*M_PI;
    float aniTime = 0.0, dt = 0.0, incTime = 0.0;
    dt = 1.0/200.0;
    do
    {
        pta[0]=ptb[0]; pta[1]=ptb[1]; pta[2]=ptb[2];

//find the point with the time: aniTime;
        int i;
        for(i=0; i<upperlimit; ++i)
        {
            if(mySolNode.time[idx+i]>=aniTime/2.0/M_PI) break;
        }

// calculate the position of this record.
        if(i==upperlimit)
        {
            ptb[0]=mySolNode.xyzCoords[idx+i-1][0];
            ptb[1]=mySolNode.xyzCoords[idx+i-1][1];
            ptb[2]=mySolNode.xyzCoords[idx+i-1][2];
        }
        else if(i==0)
        {
            ptb[0]=mySolNode.xyzCoords[idx][0];
            ptb[1]=mySolNode.xyzCoords[idx][1];
            ptb[2]=mySolNode.xyzCoords[idx][2];
        }
        else
        {
            ptb[0]=(mySolNode.xyzCoords[idx+i-1][0]+mySolNode.xyzCoords[idx+i][0])/2.0;
            ptb[1]=(mySolNode.xyzCoords[idx+i-1][1]+mySolNode.xyzCoords[idx+i][1])/2.0;
            ptb[2]=(mySolNode.xyzCoords[idx+i-1][2]+mySolNode.xyzCoords[idx+i][2])/2.0;
        }

// calculate the position of the primary at aniTime.
        calPrimPos(aniTime, primPos);
        satBlker->addChild(drawASphereWithColor(rgb1, primPos, 1.*dis/100.0));

// calculate the position of the satallite at aniTime.
        calSatPos(center, mu, aniTime, primPos, ptb);
        satBlker->addChild(drawASphereWithColor(rgb2, ptb, 1.*dis/100.0));

        vertices[0][0]=pta[0];
        vertices[0][1]=pta[1];
        vertices[0][2]=pta[2];
        vertices[1][0]=ptb[0];
        vertices[1][1]=ptb[1];
        vertices[1][2]=ptb[2];
        SoCoordinate3 *myCoords = new SoCoordinate3;
        myCoords->point.setValues(0, 2, vertices);

        int32_t myint[1];
        myint[0]=-1;

// define the orbit line set
        SoLineSet *myLine= new SoLineSet;
        myLine->numVertices.setValues(0,1,myint);
        satGroup->addChild(satMtl);
        satGroup->addChild(myCoords);
        satGroup->addChild(myLine);

// increase the time.
        incTime += dt;
        aniTime += dt;
    }while(incTime <= wholePeriod);

    satGroup->addChild(satMtl);
    satGroup->addChild(satBlker);
    return satGroup;
}
#endif


//////////////////////////////// START \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ 
//
// draw a strip by giving the stripset
SoSeparator *
drawAStrip(float stripSet[][3], int size)
//
//////////////////////////////////////////////////////////////////////
{
    SoSeparator * myStrip = new SoSeparator;
    SoShapeHints * solHints = new SoShapeHints;
    solHints->vertexOrdering = SoShapeHints::COUNTERCLOCKWISE;
    myStrip->addChild(solHints);

    SoCoordinate3 *solCoords = new SoCoordinate3;
    solCoords->point.setValues(0, size, stripSet);

    int32_t mySize[1];
    mySize[0] = size;
    SoTriangleStripSet *solStrips = new SoTriangleStripSet;
    solStrips->numVertices.setValues(0, 1, mySize);

    myStrip->addChild(solCoords);
    myStrip->addChild(solStrips);
    return myStrip;
}


//////////////////////////////////////////////////////////////////////////
//
// set parMax Color == RED
//     parMid Color == GREEN
//     parMin Color == BLUE
//
SoGroup *
setLineAttributesByParameterValue(double parValue, double parMax, 
           double parMid, double parMin, int stability, float scaler)
//
//////////////////////////////////////////////////////////////////////////
{
    legendScaleValues[0] = parMin;
    legendScaleValues[1] = parMax;

    SoDrawStyle * lineStyle = new SoDrawStyle;
    lineStyle->style = SoDrawStyle::FILLED;
    if(stability == 1 || stability == 3)
        lineStyle->linePattern = stabilityLinePattern[0];
    else
        lineStyle->linePattern = stabilityLinePattern[1];

    SoMaterial * lineMtl = new SoMaterial;
    lineMtl->diffuseColor.setValue(1.0, 1.0, 1.0);
    lineMtl->transparency = 0.0;

// calculating the color.
    double colFactor;
    if(parMax != parMin)
    {
        if(parValue > parMid)
        {
            colFactor = (parValue-parMid)/(parMax-parMid);
            lineMtl->diffuseColor.setValue(1.0*colFactor, 1.0*(1-colFactor), 0.0);
        }
        else
        {
            colFactor = (parValue-parMin)/(parMid-parMin);
            lineMtl->diffuseColor.setValue(0.0*colFactor, 1.0*colFactor, 1.0*(1-colFactor));
        }
    }

    lineStyle->lineWidth   = 1.5*scaler;

    lineStyle->lineWidth = scaler;

    SoGroup * lineAttributes = new SoGroup;
    lineAttributes->addChild(lineStyle);
    lineAttributes->addChild(lineMtl);

    return lineAttributes;
}


//////////////////////////////////////////////////////////////////////////
//
//  Set the line pattern and color for the orbit according to
//  its stablity and type.
//
SoGroup *
setLineAttributesByBranch(int branchID, int stability, float scaler)
//
//////////////////////////////////////////////////////////////////////////
{
    SoDrawStyle * lineStyle = new SoDrawStyle;
    lineStyle->style = SoDrawStyle::FILLED;
    if(stability == 1 || stability == 3)
        lineStyle->linePattern = stabilityLinePattern[0];
    else
        lineStyle->linePattern = stabilityLinePattern[1];

    SoMaterial * lineMtl = new SoMaterial;
    lineMtl->diffuseColor.setValue(1.0, 1.0, 1.0);
    lineMtl->transparency = 0.0;

    lineStyle->lineWidth   = 1.5*scaler;
    switch(abs(branchID)%13)
    {
        case 0:
            lineMtl->diffuseColor.setValue(lineColor[0]);
            lineStyle->lineWidth = scaler;
            break;
        case 1:
            lineMtl->diffuseColor.setValue(lineColor[1]);
            break;
        case 2:
            lineMtl->diffuseColor.setValue(lineColor[2]);
            break;
        case 3:
            lineMtl->diffuseColor.setValue(lineColor[3]);
            break;
        case 4:
            lineMtl->diffuseColor.setValue(lineColor[4]);
            break;
        case 5:
            lineMtl->diffuseColor.setValue(lineColor[5]);
            break;
        case 6:
            lineMtl->diffuseColor.setValue(lineColor[6]);
            break;
        case 7:
            lineMtl->diffuseColor.setValue(lineColor[7]);
            break;
        case 8:
            lineMtl->diffuseColor.setValue(lineColor[8]);
            break;
        case 9:
            lineMtl->diffuseColor.setValue(lineColor[9]);
            break;
        case 10:
            lineMtl->diffuseColor.setValue(lineColor[10]);
            lineStyle->lineWidth = scaler;
            break;
        case 11:
            lineMtl->diffuseColor.setValue(lineColor[11]);
            lineStyle->lineWidth = scaler;
            break;
        case 12:
        default:
            lineMtl->diffuseColor.setValue(lineColor[12]);
            lineStyle->lineWidth = scaler;
            break;
    }

    SoGroup * lineAttributes = new SoGroup;
    lineAttributes->addChild(lineStyle);
    lineAttributes->addChild(lineMtl);
    return lineAttributes;
}


//////////////////////////////////////////////////////////////////////////
//
SoGroup *
setLineColorBlendingByStability(float * vertices, long int size, int stability, float scaler)
//
//////////////////////////////////////////////////////////////////////////
{
    float (*colors)[3] = new float[size][3];
#ifndef R3B
    static float maxColor[3];
    static float minColor[3]; 
#else
    static float maxColor[3] = { 1.0, 0.0, 0.0 };
    static float minColor[3] = { 0.0, 0.0, 1.0 };
#endif

    for(int i=0; i<3; ++i)
    {
        maxColor[i] = envColors[6][i];
        minColor[i] = envColors[7][i];
    }

    for(int i=0; i<size; ++i)
    {
        if(vertices[i]== 1 || vertices[i]== 3)
            for(int j=0; j<3; ++j) colors[i][j] = maxColor[j];
        else
            for(int j=0; j<3; ++j) colors[i][j] = minColor[j];
    }

    SoGroup * result = new SoGroup ;

#ifndef R3B
    SoDrawStyle * lineStyle = new SoDrawStyle;
    if(stability == 1 || stability == 3)
    {
        lineStyle->style = SoDrawStyle::FILLED;
        lineStyle->linePattern = stabilityLinePattern[0];
    }
    else
    {
        lineStyle->style = SoDrawStyle::FILLED;
        lineStyle->linePattern = stabilityLinePattern[1];
    }
    lineStyle->lineWidth = scaler;
    lineStyle->lineWidth = scaler;
    result->addChild(lineStyle);

#endif
    SoMaterial *myMaterials = new SoMaterial;
    myMaterials->diffuseColor.setValues(0, size, colors);
    result->addChild(myMaterials);

    SoMaterialBinding *myMaterialBinding = new SoMaterialBinding;
    myMaterialBinding->value = SoMaterialBinding::PER_VERTEX;
    result->addChild(myMaterialBinding);
    delete [] colors;
    return result;
}


//////////////////////////////////////////////////////////////////////////
//
SoGroup *
setLineAttributesByStability(int stability, float scaler)
//
//////////////////////////////////////////////////////////////////////////
{
    SoDrawStyle * lineStyle = new SoDrawStyle;
    lineStyle->style = SoDrawStyle::FILLED;

    SoMaterial * lineMtl = new SoMaterial;
    lineMtl->shininess = 0.9;

    if(stability == 1 || stability == 3)
    {
        lineStyle->linePattern = stabilityLinePattern[0];
#ifndef R3B
        lineMtl->diffuseColor.setValue(envColors[6]);
#else
        lineMtl->diffuseColor.setValue(envColors[10]);
#endif
    }
    else
    {
        lineStyle->linePattern = stabilityLinePattern[1];
#ifndef R3B
        lineMtl->diffuseColor.setValue(envColors[7]);
#else
        lineMtl->diffuseColor.setValue(envColors[11]);
#endif
    }

    lineStyle->lineWidth   = scaler;

    SoGroup * lineAttributes = new SoGroup;
    lineAttributes->addChild(lineStyle);
    lineAttributes->addChild(lineMtl);
    return lineAttributes;
}


//////////////////////////////////////////////////////////////////////////
//
//  Set the line pattern and color for the orbit according to
//  its stablity and type.
//
SoGroup *
setLineAttributesByType(int stability, int type, float scaler)
//
//////////////////////////////////////////////////////////////////////////
{
    SoDrawStyle * lineStyle = new SoDrawStyle;

    lineStyle->style = SoDrawStyle::FILLED;
    if(stability == 1 || stability == 3)
        lineStyle->linePattern = stabilityLinePattern[0];
    else
        lineStyle->linePattern = stabilityLinePattern[1];

    SoMaterial * lineMtl = new SoMaterial;
    lineMtl->shininess = 0.9;

    lineStyle->lineWidth   = 1.5*scaler;
    switch(type)
    {
        case 0:
            lineMtl->diffuseColor.setValue(lineColor[0]);
            lineStyle->lineWidth = scaler;
            break;
        case 1:
            lineMtl->diffuseColor.setValue(lineColor[1]);
            break;
        case 2: 
            lineMtl->diffuseColor.setValue(lineColor[2]);
            break;
        case 3: 
            lineMtl->diffuseColor.setValue(lineColor[3]);
            break;
        case 4: 
            lineMtl->diffuseColor.setValue(lineColor[4]);
            break;
        case -4:
            lineMtl->diffuseColor.setValue(lineColor[5]);
            break;
        case 5:
            lineMtl->diffuseColor.setValue(lineColor[6]);
            break;
        case 6: 
            lineMtl->diffuseColor.setValue(lineColor[7]);
            break;
        case 7:
            lineMtl->diffuseColor.setValue(lineColor[8]);
            break;
        case 8:
            lineMtl->diffuseColor.setValue(lineColor[9]);
            break;
        case 9:
            lineMtl->diffuseColor.setValue(lineColor[10]);
            lineStyle->lineWidth = scaler;
            break;
        case -9:
            lineMtl->diffuseColor.setValue(lineColor[11]);
            lineStyle->lineWidth = scaler;
            break;
        default:
            lineMtl->diffuseColor.setValue(lineColor[12]);
            lineStyle->lineWidth = scaler;
            break;
    }

    SoGroup * lineAttributes = new SoGroup;
    lineAttributes->addChild(lineStyle);
    lineAttributes->addChild(lineMtl);
    return lineAttributes;
}


//////////////////////////////////////////////////////////////////////////
//
SoGroup *
setLineColorBlending(float * vertices, long int size, int stability, int type, float scaler)
//
//////////////////////////////////////////////////////////////////////////
{
    float (*colors)[3] = new float[size][3];
    static float maxColor[3] =                    //red
    {
        1.0, 0.0, 0.0
    };
    static float midColor[3] =                    //green
    {
        0.0, 1.0, 0.0
    };
    static float minColor[3] =                    //blue
    {
        0.0, 0.0, 1.0
    };

    float maxValue = vertices[0];
    float minValue = vertices[0];
    float midValue = vertices[0];

    for(int i=0; i<size; ++i)
    {
        if(maxValue<vertices[i])maxValue=vertices[i];
        if(minValue>vertices[i])minValue=vertices[i];
    }

    if( whichType == SOLUTION &&
        (coloringMethod != CL_POINT_NUMBER && animationLabel == 0 || options[OPT_PERIOD_ANI] ))
    {
        legendScaleValues[0]= minValue = clientData.solMin[coloringMethod];
        legendScaleValues[1]= maxValue = clientData.solMax[coloringMethod];
    }
    else
    {
        legendScaleValues[0]= minValue;
        legendScaleValues[1]= maxValue;
    }
    midValue = (maxValue + minValue)/2.0;

// interpolate the colors for each vertices.
    float dt = maxValue - minValue;
    float dtColor;
    for(int i=0; i<size; ++i)
    {
        for(int j=0; j<3; ++j)
        {
            if(vertices[i]>midValue)
            {
                dt = maxValue - midValue;
                dtColor = maxColor[j]-midColor[j];
                colors[i][j] = (dt<1.0e-9) ?
                    ((midColor[j]+maxColor[j])/2.0) :
                (midColor[j] + dtColor * (vertices[i]-midValue)/dt);
            }
            else
            {
                dt = midValue - minValue;
                dtColor = midColor[j]-minColor[j];
                colors[i][j] = (dt<1.0e-9) ?
                    ((minColor[j]+midColor[j])/2.0) :
                (minColor[j] + dtColor * (vertices[i]-minValue)/dt);
            }
        }
    }

    SoGroup * result = new SoGroup ;

// set line style and pattern
    SoDrawStyle * lineStyle = new SoDrawStyle;
    if(stability == 1 || stability == 3)
    {
        lineStyle->style = SoDrawStyle::FILLED;
        lineStyle->linePattern = stabilityLinePattern[0];
    }
    else
    {
        lineStyle->style = SoDrawStyle::FILLED;
        lineStyle->linePattern = stabilityLinePattern[1];
    }
    lineStyle->lineWidth = scaler;
    result->addChild(lineStyle);

    SoMaterial *myMaterials = new SoMaterial;
    myMaterials->diffuseColor.setValues(0, size, colors);
    result->addChild(myMaterials);

    SoMaterialBinding *myMaterialBinding = new SoMaterialBinding;
    myMaterialBinding->value = SoMaterialBinding::PER_VERTEX;
    result->addChild(myMaterialBinding);
    delete [] colors;
    return result;
}


///////////////////////////////////////////////////////////////////////////
//
//         animate the solution by using lines. This version use less memory
//         and much faster.
//       ===============================================================
//         +              +     UNSTABLE STEADY STATE           1
//         +              -     STABLE STEADY STATE             2
//         -              +     UNSTABLE PERIODIC               3
//         -              -     STABLE PERIODIC                 4
//      ===============================================================
//
//
SoSeparator *
drawAnOrbitUsingLines(int iBranch,  long int l, long int si, 
       float scaler, int stability, int type, bool aniColoring)
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator * anOrbit = new SoSeparator;
#ifndef R3B
    int32_t  myint[10];
#endif

    float dis = !options[OPT_NORMALIZE_DATA] ? 
	               (max(max(fabs(mySolNode.max[0]-mySolNode.min[0]),
        fabs(mySolNode.max[1]-mySolNode.min[1])),
        fabs(mySolNode.max[2]-mySolNode.min[2]))) : 2.0;

    long numVertices = mySolNode.numVerticesEachPeriod[l-1];
    if(numVertices == 1 )
    {
        long int idx = si;
        SoSeparator * ptSep = new SoSeparator;
#ifdef R3B
        SoTransform * aTrans = new SoTransform;

#endif

        if(coloringMethod == CL_BRANCH_NUMBER)
            ptSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
        else if(coloringMethod == CL_STABILITY)
            ptSep->addChild(setLineAttributesByStability(stability, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            ptSep->addChild(setLineAttributesByType(stability, type, scaler));
        else if(coloringMethod == CL_LABELS)
        {
            ptSep->addChild(setLineAttributesByParameterValue(
                l-1, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                stability, scaler));
        }
        else if(coloringMethod == CL_COMPONENT)
        {
             ptSep->addChild(setLineAttributesByParameterValue(
                 curComponent, maxComponent, maxComponent/2.0, 0,
                 stability, scaler));
        }
        else 
        {
            SoMaterial * ptMtl = new SoMaterial;
            ptMtl->diffuseColor.setValue(1,0,0);
            ptSep->addChild(ptMtl);
        }

        
#ifndef R3B
        float ver[2][3];

        if(time_on != TIME_IS_OFF)
        {
            if(time_on == TIME_ON_X)
            {
                ver[0][0] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0; //-(mySolNode.max[0]-mySolNode.min[0]) : 0;
                ver[0][1] = mySolNode.xyzCoords[idx][1];
                ver[0][2] = 0;
                ver[1][0] = (mySolNode.max[0] <= 1.0) ? 1.0 : mySolNode.max[0];
                ver[1][1] = mySolNode.xyzCoords[idx][1];
                ver[1][2] = 0;
            }
            else if(time_on == TIME_ON_Y)
            {
                ver[0][0] = mySolNode.xyzCoords[idx][0];
                ver[0][1] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0; //-(mySolNode.max[1]-mySolNode.min[1]) : 0;
                ver[0][2] = 0;
                ver[1][0] = mySolNode.xyzCoords[idx][0]; 
                ver[1][1] = ( mySolNode.max[1] <= 1.0 ) ? 1.0 : mySolNode.max[1];
                ver[1][2] = 0;
            }
            else if(time_on == TIME_ON_Z)
            {
                ver[0][0] = mySolNode.xyzCoords[idx][0];
                ver[0][1] = 0; 
                ver[0][2] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0; //-(mySolNode.max[2]-mySolNode.min[2]) : 0; 
                ver[1][0] = mySolNode.xyzCoords[idx][0];
                ver[1][1] = 0; 
                ver[1][2] = ( mySolNode.max[2] <= 1.0 ) ? 1.0 : mySolNode.max[2]; 
            }

            SoCoordinate3 *myC = new SoCoordinate3;
            myC->point.setValues(0, 2, ver);
            myint[0]=2;
            SoLineSet *myL = new SoLineSet;
            myL->numVertices.setValues(0, 1, myint);
            ptSep->addChild(myC);
            ptSep->addChild(myL);
            anOrbit->addChild(ptSep);
        }
        else
        {
            SoTransform * aTrans = new SoTransform;
#endif
            aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], 
		           mySolNode.xyzCoords[idx][1], mySolNode.xyzCoords[idx][2]);
            ptSep->addChild(aTrans);
            SoSphere *aPoint = new SoSphere;
            aPoint->radius = dis * STATIONARY_POINT_RADIUS;
            ptSep->addChild(aPoint);
            anOrbit->addChild(ptSep);
#ifndef R3B
        }
#endif
        return anOrbit;
    }

#ifdef R3B
    int32_t  myint[10];
#endif
    float (*vertices)[3] = new float[numVertices][3];
    float *colorBase = new float[numVertices];

    for(int m=0; m<numVertices; m++)
    {
        long int idx = si+m;
        vertices[m][0]=mySolNode.xyzCoords[idx][0];
        vertices[m][1]=mySolNode.xyzCoords[idx][1];
        vertices[m][2]=mySolNode.xyzCoords[idx][2];
        if(coloringMethod>=0)colorBase[m]  = clientData.solData[idx][coloringMethod];
        if(coloringMethod==CL_POINT_NUMBER)colorBase[m]  = m;
    }

    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, mySolNode.numVerticesEachPeriod[l-1], vertices);
    myint[0]=mySolNode.numVerticesEachPeriod[l-1];

// define the solution line set
    SoLineSet *myLine= new SoLineSet;
    myLine->numVertices.setValues(0,1,myint);
//    if(maxComponent == 1)
//    {
#ifdef R3B
    if(maxComponent == 1)
    {
#endif
        if(!aniColoring)
        {
            anOrbit->addChild(setLineAttributesByType(stability, 0, scaler));
        }
        else
        {
#ifndef R3B
            if(coloringMethod == CL_BRANCH_NUMBER)
                anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
            else if(coloringMethod == CL_STABILITY)
#else
            if(coloringMethod == CL_STABILITY)
#endif
                anOrbit->addChild(setLineAttributesByStability(stability, scaler));
#ifdef R3B
            else if(coloringMethod == CL_BRANCH_NUMBER)
                anOrbit->addChild(setLineAttributesByBranch(mySolNode.branchID[iBranch], stability, scaler));
#endif
            else if(coloringMethod == CL_ORBIT_TYPE)
                anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
            else if(coloringMethod == CL_LABELS)
            {
#ifdef R3B
                double bMin = 0;
                for(int ib = 0; ib< iBranch; ++ib)
                    bMin +=  mySolNode.numOrbitsInEachBranch[ib];
                double bMax = bMin+mySolNode.numOrbitsInEachBranch[iBranch]-1;
                anOrbit->addChild(setLineAttributesByParameterValue(
                    l-1, bMax, (bMax+bMin)/2.0, bMin,
                    stability, scaler));
#else
                anOrbit->addChild(setLineAttributesByParameterValue(
                     l-1, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                     stability, scaler));
#endif
            }
            else if(coloringMethod == CL_COMPONENT)
                anOrbit->addChild(setLineAttributesByParameterValue(
                        curComponent, maxComponent, maxComponent/2.0, 0,
                        stability, scaler));
            else if(coloringMethod >= mySolNode.nar)
                anOrbit->addChild(setLineAttributesByParameterValue(
                    mySolNode.par[l-1][mySolNode.parID[coloringMethod-mySolNode.nar]],
                    mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                    mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                    mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                    stability, scaler));
            else
                anOrbit->addChild(setLineColorBlending(colorBase,
                    mySolNode.numVerticesEachPeriod[l-1],stability, type, scaler));
        }
//    }else
//    anOrbit->addChild(setLineAttributesByParameterValue(
//           curComponent, maxComponent, maxComponent/2.0, 0,
//          stability, scaler));
#ifdef R3B
    }else
    anOrbit->addChild(setLineAttributesByParameterValue(
            curComponent, maxComponent, maxComponent/2.0, 0,
            stability, scaler));
#endif

    anOrbit->addChild(myCoords);
    anOrbit->addChild(myLine);

    delete [] vertices;
    delete [] colorBase;

    return anOrbit;
}


//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawAPoint(float x, float y, float z, float size, float scale)
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator * ptSep = new SoSeparator;
    SoTransform * ptTrans = new SoTransform;
    ptTrans->translation.setValue(x, y, z);
    ptSep->addChild(ptTrans);

    SoCube *aPoint = new SoCube;
    aPoint->width = size*scale;
    aPoint->height= size*scale;
    aPoint->depth = size*scale;
    ptSep->addChild(aPoint);
    return ptSep;
}


//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawAnOrbitUsingPoints(int style, int iBranch,  long int l, 
     long int si, float scaler, int stability, int type, bool aniColoring)
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator * anOrbit = new SoSeparator;

    float dis = !options[OPT_NORMALIZE_DATA] ? (max(max(fabs(mySolNode.max[0]-mySolNode.min[0]),
        fabs(mySolNode.max[1]-mySolNode.min[1])),
        fabs(mySolNode.max[2]-mySolNode.min[2]))) : 2.0;

    long numVertices = mySolNode.numVerticesEachPeriod[l-1];
    if(numVertices == 1)
    {
        long int idx = si;
        SoSeparator * ptSep = new SoSeparator;
        SoTransform * aTrans = new SoTransform;

        if(coloringMethod == CL_BRANCH_NUMBER)
            ptSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
        else if(coloringMethod == CL_STABILITY)
            ptSep->addChild(setLineAttributesByStability(stability, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            ptSep->addChild(setLineAttributesByType(stability, type, scaler));
        else if(coloringMethod == CL_LABELS)
        {
            ptSep->addChild(setLineAttributesByParameterValue(
                l-1, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                stability, scaler));
        }
        else if(coloringMethod == CL_COMPONENT)
        {
             ptSep->addChild(setLineAttributesByParameterValue(
                 curComponent, maxComponent, maxComponent/2.0, 0,
                 stability, scaler));
        }
        else 
        {
            SoMaterial * ptMtl = new SoMaterial;
            ptMtl->diffuseColor.setValue(1,0,0);
            ptSep->addChild(ptMtl);
        }

        aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], 
		           mySolNode.xyzCoords[idx][1], mySolNode.xyzCoords[idx][2]);
        ptSep->addChild(aTrans);

        SoSphere *aPoint = new SoSphere;
        aPoint->radius = dis * STATIONARY_POINT_RADIUS;
        ptSep->addChild(aPoint);
        anOrbit->addChild(ptSep);
        return anOrbit;
    }

    int32_t  myint[10];
    float (*vertices)[3] = new float[numVertices][3];
    float *colorBase = new float[numVertices];

    for(int m=0; m<numVertices; m++)
    {
        long int idx = si+m;
        vertices[m][0]=mySolNode.xyzCoords[idx][0];
        vertices[m][1]=mySolNode.xyzCoords[idx][1];
        vertices[m][2]=mySolNode.xyzCoords[idx][2];
        if(coloringMethod>=0)colorBase[m]  = clientData.solData[idx][coloringMethod];
        if(coloringMethod==CL_POINT_NUMBER)colorBase[m]  = m;

//        if(maxComponent == 1)
//        {
#ifdef R3B
        if(maxComponent == 1)
        {
#endif
            if(!aniColoring)
            {
                anOrbit->addChild(setLineAttributesByType(stability, 0, scaler));
            }
            else
            {
                if(coloringMethod == CL_BRANCH_NUMBER)
                    anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
                else if(coloringMethod == CL_STABILITY)
                    anOrbit->addChild(setLineAttributesByStability(stability, scaler));
                else if(coloringMethod == CL_ORBIT_TYPE)
                    anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
                else if(coloringMethod == CL_LABELS)
                {
#ifdef R3B
                    double bMin = 0;
                    for(int ib = 0; ib< iBranch; ++ib)
                        bMin +=  mySolNode.numOrbitsInEachBranch[ib];
                    double bMax = bMin+mySolNode.numOrbitsInEachBranch[iBranch]-1;
                    anOrbit->addChild(setLineAttributesByParameterValue(
                        l-1, bMax, (bMax+bMin)/2.0, bMin, stability, scaler));
#else
                    anOrbit->addChild(setLineAttributesByParameterValue(
                         l-1, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                         stability, scaler));
#endif
                }
                else if(coloringMethod == CL_COMPONENT)
                    anOrbit->addChild(setLineAttributesByParameterValue(
                        curComponent, maxComponent, maxComponent/2.0, 0,
                        stability, scaler));
                else if(coloringMethod >= mySolNode.nar)
                {
                    anOrbit->addChild(setLineAttributesByParameterValue(
                        mySolNode.par[l-1][mySolNode.parID[coloringMethod-mySolNode.nar]],
                        mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                        mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                        mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                        stability, scaler));
                }
                else
                    anOrbit->addChild(setLineColorBlending(colorBase,
                        mySolNode.numVerticesEachPeriod[l-1],stability, type, scaler));
            }
//        }else
//        anOrbit->addChild(setLineAttributesByParameterValue(
//                curComponent, maxComponent, maxComponent/2.0, 0,
//                stability, scaler));
#ifdef R3B
        }else
        anOrbit->addChild(setLineAttributesByParameterValue(
                curComponent, maxComponent, maxComponent/2.0, 0,
                stability, scaler));
#endif

        if(style == MESH_POINTS)
        {
            if(m%mySolNode.ncol[l-1] == 0)
                anOrbit->addChild(drawAPoint(mySolNode.xyzCoords[idx][0], mySolNode.xyzCoords[idx][1],
#ifndef R3B
                    mySolNode.xyzCoords[idx][2], dis, STATIONARY_POINT_RADIUS*0.5));
#else
                    mySolNode.xyzCoords[idx][2], dis, STATIONARY_POINT_RADIUS*0.25));
#endif
        }else
        anOrbit->addChild(drawAPoint(mySolNode.xyzCoords[idx][0], mySolNode.xyzCoords[idx][1],
#ifndef R3B
                mySolNode.xyzCoords[idx][2], dis, STATIONARY_POINT_RADIUS*0.5));
#else
                mySolNode.xyzCoords[idx][2], dis, STATIONARY_POINT_RADIUS*0.25));
#endif
    }


    delete [] vertices;
    delete [] colorBase;

    return anOrbit;
}


//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawAnOrbitUsingNurbsCurve(int iBranch, long int l, long int si, float scaler, int stability, int type)
//
//////////////////////////////////////////////////////////////////////////
{
    int32_t  myint[10];
    SoSeparator * anOrbit = new SoSeparator;
    float (*vertices)[3];
    vertices = new float[mySolNode.numVerticesEachPeriod[l-1]][3];
    for(int m=0; m<mySolNode.numVerticesEachPeriod[l-1]; m++)
    {
        vertices[m][0]=mySolNode.xyzCoords[si+m][0];
        vertices[m][1]=mySolNode.xyzCoords[si+m][1];
        vertices[m][2]=mySolNode.xyzCoords[si+m][2];
    }
    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, mySolNode.numVerticesEachPeriod[l-1], vertices);
    myint[0]=mySolNode.numVerticesEachPeriod[l-1];

    int number = mySolNode.numVerticesEachPeriod[l-1];
    float * knots = new float[number+4];
    for (int i=0; i<4; ++i) knots[i]=0, knots[i+number]=number-3;
    for(int i=4; i<number; ++i) knots[i]=i-3;
    SoNurbsCurve *myCurve = new SoNurbsCurve;
    myCurve->numControlPoints = mySolNode.numVerticesEachPeriod[l-1];
    myCurve->knotVector.setValues(0, number+4, knots);

    if(coloringMethod == CL_BRANCH_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch,stability,scaler));
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_LABELS)
    {
        anOrbit->addChild(setLineAttributesByParameterValue(
           l-1, clientData.totalLabels, clientData.totalLabels/2.0, 0,
           stability, scaler));
    }
    else
        anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
    anOrbit->addChild(myCoords);
    anOrbit->addChild(myCurve);
    delete [] vertices;
    delete [] knots;
    return anOrbit;
}


//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawAnOrbitUsingTubes(int iBranch, long int l, long int si, float scaler, int stability, int type)
//
//////////////////////////////////////////////////////////////////////////
{
//int32_t  myint[10];
    float dis = !options[OPT_NORMALIZE_DATA] ? (max(max(fabs(mySolNode.max[0]-mySolNode.min[0]),
        fabs(mySolNode.max[1]-mySolNode.min[1])),
        fabs(mySolNode.max[2]-mySolNode.min[2]))) : 2.0 ;
    int32_t  myint[10];

    SoSeparator * anOrbit = new SoSeparator;
    long int sumX = 0;
    long int numVertices = mySolNode.numVerticesEachPeriod[l-1];
    if(numVertices == 1)
    {
        int idx = si;
        SoSeparator * ptSep = new SoSeparator;
        SoSphere * aPoint = new SoSphere;
        aPoint->radius = dis * STATIONARY_POINT_RADIUS;

        if(coloringMethod == CL_BRANCH_NUMBER)
            ptSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
        else if(coloringMethod == CL_STABILITY)
            ptSep->addChild(setLineAttributesByStability(stability, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            ptSep->addChild(setLineAttributesByType(stability, type, scaler));
        else if(coloringMethod == CL_LABELS)
        {
            anOrbit->addChild(setLineAttributesByParameterValue(
                l-1, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                stability, scaler));
        }
        else if(coloringMethod == CL_COMPONENT)
        {
             anOrbit->addChild(setLineAttributesByParameterValue(
                 curComponent, maxComponent, maxComponent/2.0, 0,
                 stability, scaler));
        }
        else 
        {
            SoMaterial * ptMtl = new SoMaterial;
            ptMtl->diffuseColor.setValue(1,0,0);
            ptSep->addChild(ptMtl);
        }

#ifndef R3B
        float ver[2][3];

        if(time_on != TIME_IS_OFF)
        {
            if(time_on == TIME_ON_X)
            {
                ver[0][0] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0 ; //-(mySolNode.max[0]-mySolNode.min[0]) : 0;
                ver[0][1] = mySolNode.xyzCoords[idx][1];
                ver[0][2] = 0;
                ver[1][0] = (mySolNode.max[0] <= 1.0) ? 1.0 : mySolNode.max[0];
                ver[1][1] = mySolNode.xyzCoords[idx][1];
                ver[1][2] = 0;
            }
            else if(time_on == TIME_ON_Y)
            {
                ver[0][0] = mySolNode.xyzCoords[idx][0];
                ver[0][1] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0; //-(mySolNode.max[1]-mySolNode.min[1]) : 0;
                ver[0][2] = 0;
                ver[1][0] = mySolNode.xyzCoords[idx][0]; 
                ver[1][1] = ( mySolNode.max[1] <= 1.0 ) ? 1.0 : mySolNode.max[1];
                ver[1][2] = 0;
            }
            else if(time_on == TIME_ON_Z)
            {
                ver[0][0] = mySolNode.xyzCoords[idx][0];
                ver[0][1] = 0; 
                ver[0][2] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0; //-(mySolNode.max[2]-mySolNode.min[2]) : 0; 
                ver[1][0] = mySolNode.xyzCoords[idx][0];
                ver[1][1] = 0; 
                ver[1][2] = ( mySolNode.max[2] <= 1.0 ) ? 1.0 : mySolNode.max[2]; 
            }
            SoCoordinate3 *myC = new SoCoordinate3;
            myC->point.setValues(0, 2, ver);
            myint[0]=2;
            SoLineSet *myL = new SoLineSet;
            myL->numVertices.setValues(0, 1, myint);
            ptSep->addChild(myC);
            ptSep->addChild(myL);
            anOrbit->addChild(ptSep);
        }
        else
        {
#endif
            SoTransform * aTrans = new SoTransform;
            aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], 
		           mySolNode.xyzCoords[idx][1], mySolNode.xyzCoords[idx][2]);
            ptSep->addChild(aTrans);
#ifndef R3B
            SoSphere *aPoint = new SoSphere;
            aPoint->radius = dis * STATIONARY_POINT_RADIUS;
#endif
            ptSep->addChild(aPoint);
            anOrbit->addChild(ptSep);
#ifndef R3B
        }
/*
        SoTransform * aTrans = new SoTransform;
        aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], 
		           mySolNode.xyzCoords[idx][1], mySolNode.xyzCoords[idx][2]);
        ptSep->addChild(aTrans);
        ptSep->addChild(aPoint);
        anOrbit->addChild(ptSep);
*/
#endif
        return anOrbit;
    }
    else if( numVertices < 1 )
    {
        return anOrbit;
    }

    float (*vertices)[3] = new float[mySolNode.numVerticesEachPeriod[l-1]][3];
    float *colorBase = new float[mySolNode.numVerticesEachPeriod[l-1]*11];
    Tube tube;
    for(int m=0; m<mySolNode.numVerticesEachPeriod[l-1]; m++)
    {
        vertices[m][0]=mySolNode.xyzCoords[si+m][0];
        vertices[m][1]=mySolNode.xyzCoords[si+m][1];
        vertices[m][2]=mySolNode.xyzCoords[si+m][2];
        if(coloringMethod>=0)
            for(int j=0; j<11; ++j)
                colorBase[m*11+j]  = clientData.solData[si+m][coloringMethod];
        if(coloringMethod==CL_POINT_NUMBER)
            for(int j=0; j<11; ++j)
                colorBase[m*11+j]  = m;
    }
    tube = Tube(mySolNode.numVerticesEachPeriod[l-1], vertices, lineWidthScaler*0.005, 10);

#ifdef R3B
    if(maxComponent == 1)
    {
#else
//    if(maxComponent == 1)
//    {
#endif
        if(coloringMethod == CL_BRANCH_NUMBER)
            anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
        else if(coloringMethod == CL_STABILITY)
            anOrbit->addChild(setLineAttributesByStability(stability, scaler));
        else if(coloringMethod == CL_BRANCH_NUMBER)
            anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
        else if(coloringMethod == CL_LABELS)
        {
#ifdef R3B
//          always start from blue to red for different branches. 
            double bMin = 0;
            for(int ib = 0; ib< iBranch; ++ib)
                bMin +=  mySolNode.numOrbitsInEachBranch[ib];
            double bMax = bMin+mySolNode.numOrbitsInEachBranch[iBranch]-1;
            anOrbit->addChild(setLineAttributesByParameterValue(
                l-1, bMax, (bMax+bMin)/2.0, bMin,
                stability, scaler));
#else
//          always set the first label blue, the last red, namely look all
//          branches as one.
            anOrbit->addChild(setLineAttributesByParameterValue(
                l-1, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                stability, scaler));
#endif
        }
        else if(coloringMethod == CL_COMPONENT)
        {
            anOrbit->addChild(setLineAttributesByParameterValue(
                curComponent, maxComponent, maxComponent/2.0, 0,
                stability, scaler));
        }
        else if(coloringMethod >= mySolNode.nar)
            anOrbit->addChild(setLineAttributesByParameterValue(
                    mySolNode.par[l-1][mySolNode.parID[coloringMethod-mySolNode.nar]],
                    mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                    mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                    mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                    stability, scaler));
        else
            anOrbit->addChild(setLineColorBlending(colorBase,
                mySolNode.numVerticesEachPeriod[l-1]*11,stability, type, scaler));
//    }
//    else
//    {
//        anOrbit->addChild(setLineAttributesByParameterValue(
//            curComponent, maxComponent, maxComponent/2.0, 0,
//            stability, scaler));
//    }
#ifdef R3B
    }
    else
    {
        anOrbit->addChild(setLineAttributesByParameterValue(
            curComponent, maxComponent, maxComponent/2.0, 0,
            stability, scaler));
    }
#endif

    anOrbit->addChild(tube.createTube());

    delete [] vertices;
    return anOrbit;
}


///////////////////////////////////////////////////////////////////////////
//
//         animate the solution by using lines. This version use less memory
//         and much faster.
//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawABifBranchUsingLines(int iBranch, long int l, long int si, float scaler, int stability, int type)
//
//////////////////////////////////////////////////////////////////////////
{
    int32_t  myint[10];

    SoSeparator * aBranch = new SoSeparator;
    long int size         = myBifNode.numVerticesEachBranch[l-1];
    float *colorBase      = new float[size];
    float (*vertices)[3]  = new float[size][3];

    long int curSize = 0;

    int lastStab = myBifNode.ptStability[si];
    int curStab  = lastStab;
    long int m   = 0;
    long int idx = si+m;
    do
    {
        lastStab = curStab;
        vertices[curSize][0] = myBifNode.xyzCoords[idx][0];
        vertices[curSize][1] = myBifNode.xyzCoords[idx][1];
        vertices[curSize][2] = myBifNode.xyzCoords[idx][2];
        if(coloringMethod >= 0)
            colorBase[curSize]  = clientData.bifData[idx][coloringMethod];
        else if(coloringMethod == CL_POINT_NUMBER)
            colorBase[curSize]  = m;
        else if(coloringMethod == CL_STABILITY)
            colorBase[curSize]  = myBifNode.ptStability[idx];
        m++;
        idx = si+m;
        if(m < size)
            curStab = myBifNode.ptStability[idx];
        curSize++;
        if( (lastStab == curStab || curSize <= 1) && m != size) continue;

        SoCoordinate3 *myCoords = new SoCoordinate3;
        myCoords->point.setValues(0, curSize, vertices);
        myint[0]= curSize;

// define the solution line set
        SoLineSet *myLine= new SoLineSet;
        myLine->numVertices.setValues(0, 1, myint);

        if(coloringMethod == CL_BRANCH_NUMBER)
#ifndef R3B
            aBranch->addChild(setLineAttributesByBranch(iBranch, lastStab, scaler));
#else
            aBranch->addChild(setLineAttributesByBranch(myBifNode.branchID[iBranch], lastStab, scaler));
#endif
        else if(coloringMethod == CL_STABILITY)
            aBranch->addChild(setLineColorBlendingByStability(colorBase, curSize, lastStab, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            aBranch->addChild(setLineAttributesByType(lastStab, type, scaler));
        else
            aBranch->addChild(setLineColorBlending(colorBase, curSize,
                lastStab, type, scaler));

        aBranch->addChild(myCoords);
        aBranch->addChild(myLine);

        curSize = 0;
        vertices[curSize][0] = myBifNode.xyzCoords[idx-1][0];
        vertices[curSize][1] = myBifNode.xyzCoords[idx-1][1];
        vertices[curSize][2] = myBifNode.xyzCoords[idx-1][2];
        if(coloringMethod >= 0)
            colorBase[curSize]  = clientData.bifData[idx-1][coloringMethod];
        else if(coloringMethod == CL_POINT_NUMBER)
            colorBase[curSize]  = m-1;
        else if(coloringMethod == CL_STABILITY)
            colorBase[curSize]  = myBifNode.ptStability[idx-1];
        curSize++;
    }while( m < size);

    delete [] vertices;
    delete [] colorBase;
    return aBranch;
}


//////////////////////////////////////////////////////////////////////////
//          Draw a bif branch using NURBS curve
//
SoSeparator *
drawABifBranchUsingNurbsCurve(int iBranch, long int l,
long int si, float scaler, int stability, int type)
//
//////////////////////////////////////////////////////////////////////////
{
    int32_t  myint[10];

    SoSeparator * aBranch = new SoSeparator;
    long int size = myBifNode.numVerticesEachBranch[l-1];
    float (*vertices)[3] = new float[size][3];
    float *colorBase = new float[size];
    for(long int m=0; m<size; ++m) 
    {
        long idx = si+m;
        vertices[m][0]=myBifNode.xyzCoords[si+m][0];
        vertices[m][1]=myBifNode.xyzCoords[si+m][1];
        vertices[m][2]=myBifNode.xyzCoords[si+m][2];
        if(coloringMethod>=0)
            colorBase[m]  =clientData.bifData[idx][coloringMethod];
        if(coloringMethod==CL_POINT_NUMBER)
            colorBase[m]  = m;
    }
    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, size, vertices);
    myint[0]= size;

    SoGroup *cvGrp = new SoGroup;

    float * knots = new float[size+4];
    if(size> 4)
    {
        for (int i=0; i<4; ++i) knots[i]=0, knots[i+size]=size-3;
        for(int i=4; i<size; ++i) knots[i]=i-3;
        SoNurbsCurve *myCurve = new SoNurbsCurve;
        myCurve->numControlPoints = size;
        myCurve->knotVector.setValues(0, size+4, knots);
        cvGrp->addChild(myCurve);
    }
    else
    {
        SoLineSet *myLine= new SoLineSet;
        myLine->numVertices.setValues(0,1,myint);
        cvGrp->addChild(myLine);
    }

    if(coloringMethod == CL_BRANCH_NUMBER)
        aBranch->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
    else if(coloringMethod == CL_STABILITY)
        aBranch->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        aBranch->addChild(setLineAttributesByType(stability, type, scaler));
    else
        aBranch->addChild(setLineColorBlending(colorBase, size,
            stability, type, scaler));
    aBranch->addChild(myCoords);
    aBranch->addChild(cvGrp);

    delete [] vertices;
    delete [] colorBase;
    delete [] knots;
    return aBranch;
}


//////////////////////////////////////////////////////////////////////////
//
//          Draw a label interval
//
SoSeparator *
drawABifLabelInterval(long int l, long int si, float scaler, int stability, int type)
//
//////////////////////////////////////////////////////////////////////////
{
    int32_t  myint[10];

    SoSeparator * anInterval = new SoSeparator;
    long int size = myBifNode.numVerticesEachLabelInterval[l-1];
    float *colorBase = new float[size];
    float (*vertices)[3] = new float[size][3];
    for(int m=0; m<size; m++)
    {
        long int idx = si+m;
        vertices[m][0]=myBifNode.xyzCoords[idx][0];
        vertices[m][1]=myBifNode.xyzCoords[idx][1];
        vertices[m][2]=myBifNode.xyzCoords[idx][2];
        if(coloringMethod>=0)
            colorBase[m]  =clientData.bifData[idx][coloringMethod];
        if(coloringMethod==CL_POINT_NUMBER)
            colorBase[m]  = m;
    }
    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, size, vertices);
    myint[0]=size;

    SoLineSet *myLine= new SoLineSet;
    myLine->numVertices.setValues(0,1,myint);

    if(coloringMethod == CL_STABILITY)
        anInterval->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        anInterval->addChild(setLineAttributesByType(stability, type, scaler));
    else
        anInterval->addChild(setLineColorBlending(colorBase, size,
            stability, type, scaler));

    anInterval->addChild(myCoords);
    anInterval->addChild(myLine);
    delete [] vertices;
    return anInterval;
}


//////////////////////////////////////////////////////////////////////////
//
//          Draw a label interval using NURBS curve
//
SoSeparator *
drawABifLabelIntervalUsingNurbsCurve(long int l,
      long int si, float scaler, int stability, int type)
//
//////////////////////////////////////////////////////////////////////////
{
    int32_t  myint[10];

    SoSeparator * anInterval = new SoSeparator;
    float (*vertices)[3];
    vertices = new float[myBifNode.numVerticesEachLabelInterval[l-1]][3];
    for(int m=0; m<myBifNode.numVerticesEachLabelInterval[l-1]; m++)
    {
        vertices[m][0]=myBifNode.xyzCoords[si+m][0];
        vertices[m][1]=myBifNode.xyzCoords[si+m][1];
        vertices[m][2]=myBifNode.xyzCoords[si+m][2];
    }
    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, myBifNode.numVerticesEachLabelInterval[l-1], vertices);
    myint[0]=myBifNode.numVerticesEachLabelInterval[l-1];

    SoGroup *cvGrp = new SoGroup;

    int number = myBifNode.numVerticesEachLabelInterval[l-1];
    float * knots = new float[number+4];
    if(number > 4)
    {
        for (int i=0; i<4; ++i) knots[i]=0, knots[i+number]=number-3;
        for(int i=4; i<number; ++i) knots[i]=i-3;
        SoNurbsCurve *myCurve = new SoNurbsCurve;
        myCurve->numControlPoints = number;
        myCurve->knotVector.setValues(0, number+4, knots);
        cvGrp->addChild(myCurve);
    }
    else
    {
        SoLineSet *myLine= new SoLineSet;
        myLine->numVertices.setValues(0,1,myint);
        cvGrp->addChild(myLine);
    }

    anInterval->addChild(setLineAttributesByType(stability, type, scaler));
    anInterval->addChild(myCoords);
    anInterval->addChild(cvGrp);

    delete [] vertices;
    delete [] knots;
    return anInterval;
}


#ifdef R3B
///////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawEarthMovement(int k)
//
///////////////////////////////////////////////////////////////////////////
{
    SoSeparator * eSep = new SoSeparator;
    int32_t  myint[10];

    SoDrawStyle *drawStyle = new SoDrawStyle;
    drawStyle->style = SoDrawStyle::FILLED;
    drawStyle->lineWidth = 3.0;
    eSep->addChild(drawStyle);

    SoMaterial *eMtl= new SoMaterial;

    eMtl->diffuseColor.setValue(0.0,0.5,0.5);
    eMtl->transparency = 0.0;
    eSep->addChild(eMtl);

    float vertices[30000][3];
    for(int i=0; i<mySolNode.numVerticesEachPeriod[k]; ++i)
    {
        vertices[i][0]= cos(mySolNode.time[i]/2.0/M_PI*360.0);
        vertices[i][1]= sin(mySolNode.time[i]*360.0/2.0/M_PI);
        vertices[i][2]= 0;
    }
    myint[0]=mySolNode.numVerticesEachPeriod[k];
    SoCoordinate3 * eCoords = new SoCoordinate3;
    eCoords->point.setValues(0, mySolNode.numVerticesEachPeriod[k], vertices);
    SoLineSet *eLine= new SoLineSet;
    eLine->numVertices.setValues(0, 1, myint);
    eSep->addChild(eCoords);
    eSep->addChild(eLine);
    return eSep;
}
#endif


///////////////////////////////////////////////////////////////////////////
//
// animate solution by using tubes. 
// animate solution by using tubes. This version use much memory and
// much slower.
//
SoSeparator *
animateSolutionUsingTubes(bool aniColoring)
//
///////////////////////////////////////////////////////////////////////////
{
    long int sumX = 0;
    SoSeparator *solGroup = new SoSeparator;

    float dis = !options[OPT_NORMALIZE_DATA] ? (max(max(fabs(mySolNode.max[0]-mySolNode.min[0]),
        fabs(mySolNode.max[1]-mySolNode.min[1])),
        fabs(mySolNode.max[2]-mySolNode.min[2]))) : 2.0;

    SoBlinker *tubeBlker = new SoBlinker;
    tubeBlker->speed = orbitSpeed;
    int iBranch = 0;
    int curBranchID = mySolNode.branchID[iBranch];
    int sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];
    for(int j=0; j<mySolNode.numOrbits; j++)
    {
        long int upperlimit = mySolNode.numVerticesEachPeriod[j];
        if(upperlimit == 1)
        {
            int idx = sumX;
            SoSeparator * ptSep = new SoSeparator;
            SoSphere * aPoint = new SoSphere;
            aPoint->radius = dis * STATIONARY_POINT_RADIUS;

            SoTransform * aTrans = new SoTransform;
            aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], \
                                         mySolNode.xyzCoords[idx][1], \
                                         mySolNode.xyzCoords[idx][2]);
            ptSep->addChild(aTrans);
            ptSep->addChild(aPoint);
            solGroup->addChild(ptSep);
        }
        else
        {
            float (*path)[3] = new float[upperlimit][3];
            float *colorBase = new float[upperlimit*11];
#ifdef R3B
            int stability = clientData.labelIndex[j+1][3];
            int type = clientData.labelIndex[j+1][2];
#else
            int stability=clientData.labelIndex[j][3];
            int type =clientData.labelIndex[j][2];
#endif
            SoSeparator *anOrbit = new SoSeparator;
            Tube tube;
            if(j >= sumOrbit)
            {
                curBranchID = mySolNode.branchID[++iBranch];
                sumOrbit   += mySolNode.numOrbitsInEachBranch[iBranch];
            }

            for(int i=0; i<upperlimit; i++)
            {
                int idx = i+sumX;
                path[i][0]=mySolNode.xyzCoords[idx][0];
                path[i][1]=mySolNode.xyzCoords[idx][1];
                path[i][2]=mySolNode.xyzCoords[idx][2];
                if(coloringMethod>=0)
                    for(int j=0; j<11; ++j)
                        colorBase[i*11+j]  = clientData.solData[idx][coloringMethod];
                if(coloringMethod==CL_POINT_NUMBER)
                    for(int j=0; j<11; ++j)
                        colorBase[i*11+j]  = i;
            }

            if(!aniColoring)
                anOrbit->addChild(setLineAttributesByType(stability, 0, lineWidthScaler));
            else if(coloringMethod == CL_STABILITY)
                anOrbit->addChild(setLineAttributesByStability(stability, lineWidthScaler));
            else if(coloringMethod == CL_BRANCH_NUMBER)
                anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, lineWidthScaler));
            else if(coloringMethod == CL_ORBIT_TYPE)
                anOrbit->addChild(setLineAttributesByType(stability, type, lineWidthScaler));
            else if(coloringMethod == CL_LABELS)
            {
#ifdef R3B
                double bMin = 0;
                for(int ib = 0; ib< iBranch; ++ib)
                    bMin +=  mySolNode.numOrbitsInEachBranch[ib];
                double bMax = bMin+mySolNode.numOrbitsInEachBranch[iBranch]-1;
                anOrbit->addChild(setLineAttributesByParameterValue(
                    j, bMax, (bMax+bMin)/2.0, bMin,
                    stability, lineWidthScaler));
#else
                anOrbit->addChild(setLineAttributesByParameterValue(
                    j, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                    stability, lineWidthScaler));
#endif
            }
            else if(coloringMethod >= mySolNode.nar)
                anOrbit->addChild(setLineAttributesByParameterValue(
                        mySolNode.par[j][mySolNode.parID[coloringMethod-mySolNode.nar]],
                        mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                        mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                        mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                        stability, lineWidthScaler));
            else if(coloringMethod == CL_COMPONENT)
            {
                 anOrbit->addChild(setLineAttributesByParameterValue(
                     curComponent, maxComponent, maxComponent/2.0, 0,
                     stability, lineWidthScaler));
            }
            else
                anOrbit->addChild(setLineColorBlending(colorBase,
                    upperlimit*11,stability, type, lineWidthScaler));

            tube = Tube(upperlimit, path, lineWidthScaler*0.0075, 10);
            anOrbit->addChild(tube.createTube());
            tubeBlker->addChild(anOrbit);  
            delete [] path;
            delete [] colorBase;
        }
        sumX += upperlimit;
    }
    solGroup->addChild(tubeBlker);
    return solGroup;
}


///////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawATube(TubeNode cnode)
//
///////////////////////////////////////////////////////////////////////////
{
    SoSeparator *tSep = new SoSeparator;
    SoTransform *tXform = new SoTransform;
    tXform->translation.setValue(cnode.translation.x,\
        cnode.translation.y,\
        cnode.translation.z);
    tXform->rotation.setValue(SbVec3f(cnode.axis.x,\
        cnode.axis.y,\
        cnode.axis.z),\
        cnode.angle);
    SoCylinder *tCyl = new SoCylinder;
    tCyl->radius = 0.005;
    tCyl->height = cnode.height;
    tCyl->parts = SoCylinder::SIDES;

    tSep->addChild(tXform);
    tSep->addChild(tCyl);
    return tSep;
}


#ifdef R3B
///////////////////////////////////////////////////////////////////////////
//
//   This routine animate the calculation steps, namely the density of the
//   collocation points.
//
SoSeparator *
animateOrbitCalSteps(long int snOrbit, long int si)
//
///////////////////////////////////////////////////////////////////////////
{
    float ptb[3], pts[4][3];
    SoSeparator *satGroup = new SoSeparator;
    SoMaterial *satMtl = new SoMaterial;
    satMtl->diffuseColor.setValue(envColors[4]);
    satMtl->transparency = 0.0;

    SoDrawStyle *satStyle = new SoDrawStyle;
    satStyle->style = SoDrawStyle::FILLED;
    satGroup->addChild(satStyle);

    SoSeparator * satSep = new SoSeparator;
    SoBlinker *satBlker = new SoBlinker;

    int upperlimit = mySolNode.numVerticesEachPeriod[snOrbit-1];
    int idx = si;
    satBlker->speed = satSpeed;

    float maxV[3], minV[3];
    maxV[0]=minV[0]=mySolNode.xyzCoords[idx+0][0];
    maxV[1]=minV[1]=mySolNode.xyzCoords[idx+0][0];
    maxV[2]=minV[2]=mySolNode.xyzCoords[idx+0][0];

    for(int i=1; i<upperlimit; i++)
    {
        if(maxV[0]<mySolNode.xyzCoords[idx+i][0]) maxV[0]=mySolNode.xyzCoords[idx+i][0] ;
        if(minV[0]>mySolNode.xyzCoords[idx+i][0]) minV[0]=mySolNode.xyzCoords[idx+i][0] ;
        if(maxV[1]<mySolNode.xyzCoords[idx+i][1]) maxV[1]=mySolNode.xyzCoords[idx+i][1] ;
        if(minV[1]>mySolNode.xyzCoords[idx+i][1]) minV[1]=mySolNode.xyzCoords[idx+i][1] ;
        if(maxV[2]<mySolNode.xyzCoords[idx+i][2]) maxV[2]=mySolNode.xyzCoords[idx+i][2] ;
        if(minV[2]>mySolNode.xyzCoords[idx+i][2]) minV[2]=mySolNode.xyzCoords[idx+i][2] ;
    }

    float dis = fabs(max(max((maxV[0]-minV[0]), (maxV[1]-minV[1])), (maxV[2]-minV[2])));
    for(int i=0; i<upperlimit; i++)
    {
        ptb[0]=mySolNode.xyzCoords[idx+i][0];
        ptb[1]=mySolNode.xyzCoords[idx+i][1];
        ptb[2]=mySolNode.xyzCoords[idx+i][2];
        satBlker->addChild(drawASphere(ptb,1.*dis/100.0));
    }
    satGroup->addChild(satMtl);
    satGroup->addChild(satBlker);

    return satGroup;
}


///////////////////////////////////////////////////////////////////////////
//   Using a red ball to simulate the movement of a sattelite and using
//   white lines to simulate the trace of the sattelite.
//
//   !!!!!!!!!!!!!!!!!!!THIS VERSION DOES NOT WORK YET!!!!!!!!!!!!!
//
//
SoSeparator *
animateOrbitWithNurbsCurveTail(long int j, long int si)
//
///////////////////////////////////////////////////////////////////////////
{
    float ptb[3], pts[4][3];
    SoSeparator *satGroup = new SoSeparator;
    SoMaterial *tailMtl = new SoMaterial;
    tailMtl->diffuseColor.setValue(1.0,1.0,1.0);
    tailMtl->transparency = 0.0;

    SoDrawStyle *satStyle = new SoDrawStyle;
    satStyle->style = SoDrawStyle::FILLED;
    satGroup->addChild(satStyle);

    SoSeparator * satSep = new SoSeparator;

    int upperlimit = mySolNode.numVerticesEachPeriod[j-1];
    int idx = si;

    float maxV[3], minV[3];
    double *time = new double[upperlimit];
    double dt = 1.0/upperlimit;
    maxV[0]=minV[0]=mySolNode.xyzCoords[idx+0][0];
    maxV[1]=minV[1]=mySolNode.xyzCoords[idx+0][0];
    maxV[2]=minV[2]=mySolNode.xyzCoords[idx+0][0];

    time[0] = 0.0;
    for(int i=1; i<upperlimit; i++)
    {
        if(maxV[0]<mySolNode.xyzCoords[idx+i][0]) maxV[0]=mySolNode.xyzCoords[idx+i][0] ;
        if(minV[0]>mySolNode.xyzCoords[idx+i][0]) minV[0]=mySolNode.xyzCoords[idx+i][0] ;
        if(maxV[1]<mySolNode.xyzCoords[idx+i][1]) maxV[1]=mySolNode.xyzCoords[idx+i][1] ;
        if(minV[1]>mySolNode.xyzCoords[idx+i][1]) minV[1]=mySolNode.xyzCoords[idx+i][1] ;
        if(maxV[2]<mySolNode.xyzCoords[idx+i][2]) maxV[2]=mySolNode.xyzCoords[idx+i][2] ;
        if(minV[2]>mySolNode.xyzCoords[idx+i][2]) minV[2]=mySolNode.xyzCoords[idx+i][2] ;
        time[i] = i*dt;
    }

    float dis = fabs(max(max((maxV[0]-minV[0]), (maxV[1]-minV[1])), (maxV[2]-minV[2])));

    float (*myVertices)[3]= new float[upperlimit][3];

    myVertices[0][0]=mySolNode.xyzCoords[idx][0];
    myVertices[0][1]=mySolNode.xyzCoords[idx][1];
    myVertices[0][2]=mySolNode.xyzCoords[idx][2];
    for(int i=1; i<upperlimit; i++)
    {
        int m = 1;
        while(time[i] > mySolNode.time[idx+m] && m < upperlimit) ++m;

        if( fabs(time[i]-mySolNode.time[m]) <= 1.0e-9 )
        {
            myVertices[i][0]=mySolNode.xyzCoords[idx+m][0];
            myVertices[i][1]=mySolNode.xyzCoords[idx+m][1];
            myVertices[i][2]=mySolNode.xyzCoords[idx+m][2];
        }
        else
        {
            myVertices[i][0]= (mySolNode.xyzCoords[idx+m][0]+mySolNode.xyzCoords[idx+m-1][0])/2.0;
            myVertices[i][1]= (mySolNode.xyzCoords[idx+m][1]+mySolNode.xyzCoords[idx+m-1][1])/2.0;
            myVertices[i][2]= (mySolNode.xyzCoords[idx+m][2]+mySolNode.xyzCoords[idx+m-1][2])/2.0;
        }
    }

    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, upperlimit, myVertices);
    satGroup->addChild(myCoords);

    SoTimeCounter *myCounter = new SoTimeCounter;
    myCounter->max = upperlimit-1;
    myCounter->min = 4;
    myCounter->frequency = 0.1*satSpeed;

    SoLineSet *myLine= new SoLineSet;
    myLine->numVertices.connectFrom(&myCounter->output);
    satGroup->addChild(tailMtl);
    satGroup->addChild(myLine);

    SoMaterial * satMtl = new SoMaterial;
    SoSphere * mySat = new SoSphere;
    mySat->radius = dis*0.005*satRadius;

    SoTranslation * satTrans = new SoTranslation;
    satMtl->diffuseColor.setValue(envColors[4]);
    satGroup->addChild(satMtl);
    satGroup->addChild(satTrans);
    satGroup->addChild(mySat);

    SoSelectOne *mysel = new SoSelectOne(SoMFVec3f::getClassTypeId());
    mysel->index.connectFrom(&myCounter->output);
    mysel->input->enableConnection(TRUE);
    mysel->input->connectFrom(&myCoords->point);
    satTrans->translation.connectFrom(mysel->output);

    delete [] myVertices;
    delete [] time;

    return satGroup;
}
#endif


///////////////////////////////////////////////////////////////////////////
//   Using a red ball to simulate the movement of a sattelite and using
//   white lines to simulate the trace of the sattelite.
//
SoSeparator *
animateOrbitWithTail(int iBranch, long int j, long int si)
//
///////////////////////////////////////////////////////////////////////////
{
#ifndef R3B
   
    SoSeparator *aniSep = new SoSeparator;
#endif
    SoSeparator *satGroup = new SoSeparator;

    float distance = !options[OPT_NORMALIZE_DATA] ? (max(max(fabs(mySolNode.max[0]-mySolNode.min[0]),
        fabs(mySolNode.max[1]-mySolNode.min[1])),
        fabs(mySolNode.max[2]-mySolNode.min[2]))) : 2.0;
#ifdef R3B
    int stability = clientData.labelIndex[j][3];
    int type = clientData.labelIndex[j][2];
#else
    int stability = clientData.labelIndex[j-1][3];
    int type = clientData.labelIndex[j-1][2];
#endif

    long int upperlimit = mySolNode.numVerticesEachPeriod[j-1];
    long int idx = si; 
    if(upperlimit == 1)
    {
        SoSeparator * ptSep = new SoSeparator;
        SoTransform * aTrans = new SoTransform;
        aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], 
                                     mySolNode.xyzCoords[idx][1], 
                                     mySolNode.xyzCoords[idx][2]);
        ptSep->addChild(aTrans);

        SoSphere * aPoint = new SoSphere;
        aPoint->radius = distance * STATIONARY_POINT_RADIUS;
        ptSep->addChild(aPoint);
        satGroup->addChild(ptSep);
        return satGroup;
    }

    float maxV[3], minV[3];
    long int orbitSize =  upperlimit;
#ifndef R3B
    long int arrSize = orbitSize;
#else
    long int arrSize = (numPeriodAnimated==0) ? orbitSize : (long int)ceil(numPeriodAnimated * orbitSize);
#endif

#ifndef R3B
    double *time = new double[upperlimit+1];
#else
    double *time = new double[upperlimit];
#endif
    double dt = 1.0/upperlimit;

    maxV[0]=minV[0]=mySolNode.xyzCoords[idx+0][0];
    maxV[1]=minV[1]=mySolNode.xyzCoords[idx+0][0];
    maxV[2]=minV[2]=mySolNode.xyzCoords[idx+0][0];

    time[0] = 0.0;
    for(long int i=1; i<upperlimit; i++)
    {
        if(maxV[0]<mySolNode.xyzCoords[idx+i][0]) maxV[0]=mySolNode.xyzCoords[idx+i][0] ;
        if(minV[0]>mySolNode.xyzCoords[idx+i][0]) minV[0]=mySolNode.xyzCoords[idx+i][0] ;
        if(maxV[1]<mySolNode.xyzCoords[idx+i][1]) maxV[1]=mySolNode.xyzCoords[idx+i][1] ;
        if(minV[1]>mySolNode.xyzCoords[idx+i][1]) minV[1]=mySolNode.xyzCoords[idx+i][1] ;
        if(maxV[2]<mySolNode.xyzCoords[idx+i][2]) maxV[2]=mySolNode.xyzCoords[idx+i][2] ;
        if(minV[2]>mySolNode.xyzCoords[idx+i][2]) minV[2]=mySolNode.xyzCoords[idx+i][2] ;
        time[i] = i*dt;
    }

#ifndef R3B
    float dis = distance;// fabs(max(max((maxV[0]-minV[0]), (maxV[1]-minV[1])), (maxV[2]-minV[2])));
    float (*myVertices)[3]= new float[arrSize+1][3];
    float *myColorBase = new float [arrSize+1];
#else
    float dis = fabs(max(max((maxV[0]-minV[0]), (maxV[1]-minV[1])), (maxV[2]-minV[2])));
    float (*myVertices)[3]= new float[arrSize][3];
    float *myColorBase = new float [arrSize];
#endif

#ifndef R3B
    myVertices[0][0] = myVertices[arrSize][0] = mySolNode.xyzCoords[idx][0];
    myVertices[0][1] = myVertices[arrSize][1] = mySolNode.xyzCoords[idx][1];
    myVertices[0][2] = myVertices[arrSize][2] = mySolNode.xyzCoords[idx][2];
#else
// animate the orbit in the proximately correct speed.
    myVertices[0][0]=mySolNode.xyzCoords[idx][0];
    myVertices[0][1]=mySolNode.xyzCoords[idx][1];
    myVertices[0][2]=mySolNode.xyzCoords[idx][2];
#endif
    if(coloringMethod>=0)myColorBase[0]  = clientData.solData[idx][coloringMethod];
    if(coloringMethod==CL_POINT_NUMBER)myColorBase[0]  = 0;
    for(long int i=1; i<upperlimit; i++)
    {
        double tTemp = time[i];
        long int m = 0;
        while(tTemp > mySolNode.time[idx+m] && m < upperlimit) ++m;

        if( fabs(tTemp-mySolNode.time[idx+m]) <= 1.0e-9 ||
            fabs(mySolNode.time[idx+m]-mySolNode.time[idx+m-1])<=1.0e-8)
        {
            myVertices[i][0] = mySolNode.xyzCoords[idx+m][0];
            myVertices[i][1] = mySolNode.xyzCoords[idx+m][1];
            myVertices[i][2] = mySolNode.xyzCoords[idx+m][2];
        }
        else
        {
            myVertices[i][0] = (mySolNode.xyzCoords[idx+m][0] +
                                mySolNode.xyzCoords[idx+m-1][0])*0.5;
            myVertices[i][1] = (mySolNode.xyzCoords[idx+m][1] +
                                mySolNode.xyzCoords[idx+m-1][1])*0.5;
            myVertices[i][2] = (mySolNode.xyzCoords[idx+m][2] +
                                mySolNode.xyzCoords[idx+m-1][2])*0.5;
        }

        if(coloringMethod>=0)myColorBase[i]  = clientData.solData[idx+m][coloringMethod];
        if(coloringMethod==CL_POINT_NUMBER)myColorBase[i]  = i;
    }

#ifdef R3B
    if(numPeriodAnimated >1)
    {
        for(long int i=upperlimit; i<arrSize; i++)
        {
            myVertices[i][0]=myVertices[i%upperlimit][0];
            myVertices[i][1]=myVertices[i%upperlimit][1];
            myVertices[i][2]=myVertices[i%upperlimit][2];
            myColorBase[i]  =myColorBase[i%upperlimit];
        }
    }

#endif
    SoDrawStyle *satStyle = new SoDrawStyle;
    satStyle->style = SoDrawStyle::FILLED;
    satGroup->addChild(satStyle);

    SoCoordinate3 *myCoords = new SoCoordinate3;
#ifndef R3B
    myCoords->point.setValues(0, arrSize+1, myVertices);
#else
    myCoords->point.setValues(0, arrSize, myVertices);
#endif
    satGroup->addChild(myCoords);

    
    SoTimeCounter *myCounter = new SoTimeCounter;
#ifndef R3B
    myCounter->max = arrSize+1;
#else
    myCounter->max = arrSize-1;
#endif
    myCounter->min = 0;
#ifndef R3B
   
    float freq, fduty; 

    if ( numPeriodAnimated == -1 ) 
    {
         freq =  0.1*satSpeed/128;
         fduty = 127*arrSize;
    }
    else if( numPeriodAnimated == 0 )
    {
         freq = 0;
         fduty = 0;
    }
    else if ( numPeriodAnimated == 1 ){
         freq = 0.1*satSpeed; 
         fduty = 1;
    }
    else {
         freq =  0.1*satSpeed/numPeriodAnimated;
         fduty = (numPeriodAnimated-1)*arrSize;
    }

    myCounter->frequency =  freq ;

    int iduty = 0;
    for(iduty = 0; iduty <= arrSize+1; ++iduty)
         if(iduty == arrSize+1) myCounter->duty.set1Value(iduty, fduty) ;
         else myCounter->duty.set1Value(iduty, 1) ;
#else
    myCounter->frequency = (numPeriodAnimated !=0) ? 0.1*satSpeed/numPeriodAnimated : 0.1*satSpeed;
#endif

//------------------------------------------Begin-----------------------------------------
    float scaler = lineWidthScaler;

//    if(maxComponent == 1)
//    {
#ifdef R3B
    if(maxComponent == 1)
    {
#endif
//------------------------------------------End-----------------------------------------
        if(coloringMethod == CL_BRANCH_NUMBER)
            satGroup->addChild(setLineAttributesByBranch(iBranch, stability, lineWidthScaler));
        else if(coloringMethod == CL_STABILITY)
            satGroup->addChild(setLineAttributesByStability(stability, lineWidthScaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            satGroup->addChild(setLineAttributesByType(stability, type, lineWidthScaler));
        else if(coloringMethod == CL_LABELS)
        {
#ifdef R3B
            double bMin = 0;
            for(int ib = 0; ib< iBranch; ++ib)
                bMin +=  mySolNode.numOrbitsInEachBranch[ib];
            double bMax = bMin+mySolNode.numOrbitsInEachBranch[iBranch]-1;
            satGroup->addChild(setLineAttributesByParameterValue(
                j-1, bMax, (bMax+bMin)/2.0, bMin,
                stability, lineWidthScaler));
#else
            satGroup->addChild(setLineAttributesByParameterValue(
                j-1, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                stability, lineWidthScaler));
#endif
        }
        else if(coloringMethod == CL_COMPONENT)
            satGroup->addChild(setLineAttributesByParameterValue(
                curComponent, maxComponent, maxComponent/2.0, 0,
                stability, scaler));
        else if(coloringMethod >= mySolNode.nar)
        {
            satGroup->addChild(setLineAttributesByParameterValue(
                mySolNode.par[j-1][mySolNode.parID[coloringMethod-mySolNode.nar]],
                mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                stability, lineWidthScaler));
        }
        else
        {
            satGroup->addChild(setLineColorBlending(myColorBase, arrSize,
                stability, type, lineWidthScaler));
        }
//------------------------------------------Begin-----------------------------------------
//    }
//    else
//    {
//        satGroup->addChild(setLineAttributesByParameterValue(
//            curComponent, maxComponent, maxComponent/2.0, 0,
//            stability, scaler));
//    }
#ifdef R3B
    }
    else
    {
        satGroup->addChild(setLineAttributesByParameterValue(
            curComponent, maxComponent, maxComponent/2.0, 0,
            stability, scaler));
    }
#endif
//------------------------------------------End-----------------------------------------

// define the solution line set
    SoLineSet *myLine= new SoLineSet;
    myLine->numVertices.connectFrom(&myCounter->output);
    satGroup->addChild(myLine);

    SoMaterial * satMtl = new SoMaterial;
    SoSphere * mySat = new SoSphere;
    mySat->radius = satRadius*dis*0.005;


    SoTranslation * satTrans = new SoTranslation;
//    satMtl->diffuseColor.setValue(envColors[4]);
#ifdef R3B
    satMtl->diffuseColor.setValue(envColors[4]);
#endif

//    satGroup->addChild(satMtl);
#ifdef R3B
    satGroup->addChild(satMtl);
#endif
    satGroup->addChild(satTrans);
    satGroup->addChild(mySat);

#ifndef R3B
    SoTimeCounter *myCounter2 = new SoTimeCounter;
    myCounter2->max = arrSize;
    myCounter2->min = 0;
    if (numPeriodAnimated != 0)
        myCounter2->frequency =  0.1*satSpeed;//*numPeriodAnimated;
    else
        myCounter2->frequency =  0.1*satSpeed;
    for(iduty = 0; iduty < arrSize; ++iduty)
         myCounter2->duty.set1Value(iduty, 1) ;

#endif
    SoSelectOne *mysel = new SoSelectOne(SoMFVec3f::getClassTypeId());
#ifndef R3B
    mysel->index.connectFrom(&myCounter2->output);
#else
    mysel->index.connectFrom(&myCounter->output);
#endif
    mysel->input->enableConnection(TRUE);
    mysel->input->connectFrom(&myCoords->point);
    satTrans->translation.connectFrom(mysel->output);

    delete [] myVertices;
    delete [] myColorBase;
    delete [] time;
    return satGroup;
}


///////////////////////////////////////////////////////////////////////////
//
//   Using a red sphere to simulate the movement of a sattelite.
//
SoSeparator *
animateOrbitMovement(long int j, long int si)
//
///////////////////////////////////////////////////////////////////////////
{
    float ptb[3], pts[4][3];
    SoSeparator *satGroup = new SoSeparator;

    SoMaterial *satMtl = new SoMaterial;
    satMtl->diffuseColor.setValue(1.0,0.0,0.0);
    satMtl->transparency = 0.0;

    SoDrawStyle *satStyle = new SoDrawStyle;
    satStyle->style = SoDrawStyle::FILLED;
    satGroup->addChild(satStyle);

    SoSeparator * satSep = new SoSeparator;
    SoBlinker *satBlker = new SoBlinker;

    int upperlimit = mySolNode.numVerticesEachPeriod[j-1];
    int idx = si;             
    satBlker->speed = satSpeed;

    float maxV[3], minV[3];
    maxV[0]=minV[0]=mySolNode.xyzCoords[idx+0][0];
    maxV[1]=minV[1]=mySolNode.xyzCoords[idx+0][0];
    maxV[2]=minV[2]=mySolNode.xyzCoords[idx+0][0];
    double *time = new double[upperlimit];
    float dt = 1.0/upperlimit;
    time[0] = 0.0;
    for(int i=1; i<upperlimit; i++)
    {
        if(maxV[0]<mySolNode.xyzCoords[idx+i][0]) maxV[0]=mySolNode.xyzCoords[idx+i][0] ;
        if(minV[0]>mySolNode.xyzCoords[idx+i][0]) minV[0]=mySolNode.xyzCoords[idx+i][0] ;
        if(maxV[1]<mySolNode.xyzCoords[idx+i][1]) maxV[1]=mySolNode.xyzCoords[idx+i][1] ;
        if(minV[1]>mySolNode.xyzCoords[idx+i][1]) minV[1]=mySolNode.xyzCoords[idx+i][1] ;
        if(maxV[2]<mySolNode.xyzCoords[idx+i][2]) maxV[2]=mySolNode.xyzCoords[idx+i][2] ;
        if(minV[2]>mySolNode.xyzCoords[idx+i][2]) minV[2]=mySolNode.xyzCoords[idx+i][2] ;
        time[i] = i*dt;
    }
    float dis = max(max((maxV[0]-minV[0]), (maxV[1]-minV[1])), (maxV[2]-minV[2]));

// animate the orbit
    ptb[0]=mySolNode.xyzCoords[idx][0];
    ptb[1]=mySolNode.xyzCoords[idx][1];
    ptb[2]=mySolNode.xyzCoords[idx][2];
    satBlker->addChild(drawASphere(ptb,1.*dis/100.0));

    for(int i=1; i<upperlimit; i++)
    {
        int m = 1;
        while(time[i] > mySolNode.time[idx+m] && m < upperlimit) ++m;

        if( fabs(time[i]-mySolNode.time[m]) <= 1.0e-9 )
        {
            ptb[0]=mySolNode.xyzCoords[idx+m][0];
            ptb[1]=mySolNode.xyzCoords[idx+m][1];
            ptb[2]=mySolNode.xyzCoords[idx+m][2];
            satBlker->addChild(drawASphere(ptb,1.*dis/100.0));
        }
        else
        {
            ptb[0]= (mySolNode.xyzCoords[idx+m][0]+mySolNode.xyzCoords[idx+m-1][0])/2.0;
            ptb[1]= (mySolNode.xyzCoords[idx+m][1]+mySolNode.xyzCoords[idx+m-1][1])/2.0;
            ptb[2]= (mySolNode.xyzCoords[idx+m][2]+mySolNode.xyzCoords[idx+m-1][2])/2.0;
            satBlker->addChild(drawASphere(ptb,1.*dis/100.0));
        }
    }
    delete [] time;
    satGroup->addChild(satMtl);
    satGroup->addChild(satBlker);
    return satGroup;
}


///////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawASphere(float position[], float size)
//
///////////////////////////////////////////////////////////////////////////
{
    float scaler = 1.0e-4;
    SoSeparator *satSep = new SoSeparator;
    SoTransform *satXform = new SoTransform;

    satXform->translation.setValue(position[0],position[1],position[2]);
    satSep->addChild(satXform);

    SoSeparator *satSph;

    bool obj = FALSE;
    SoInput mySceneInput;
    if (obj && mySceneInput.openFile("widgets/sattelite.iv"))
    {
        satSph = SoDB::readAll(&mySceneInput);
        if (satSph == NULL)
        {
            obj=FALSE;
        }
        else
        {

            mySceneInput.closeFile();
            SoTransform *satTrans = new SoTransform;
            satTrans->scaleFactor.setValue(scaler,scaler,scaler);
            satSep->addChild(satTrans);
            satSep->addChild(satSph);
            satSep->addChild(satSph);
            obj = TRUE;
        }
    }
    else
    {
        obj = FALSE;
    }

    if(!obj)
    {
        SoSphere *satSph = new SoSphere;
        satSph->radius = size;
        satSep->addChild(satSph);
    }

    return satSep;
}


///////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawASphereWithColor(float color[], float position[], float size)
//
///////////////////////////////////////////////////////////////////////////
{
    SoSeparator *satSep = new SoSeparator;
    SoTransform *satXform = new SoTransform;
    satXform->translation.setValue(position[0],position[1],position[2]);

    SoMaterial *satMtl = new SoMaterial;
    satMtl->diffuseColor.setValue(color);
    satMtl->transparency = 0.0;
    satSep->addChild(satMtl);

    SoSphere *satSph = new SoSphere;
    satSph->radius = size;

    satSep->addChild(satXform);
    satSep->addChild(satSph);

    return satSep;
}


////////////////////////////////////////////////////////////////////////
//
//  initialize win basic things
//
bool
readSolutionAndBifurcationData(bool blFirstRead)
//
///////////////////////////////////////////////////////////////////////
{
    long int  total=0, rows=0;

    solHead = parseSolution(sFileName, blOpenSolFile, total, rows);
    if(!blOpenSolFile)
        printf(" Solution file does not exist.\n");

    mySolNode.time = new double[mySolNode.totalNumPoints];
    mySolNode.xyzCoords = new float[mySolNode.totalNumPoints][3];
    mySolNode.numAxis   = 3;

    clientData.solMax = new float [mySolNode.nar+1];
    clientData.solMin = new float [mySolNode.nar+1];

    clientData.solData = new float*[mySolNode.totalNumPoints];
    for(int ml=0; ml<mySolNode.totalNumPoints; ++ml)
        clientData.solData[ml]= new float [mySolNode.nar];

    blOpenBifFile = parseBifurcation(bFileName) ? true : false;
#ifndef R3B
    if(!blOpenBifFile) printf(" Bifurcation file does not exist!\n");
#else
    if(blOpenBifFile)
      printf(" Parse Bifurcation file. OK\n");
    else
      printf(" No bifurcation file found!\n");
#endif

    if((!blOpenBifFile) && (!blOpenSolFile) && (!blFirstRead)) return false;
    else if((!blOpenBifFile) && (!blOpenSolFile))
    {
//        printf(" Target files do not exist!\n");
        exit(1);
    }

    myBifNode.xyzCoords = new float[myBifNode.totalNumPoints][3];
    myBifNode.ptStability = new int[myBifNode.totalNumPoints];

    clientData.bifData = new float*[myBifNode.totalNumPoints];
    for(int ml=0; ml<myBifNode.totalNumPoints; ++ml)
        clientData.bifData[ml]= new float [myBifNode.nar];


    clientData.multipliers = new float[myBifNode.totalNumPoints][6][2];
    clientData.eigenvalues = new bool[myBifNode.totalNumPoints];

    int varIndices[3];
#ifndef R3B
    if( blOpenBifFile)
    {
        bool tmp = false;
        if(!blFirstRead) clientData.totalLabels = 0;
        tmp = readBifurcation(bFileName, varIndices) ? true : false;
        if(!tmp) printf(" Failed to read the bifurcation file!\n");
    }
    else
    {
        whichType = SOLUTION;
    }

#endif
    if( blOpenSolFile )
    {
        bool tmp = false;
        tmp = readSolution(solHead, sFileName, varIndices) ? true : false;
        if(!tmp) printf(" Failed to read the solution file!\n");
        blOpenSolFile = tmp;

        if(mySolNode.nar <= 3)
	{
	    setShow3DSol = false;
	    if(whichType != BIFURCATION) setShow3D = false;
	}
    }
    else
    {
        whichType = BIFURCATION;
    }

#ifndef R3B
    if((!blOpenBifFile) && (!blOpenSolFile))
#else
    if( blOpenBifFile)
#endif
    {
//        printf(" Target files do not exist!\n");
#ifndef R3B
        exit(1);
#else
        bool tmp = false;
        tmp = readBifurcation(bFileName, varIndices) ? true : false;
        if(!tmp) printf(" Failed to read the bifurcation file!\n");
    }
    else
    {
        whichType = SOLUTION;
#endif
    }

    int st = readFM(dFileName, myBifNode.totalNumPoints);
#ifndef R3B
    if(st!=0)
//        printf(" D file OK.\n");
//    else
        printf(" Failed to read the diagnostic file.\n");
#else
    if(st==0)
        printf(" D file OK.\n");
    else
        printf(" Failed to read the D file.\n");

    if(!blOpenSolFile && !blOpenBifFile)
    {
        printf(" Target files do not exist!\n");
        exit(1);
    }
#endif

    return TRUE;
}


////////////////////////////////////////////////////////////////////////
//
//
//
void
copySolDataToWorkArray(int  varIndices[])
//
////////////////////////////////////////////////////////////////////////
{
    double mx, mi;
    for(int k=0; k<3; k++)
    {
        for(long int row=0; row<mySolNode.totalNumPoints; ++row)
        {
            mySolNode.time[row] = clientData.solData[row][0];
            if(varIndices[k]>=0)
            {
                float dummy = clientData.solData[row][varIndices[k]];
                mySolNode.xyzCoords[row][k] = dummy;
#ifdef R3B
                if(dummy>mySolNode.max[k] || row==0 )
                    mySolNode.max[k] = dummy;
                if(dummy<mySolNode.min[k] || row==0 )
                    mySolNode.min[k] = dummy;
#endif
            }
            else if(varIndices[k]<0)
            {
                mySolNode.xyzCoords[row][k]=0.0;
//                mySolNode.max[k]= 1;
//                mySolNode.min[k]=-1;
#ifndef R3B
            }
        }
/*
        mx = mySolNode.max[k];
        mi = mySolNode.min[k];
        rounding(mx, mi);
        mySolNode.max[k] = mx;
        mySolNode.min[k] = mi;
*/
    }
}

////////////////////////////////////////////////////////////////////////
//
//
//
void
searchForMaxMin(int component, int  varIndices[])
//
////////////////////////////////////////////////////////////////////////
{
    double mx, mi;
    for(int k=0; k<3; k++)
    {
        for(long int row=0; row<mySolNode.totalNumPoints; ++row)
        {
            if(varIndices[k]>=0)
            {
                float dummy = clientData.solData[row][varIndices[k]];
                if(dummy>mySolNode.max[k] || (row==0 && component==1))
                    mySolNode.max[k] = dummy;
                if(dummy<mySolNode.min[k] || (row==0 && component==1))
                    mySolNode.min[k] = dummy;
            }
            else if(varIndices[k]<0)
            {
#endif
                mySolNode.max[k]= 1;
                mySolNode.min[k]=-1;
            }
        }
        mx = mySolNode.max[k];
        mi = mySolNode.min[k];
        rounding(mx, mi);
        mySolNode.max[k] = mx;
        mySolNode.min[k] = mi;
    }
}

////////////////////////////////////////////////////////////////////////
//
//
//
void
copyBifDataToWorkArray(int  varIndices[])
//
////////////////////////////////////////////////////////////////////////
{

    double mx, mi;

    for(int k=0; k<3; k++)
    {
        for(long int row=0; row<myBifNode.totalNumPoints; ++row)
        {
            if(varIndices[k]>=0)
            {
                float dummy = clientData.bifData[row][varIndices[k]];

                myBifNode.xyzCoords[row][k] = dummy;
                if(dummy>myBifNode.max[k] || row==0 )
                    myBifNode.max[k] = dummy;
                if(dummy<myBifNode.min[k] || row==0 )
                    myBifNode.min[k] = dummy;
            }
            else if(varIndices[k]<0)
            {
                myBifNode.xyzCoords[row][k]=0.0;
                myBifNode.max[k]= 1;
                myBifNode.min[k]=-1;
            }
        }

        mx = myBifNode.max[k];
        mi =myBifNode.min[k];
        rounding(mx, mi);
        myBifNode.max[k] = mx;
        myBifNode.min[k] = mi;
    }
}



#ifdef R3B
////////////////////////////////////////////////////////////////////////
//
//      Set the initial values for the variables: animationLabel, orbitSpeed
//      satSpeed.
//
void
initPara()
//
////////////////////////////////////////////////////////////////////////
{
    if(solHead != NULL)
    {
        solutionp cur = solHead;
        while(cur)
        {
            cur=cur->next;
            delete solHead;
            solHead = cur;
        }
    }
    solHead = NULL;
    animationLabel = 0;
    orbitSpeed = 1.0;
    satSpeed   = 0.5;
}
#endif



////////////////////////////////////////////////////////////////////////
//
void
lookForThePoint(float position[],long int &bIdx, long int &sIdx)
//
////////////////////////////////////////////////////////////////////////
{

    int varIndices[3];
    int mx = max(max(xCoordIdxSize, yCoordIdxSize), max(yCoordIdxSize, zCoordIdxSize));
    float minDis = 10000;
    long int index = 0;
    long int ib = 0;
    float distance;
    for(int i=0; i<mx; i++)
    {
        varIndices[0]=xCoordIndices[(i>=xCoordIdxSize)?(i%xCoordIdxSize):(i)];
        varIndices[1]=yCoordIndices[(i>=yCoordIdxSize)?(i%yCoordIdxSize):(i)];
        varIndices[2]=zCoordIndices[(i>=zCoordIdxSize)?(i%zCoordIdxSize):(i)];
        animationLabel = myLabels[lblIndices[0]];
        long int lblidx = lblIndices[0];
        if(whichType != BIFURCATION)
        {
            if(animationLabel == 0 || lblIdxSize >1)
            {
                float p1[3];
                for(long int j=0; j<mySolNode.totalNumPoints; ++j)
                {
                    for(int k=0; k<3; ++k)
                        p1[k] = clientData.solData[j][varIndices[k]];
                    distance = 0;
                    for(int k=0; k<3; ++k)
                        distance += (position[k]-p1[k])*(position[k]-p1[k]);
                    if(minDis > distance)
                    {
                        minDis = distance;
                        index  = j;
                    }
                }

                long int sumup = 0;
                ib = 0;
                while (sumup <index && ib<clientData.totalLabels)
                    sumup += mySolNode.numVerticesEachPeriod[ib++];
            }
            else
            {
                long int sumup = 0;
                float p1[3];
                for(int j=1; j<lblidx; ++j)
                {
                    sumup += mySolNode.numVerticesEachPeriod[j-1];
                }
                for(long int j=0; j<mySolNode.numVerticesEachPeriod[lblidx-1]; ++j)
                {
                    for(int k=0; k<3; ++k)
                        p1[k] = clientData.solData[sumup+j][varIndices[k]];
                    distance = 0;
                    for(int k=0; k<3; ++k)
                        distance += (position[k]-p1[k])*(position[k]-p1[k]);
                    if(minDis > distance)
                    {
                        minDis = distance;
                        index  = j+sumup;
                    }
                }
                ib = lblidx;  
            }
            sIdx = index;
#ifdef R3B
            bIdx = clientData.labelIndex[ib][1];
#else
            bIdx = clientData.labelIndex[ib-1][1];
#endif
        }
        else
        {
            {
                float p1[3];
                for(int j=0; j<myBifNode.totalNumPoints; ++j)
                {
                    for(int k=0; k<3; ++k)
                        p1[k] = clientData.bifData[j][varIndices[k]];
                    distance = 0;
                    for(int k=0; k<3; ++k)
                        distance += (position[k]-p1[k])*(position[k]-p1[k]);
                    if(minDis > distance)
                    {
                        minDis = distance;
                        index  = j;
                    }
                }
            }
            bIdx = index;
        }
    }
}


///////////////////////////////////////////////////////////////////////////
//
void
pointsToAxisAngle(float * a, float * b, TubeNode &value)
//
///////////////////////////////////////////////////////////////////////////
{
    float c[3]={0.,0.,0.};
    float normc[3];
    int i=0;
    value.translation.x=value.translation.y=value.translation.z=0.0;
    value.axis.x=value.axis.y=value.axis.z=0.0;
    value.angle=0.0;
    value.height=0.0;
    float scaler = 2.0;
    for(i=0; i<3; i++)
        c[i]=b[i]-a[i];
    if(c[1]<0)
    {
        value.translation.x=b[0];
        value.translation.y=b[1];
        value.translation.z=b[2];
        scaler = 2.0;
        for(i=0; i<3; i++)
            c[i]=-c[i];
    }
    else
    {
        value.translation.x=a[0];
        value.translation.y=a[1];
        value.translation.z=a[2];
    }

    float c_norm, axis_norm;
    c_norm=sqrt(c[0]*c[0]+c[1]*c[1]+c[2]*c[2]);
    value.height=c_norm*scaler; 
    if(c_norm>0.0000001)
    {
        for(i=0; i<3; i++)
            normc[i]=0.0;
        for(i=0; i<3; i++)
            normc[i]=c[i]/c_norm;
        value.axis.x=normc[2];
        value.axis.y=0.0;
        value.axis.z=-normc[0];
        axis_norm=sqrt(value.axis.x*value.axis.x +\
            value.axis.y*value.axis.y +\
            value.axis.z*value.axis.z);
        if(axis_norm < 0.00000001)
        {
            value.angle=0.0;
        }
        else
        {
            value.angle=asin(axis_norm);
        }
    }
}


////////////////////////////////////////////////////////////////////
//
SbBool
writePickedPath (SoNode *result, const SbViewportRegion &viewport,
const SbVec2s &cursorPosition)
//
////////////////////////////////////////////////////////////////////
{
    SoRayPickAction myPickAction(viewport);

// Set an 4-pixel wide region around the pixel
    myPickAction.setPoint(cursorPosition);
    myPickAction.setRadius(4.0);

// Start a pick traversal
    myPickAction.apply(result);
    const SoPickedPoint *myPickedPoint =
        myPickAction.getPickedPoint();
    if (myPickedPoint == NULL)
    {
        return FALSE;
    }

    SbVec3f myPosition;
    myPosition = myPickedPoint->getPoint();
    float position[3], x, y, z;
    float * data;
    long int sIdx, bIdx;
    int size;

    myPosition.getValue(x, y, z);
    position[0]=x; position[1]=y; position[2]=z;

    if (options[OPT_NORMALIZE_DATA]) for(int k=0; k<3; k++) {
        if(whichType != BIFURCATION)
        {
            float con = 0.0;
            float div = (mySolNode.max[k]-mySolNode.min[k])/2.0;
            if(div/mySolNode.max[k]>1.0e-10) 
            {
                div = 1.0/div;
                con = div*mySolNode.min[k];
            }
            if(div/mySolNode.max[k]>1.0e-10)
	         position[k] = (position[k]+con+1.0)/div;
        }
        else
        {
            float div = (myBifNode.max[k]-myBifNode.min[k])/2.0;
            if( !((myBifNode.max[k]<=1.0  && myBifNode.max[k]>0.5 &&
                myBifNode.min[k]>=-1.0 && myBifNode.min[k]<-0.5 )||
                (div<0.00000001)))
            {
                float avg = (myBifNode.max[k]+myBifNode.min[k])/2.0;
                position[k] = position[k]*div+avg;
            }
        }
    }

    lookForThePoint(position, bIdx, sIdx);
    int idix = 0;

    if(whichType != BIFURCATION &&
        (sIdx > mySolNode.totalNumPoints || sIdx < 0))
        return false;
    else if(whichType!=BIFURCATION)
    {
        data = new float[mySolNode.nar];
        if(data == NULL)
        {
            printf(" memory allocation failed!\n");
            exit(0);
        }
        size = mySolNode.nar;
        for(int ms=0; ms<mySolNode.nar; ++ms)
            data[ms]=clientData.solData[sIdx][ms];
        idix = bIdx;
    }

    if(whichType == BIFURCATION &&
        (bIdx > myBifNode.totalNumPoints || bIdx < 0))
        return false;
    else if(whichType == BIFURCATION)
    {
        data = new float[myBifNode.nar];
        if(data == NULL)
        {
            printf(" memory allocation failed!\n");
            exit(0);
        }
        size = myBifNode.nar;
        for(int ms=0; ms<myBifNode.nar; ++ms)
            data[ms]=clientData.bifData[bIdx][ms];
        idix = bIdx;
    }

    for(int ms=0; ms<clientData.numFM; ++ms)
    {
        fmData[2*ms]   = clientData.multipliers[idix][ms][0];
        fmData[2*ms+1] = clientData.multipliers[idix][ms][1];
    }

    popupFloquetMultiplierDialog(data, size, clientData.eigenvalues[idix]);
    delete [] data;
    return TRUE;
}


///////////////////////////////////////////////////////////////////
//
// This routine is called for every mouse button event.
//
void
myMousePressCB(void *userData, SoEventCallback *eventCB)
//
///////////////////////////////////////////////////////////////////
{
    SoSeparator *result = (SoSeparator *) userData;
    const SoEvent *event = eventCB->getEvent();

    if (SO_MOUSE_PRESS_EVENT(event, ANY))
    {
        const SbViewportRegion &myRegion =
            eventCB->getAction()->getViewportRegion();
        writePickedPath(result, myRegion,
            event->getPosition(myRegion));
        eventCB->setHandled();
    }
#ifdef R3B
    else
    {
    }
#endif
}


///////////////////////////////////////////////////////////////////
//
//       Initialize the coordinate selection drop down list items
//       and the drop down label list items.
//
void
initCoordAndLableListItems()
//
///////////////////////////////////////////////////////////////////
{
    int i = 0;
    for(i=0; i<MAX_LIST; i++)
    {
        sprintf(xAxis[i], "%d", i);
        sprintf(yAxis[i], "%d", i);
        sprintf(zAxis[i], "%d", i);
    }

    int sp = 0;
    strcpy(coloringMethodList[0],"STAB"); sp++;
    strcpy(coloringMethodList[1],"PONT"); sp++;
    strcpy(coloringMethodList[2],"BRAN"); sp++;

    if(whichType == SOLUTION)
    {
        strcpy(coloringMethodList[3],"TYPE"); sp++;
        strcpy(coloringMethodList[4],"LABL"); sp++;
        strcpy(coloringMethodList[5],"COMP"); sp++; //OCT 7 added
        for(i=0; i<MAX_LIST; i++)
                sprintf(coloringMethodList[i+sp], "%d",i);
        for(i=mySolNode.nar+sp; i<mySolNode.nar+mySolNode.npar+sp; ++i)
        {
            char tmpchar[5];
            sprintf(coloringMethodList[i], "PAR(%d)", mySolNode.parID[i-(mySolNode.nar+sp)]+1);
        }
    }
    else
    {
        for(i=0; i<MAX_LIST; i++)
            sprintf(coloringMethodList[i+sp], "%d", i);
    }
    specialColorItems = sp;

    if(blOpenSolFile)
    {
// the solution file does exist.
        numLabels = mySolNode.numOrbits;
        myLabels[0] = 0;
        for(int j=0; j<numLabels; j++) myLabels[j+1] = mySolNode.labels[j];
#ifdef R3B
// initial mass dependent options.
        float lastMass = mySolNode.mass[1];
        blMassDependantOption = true;
        for(i=1; i<mySolNode.numOrbits; i++)
        {
            if(fabs(mySolNode.mass[i]-lastMass)/lastMass > 1.0e-3)
            {
                blMassDependantOption = false;
                break;
            }
        }
        if(blMassDependantOption) mass = lastMass;
#endif
    }
    else
    {
        numLabels = myBifNode.totalLabels;
        myLabels[0] = 0;
        for(int j=0; j<numLabels; j++) myLabels[j+1] = myBifNode.labels[j];
#ifdef R3B
        blMassDependantOption = false;
#endif
    }

#ifdef R3B
    if(!blMassDependantOption)
    {
        options[OPT_PRIMARY ]= false;
        options[OPT_LIB_POINTS]= false;
    }
#endif

    options[OPT_LEGEND] = false;
    options[OPT_BACKGROUND] = false;

    numLabels += SP_LBL_ITEMS;
    myLabels[numLabels-1] = MY_NONE;
    myLabels[numLabels-2] = MY_SPEC;
    myLabels[numLabels-3] = MY_HALF;
    for( i=0; i<numLabels; ++i)
    {
        int jmii = i + SP_LBL_ITEMS;
        sprintf(labels[jmii], "%d", myLabels[i+1]);
#ifndef R3B
        switch (clientData.labelIndex[i][2])
#else
        switch (clientData.labelIndex[i+1][2])
#endif
        {
            case 1 :  
                strcat(labels[jmii]," BP");
                break;
            case 2 : 
                strcat(labels[jmii]," LP");
                break;
            case 3 :
                strcat(labels[jmii]," HB");
                break;
            case -4 :
                strcat(labels[jmii]," UZ");
                break;
            case 5 :
                strcat(labels[jmii]," LP");
                break;
            case 6 : 
                strcat(labels[jmii]," BP");
                break;
            case 7 :
                strcat(labels[jmii]," PD");
                break;
            case 8 :
                strcat(labels[jmii]," TR");
                break;
            case 9 :
                strcat(labels[jmii]," EP");
                break;
            case -9 :
                strcat(labels[jmii]," MX");
                break;
            case 0 : 
            case 4 :
            default :
                break;
        }
    }

    strcpy(labels[0],"ALL");
    strcpy(labels[1],"HALF");
    strcpy(labels[2],"SPEC");
    strcpy(labels[3],"NONE");
    numLabels--;

    if(whichType == SOLUTION)
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
    }

//---------------------- Begin ---------------------------OCT 7, 04

    int half = 2;
    int iLbl = 0;
    //tmp = strtok(manyChoice, ",");
    if( lblChoice[0] == -3) // ALL
    {
        lblIndices[0] = 0;
        half = 2;
#ifndef R3B
        iLbl = lblIdxSize;
#endif
    }
    else if(lblChoice[0] == -2)  // HALF
    {
        int j = 1;
        do
        {
#ifdef R3B
            if(abs(clientData.labelIndex[j][2])!= 4 || j%half == 0)
#else
            if(abs(clientData.labelIndex[j-1][2])!= 4 || j%half == 0)
#endif
                lblIndices[iLbl++] = j;
            j++;
        } while( j < numLabels-2 );

        half *= 2;
    }
    else if(lblChoice[0] == -1) // SPEC
    {
        int j = 1;
        do
        {
#ifndef R3B
            if(clientData.labelIndex[j-1][2] !=  TYPE_UZ    && clientData.labelIndex[j-1][2] != TYPE_RG )// &&
            //    clientData.labelIndex[j][2] != TYPE_EP_ODE && clientData.labelIndex[j][2] != TYPE_MX)
#else
            if(clientData.labelIndex[j][2] !=  TYPE_UZ    && clientData.labelIndex[j][2] != TYPE_RG &&
                clientData.labelIndex[j][2] != TYPE_EP_ODE && clientData.labelIndex[j][2] != TYPE_MX)
#endif
                lblIndices[iLbl++] = j;
            j++;
        } while( j < numLabels-2 );
        half = 2;
    }
    else if(lblChoice[0] == 0)  // NONE
    {
        lblIndices[iLbl++] = numLabels;
        half = 2;
    }
    else // Specified labels
    {
        for(int idx = 0; idx < lblIdxSize; idx++)
            lblIndices[iLbl++] = lblChoice[idx];
        half = 2;
        iLbl = lblIdxSize;
    }
    lblIdxSize = iLbl;

    for(int i=0; i<11; ++i)
    {
        optSol[i] = options[i];
        optBif[i] = options[i];
    }
 

//----------------------- End ----------------------------

    if(!setShow3D)
    {
        for(int i=0; i<MAX_LIST; i++)
            zCoordIndices[i] = -1;
    }
}


///////////////////////////////////////////////////////////////////
//  
//       Read line colors and line pattern from the resource file.
//       each call of this function, read one line
//
void
readLineColorAndPattern(char* buffer, float *lineColor, unsigned long & linePattern)
//
///////////////////////////////////////////////////////////////////
{
    char * word ;
    for(int k=0; k<3; ++k)
    {
        word = strtok(NULL,",");
        lineColor[k]=atof(word);
    }
    word = strtok(NULL,",");
    linePattern = strtoul(word,NULL, 16);
}


///////////////////////////////////////////////////////////////////
// 
//       Read flex number of numbers.
//       each call of this function, read one line
//
void
readNData(char* buffer, float *data, int &size )
//
///////////////////////////////////////////////////////////////////
{
    char * word ; 
    int k = 0;
    if(size > 0)
    {
        for(k=0; k<size; ++k)
        {
            word = strtok(NULL,",");
            data[k]=atof(word);
        }
    }
    else
    {
        while ( (word = strtok(NULL,",") )!= NULL)
        {
            data[k++]=atof(word);
        }
        size = k;
    }
}


///////////////////////////////////////////////////////////////////
//
//       Read flex number of integer numbers.
//       each call of this function, read one line
//
void
readNIntData(char* buffer, int *data, int &size )
//
///////////////////////////////////////////////////////////////////
{
    char * word ; 
    int k = 0;
    if(size > 0)
    {
        for(k=0; k<size; ++k)
        {
            word = strtok(NULL,",");
            data[k]=atoi(word);
        }
    }
    else
    {
        while ( (word = strtok(NULL,",") )!= NULL)
        {
            data[k++]=atoi(word);
        }
        size = k;
    }
}


///////////////////////////////////////////////////////////////////
//
//       Read a string from the buffer. Used to parse those boolean
//       variables or sigle value in a line of the resource file.
//
void
readAString(char* buffer, char* aString)
//
///////////////////////////////////////////////////////////////////
{
    char * word = strtok(NULL,",");
    strcpy(aString, word);
}


///////////////////////////////////////////////////////////////////
//
void
readAHexdecimal(char* buffer, unsigned long & aHexdecimal )
//
///////////////////////////////////////////////////////////////////
{
    char * word;
    word = strtok(NULL,",");
    aHexdecimal = strtoul(word,NULL, 16);
}


///////////////////////////////////////////////////////////////////
//
//    INITIALIZE all the variables
//    If the resource file exists, read it and update the default values.
//    If it does not exist, just return and use default values.
//
int
readResourceParameters()
//
///////////////////////////////////////////////////////////////////
{
    int state = 0;

    char buffer[256];
    float lineColors[13][3];
    float aVector[3];
    unsigned long linePatterns[13];
    unsigned long stability;
    unsigned long aHex;
    char aString[256], *strTemp;
    char resource[256];

    float data;
    FILE * inFile;

    strcpy(resource, autoDir);
#ifndef R3B
    strcat(resource,"/plaut04/plaut04.rc");
#else
    strcat(resource,"/plaut04/r3bplaut04.rc");
#endif

#ifndef R3B
    inFile = fopen("plaut04.rc", "r");
#else
    inFile = fopen("r3bplaut04.rc", "r");
#endif
    if (!inFile)
    {
        inFile = fopen(resource, "r");
        if(!inFile)
        {
            printf("Unable to open the  resource file. I will use the default values.\n");
            state = 1;
            return state;
        }
    }

    char * next;
    while ( (next=fgets(buffer, sizeof(buffer),inFile)) != NULL )
    {
#ifndef R3B
        if(buffer[0] != '#')
#else
        if(buffer[0] == '#')
        {
// this is a comment line, discard it. Nothing need to do here.
        }
        else
#endif
        {
            strTemp = strtok(buffer,"=");
            strTemp = strrighttrim(strTemp);
            strTemp = strlefttrim(strTemp);

            bool blDealt = false;
            if( !blDealt )
            {
                for(int i = 0; i<NUM_SP_POINTS; ++i)
                {
                    if(strcasecmp(strTemp, typeTokenNames[i])==0)
                    {
                        readLineColorAndPattern(buffer, aVector, aHex);
                        lineColor[i].setValue( aVector );
                        linePattern[i] = aHex;
                        blDealt = true;
                        break;
                    }
                }
            }

            if( !blDealt )
            {
                for(int i = 0; i<XtNumber(graphWidgetItems); ++i)
                {
                    if(strcasecmp(strTemp, graphWidgetItems[i])==0)
                    {
                        readAString(buffer, aString);
                        char* aNewString = strrighttrim(aString);
                        aNewString = strlefttrim(aString);
                        options[i] = (strcasecmp(aNewString,"Yes")==0) ? true : false;
                        blDealt = true;
                        break;
                    }
                }
            }

            if( !blDealt )
            {
                for(int i = 0; i<XtNumber(blWidgetName); ++i)
                {
                    if(strcasecmp(strTemp, blWidgetName[i])==0)
                    {
                        readAString(buffer, aString);
                        char* aNewString = strrighttrim(aString);
                        aNewString = strlefttrim(aString);
			switch(i) {
			case 0:
			    setShow3DSol = (strcasecmp(aNewString,"Yes")==0);
			    setShow3DBif = setShow3DSol;
			    break;
			case 1:
			    setShow3DBif = (strcasecmp(aNewString,"Yes")==0);
			    break;
			case 2:
			    setShow3DSol = (strcasecmp(aNewString,"Yes")==0);
			    break;
			}
                        blDealt = true;
                        break;
                    }
                }
            }

            if( !blDealt )
            {
                if(strcasecmp(strTemp,"Draw Scale")==0)
                {
                    readAString(buffer, aString);
                    char* aNewString = strrighttrim(aString);
                    aNewString = strlefttrim(aString);
                    blDrawTicker = (strcasecmp(aNewString,"Yes")==0) ? true : false;

                    blDealt = true;
                }
            }

            if( !blDealt )
            {
                for(int i = 0; i < XtNumber(intVariableNames) && (!blDealt); ++i)
                {
                    if(strcasecmp(strTemp, intVariableNames[i])==0)
                    {
                        readAString(buffer, aString);
                        char* aNewString = strrighttrim(aString);
                        aNewString = strlefttrim(aString);
                        switch (i)
                        {
                            case 0:
                                whichType = atoi(aString);
                                break;
                            case 1:
                                whichStyle = atoi(aString);
                                break;
                            case 2:
                                winWidth  = atoi(aString);
                                break;
                            case 3:
                                winHeight = atoi(aString);
                                break;
                            case 4:
                                coloringMethodType[SOLUTION] =
                                coloringMethodType[BIFURCATION] = atoi(aString);
                                break;
                            case 5:
                                coloringMethodType[SOLUTION] = atoi(aString);
                                break;
                            case 6:
                                coloringMethodType[BIFURCATION] = atoi(aString);
                                break;
                            case 7:
                                lineWidthScaler = atof(aString);
                                break;
                            case 8:
                                aniLineScaler= atof(aString);
                                break;
                            case 9:
                                MAX_SAT_SPEED = atoi(aString);
                                break;
                            case 10:
                                MIN_SAT_SPEED = atoi(aString);
                                break;
                            case 11:
                                MAX_ORBIT_SPEED = atoi(aString);
                                break;
                            case 12:
                                MIN_ORBIT_SPEED = atoi(aString);
                                break;
                            case 13:
                                whichCoord = atoi(aString);
                                break;
                            case 14:
                                bgTransparency = atof(aString);
                                break;
                            case 15:
                                numPeriodAnimated = atof(aString);
                                break;
                            case 16:
                                sphereRadius = atof(aString);
                                break;
                            case 17:
                                satRadius = atof(aString);
                                break;
#ifdef R3B
                            case 18:
                                largePrimRadius = atof(aString);
                                break;
                            case 19:
                                smallPrimRadius = atof(aString);
                                break;
                            case 20:
                                libPtScaler = atof(aString);
                                break;
                            case 21:
                                diskTransparency = atof(aString);
                                break;
                            case 22:
                                diskFromFile = (strcasecmp(aString,"Yes")==0) ? true : false;
                                break;
                            case 23:
                                whichCoordSystem = atoi(aString);
                                break;
                            case 24:
                                numOfStars = atoi(aString);
                                break;
#endif
                        }

                        blDealt = true;
                        break;
                    }
                }
            }

            if( !blDealt )
            {
                for(int i = 0; i<2; ++i)
                {
                    if(strcasecmp(strTemp, hexdecimalVarNames[i])==0)
                    {
                        readAHexdecimal(buffer, aHex);
                        stabilityLinePattern[i] = aHex;

                        blDealt = true;
                        break;
                    }
                }
            }

            if( !blDealt)
            {
                int size = 3;
                float colors[3];
                for(int i = 0; i<XtNumber(nDataVarNames); ++i)
                {
                    if(strcasecmp(strTemp, nDataVarNames[i])==0)
                    {
                        readNData(buffer, colors, size);
                        for(int j=0; j<3; ++j) envColors[i][j]=colors[j];
                        blDealt = true;
                        break;
                    }
                }
            }

            if(!blDealt && strcasecmp(strTemp, "parameter ID")==0)
            {
                int size = -1;
                int parIDs[MAX_PAR];
                readNIntData(buffer, parIDs, size);
                mySolNode.npar = size;
                blDealt = true;
                for(int is=0; is<size; ++is)
                {
                    mySolNode.parID[is] = parIDs[is];
                }
            }

//---------------------- Begin ---------------------------OCT 7, 04

            if(!blDealt && strcasecmp(strTemp, "Labels")==0)
            {
                int size = -1;
                int lblIdx[MAX_LABEL];
                readNIntData(buffer, lblIdx, size);
                lblIdxSize = size;
                blDealt = true;
                for(int is=0; is<size; ++is)
                {
                    lblChoice[is] = lblIdx[is];
                }
            }

//----------------------- End ---------------------------OCT 7, 04

            if(!blDealt)
            {
                for(int i = 0; i < XtNumber(axesNames) && (!blDealt); ++i)
                {
                    if(strcasecmp(strTemp, axesNames[i]) == 0)
                    {
                        int size = -1;
                        int pars[MAX_PAR];
                        readNIntData(buffer, pars, size);
#ifndef R3B
                        mySolNode.npar = size;
#else
//                        mySolNode.npar = size;
#endif
                        blDealt = true;
                        switch ( i )
                        {
                            case 0:
                                dai.solXSize = size;
                                for(int is=0; is<size; ++is)
                                    dai.solX[is] = pars[is];
                                break;
                            case 1:
                                dai.solYSize = size;
                                for(int is=0; is<size; ++is)
                                    dai.solY[is] = pars[is];
                                break;
                            case 2:
                                dai.solZSize = size;
                                for(int is=0; is<size; ++is)
                                    dai.solZ[is] = pars[is];
                                break;
                            case 3:
                                dai.bifXSize = size;
                                for(int is=0; is<size; ++is)
                                    dai.bifX[is] = pars[is];
                                break;
                            case 4:
                                dai.bifYSize = size;
                                for(int is=0; is<size; ++is)
                                    dai.bifY[is] = pars[is];
                                break;
                            case 5:
                                dai.bifZSize = size;
                                for(int is=0; is<size; ++is)
                                    dai.bifZ[is] = pars[is];
                                break;
                        }
                    }
                }
            }
        }
    }                           
    if( whichType != BIFURCATION )
    {
        setShow3D = setShow3DSol;
    }
    else
    {
        setShow3D = setShow3DBif;
    }
    coloringMethod = coloringMethodType[whichType];
    fclose(inFile);
    return state;
}


/////////////////////////////////////////////////////////////////////
//
//       Set initial values for those temp variables according to
//     their correspondent variables.
//
void
initTempVariables()
//
/////////////////////////////////////////////////////////////////////
{
    graphWidgetToggleSet = 0;
    for(int i = 0; i<XtNumber (graphWidgetItems); ++i)
    {
        if(options[i]) graphWidgetToggleSet |= (1 << i);
        optionsOld[i] = options[i];
        optionsTemp[i] = options[i];
    }

    graphWidgetToggleSetTemp = graphWidgetToggleSet;
    graphWidgetToggleSetOld  = graphWidgetToggleSet;

    whichTypeTemp = whichType;
    whichTypeOld = whichType;

    whichStyleTemp = whichStyle;
    whichStyleOld  = whichStyle;

#ifdef R3B
    whichCoordSystemTemp = whichCoordSystem;
    whichCoordSystemOld  = whichCoordSystem ;

#endif
    for(int i=0; i<NUM_SP_POINTS; ++i)
    {
        linePatternTemp[i] = linePattern[i];
        linePatternOld[i]  = linePattern[i];
        for(int j=0; j<3; ++j)
            lineColorOld[i][j] = lineColor[i][j];
    }

}


/////////////////////////////////////////////////////////////////////
//
//     Set default values for the base variables. These variable includes
//     lineColor, line pattern for each solution, the coordinate system,
//     the graph style, the graph type, orbit animation speed, sattelite
//     animation speed.
//
void
setVariableDefaultValues() 
//
/////////////////////////////////////////////////////////////////////
{
    winWidth  = WIN_WIDTH;
    winHeight = WIN_HEIGHT;

    lineColor[ 0][0] = 1.0;  lineColor[ 0][1] = 1.0;  lineColor[ 0][2] = 1.0;
    lineColor[ 1][0] = 1.0;  lineColor[ 1][1] = 0.0;  lineColor[ 1][2] = 0.0;
    lineColor[ 2][0] = 0.0;  lineColor[ 2][1] = 1.0;  lineColor[ 2][2] = 0.0;
    lineColor[ 3][0] = 0.0;  lineColor[ 3][1] = 0.0;  lineColor[ 3][2] = 1.0;
    lineColor[ 4][0] = 1.0;  lineColor[ 4][1] = 1.0;  lineColor[ 4][2] = 0.0;
    lineColor[ 5][0] = 0.5;  lineColor[ 5][1] = 0.5;  lineColor[ 5][2] = 0.0;
    lineColor[ 6][0] = 0.0;  lineColor[ 6][1] = 0.0;  lineColor[ 6][2] = 0.5;
    lineColor[ 7][0] = 0.0;  lineColor[ 7][1] = 0.5;  lineColor[ 7][2] = 0.5;
    lineColor[ 8][0] = 1.0;  lineColor[ 8][1] = 0.0;  lineColor[ 8][2] = 1.0;
    lineColor[ 9][0] = 0.0;  lineColor[ 9][1] = 1.0;  lineColor[ 9][2] = 1.0;
    lineColor[10][0] = 0.3;  lineColor[10][1] = 0.0;  lineColor[10][2] = 0.3;
    lineColor[11][0] = 0.6;  lineColor[11][1] = 0.0;  lineColor[11][2] = 0.6;
    lineColor[12][0] = 1.0;  lineColor[12][1] = 1.0;  lineColor[12][2] = 1.0;

    envColors[ 0][0] = 0.0;  envColors[ 0][1] = 0.0;  envColors[ 0][2] = 0.0;
    envColors[ 1][0] = 1.0;  envColors[ 1][1] = 0.0;  envColors[ 1][2] = 0.0;
    envColors[ 2][0] = 0.0;  envColors[ 2][1] = 1.0;  envColors[ 2][2] = 0.0;
    envColors[ 3][0] = 0.0;  envColors[ 3][1] = 0.0;  envColors[ 3][2] = 1.0;
    envColors[ 4][0] = 1.0;  envColors[ 4][1] = 0.0;  envColors[ 4][2] = 0.0;
    envColors[ 5][0] = 0.0;  envColors[ 5][1] = 1.0;  envColors[ 5][2] = 0.0;
    envColors[ 6][0] = 1.0;  envColors[ 6][1] = 0.0;  envColors[ 6][2] = 0.0;
    envColors[ 7][0] = 0.0;  envColors[ 7][1] = 0.0;  envColors[ 7][2] = 1.0;
    envColors[ 8][0] = 1.0;  envColors[ 8][1] = 0.0;  envColors[ 8][2] = 1.0;
    envColors[ 9][0] = 1.0;  envColors[ 9][1] = 0.0;  envColors[ 9][2] = 0.0;
#ifdef R3B
    envColors[10][0] = 1.0;  envColors[10][1] = 0.0;  envColors[10][2] = 0.0;
    envColors[11][0] = 0.0;  envColors[11][1] = 0.0;  envColors[11][2] = 1.0;
#endif

    for(int i=0; i<NUM_SP_POINTS; ++i)
        linePattern[i]   = 0xffff;

#ifndef R3B
    stabilityLinePattern[0]   = 0x1111;
#else
    stabilityLinePattern[0]   = 0xffff;
#endif
    stabilityLinePattern[1]   = 0xffff;

// set options.
    for(int i = 0; i < sizeof(options); ++i)
        options[i] = false;

#ifndef R3B
    options[OPT_NORMALIZE_DATA] = true;

#else
// set default graph type/style specification
    whichCoordSystem = ROTATING_F;
#endif
    whichStyle      = 0;  
#ifdef R3B
    whichCoord         = 3;
#endif
    whichType       = SOLUTION; 

    lblIdxSize       = 1;

    lblIndices[0]    = 0;   

#ifndef R3B
    dai.solXSize = 1;
    dai.solYSize = 1;
    dai.solZSize = 1;
    dai.solX[0] = 0;
    dai.solY[0] = 1;
    dai.solZ[0] = 2;

    dai.bifXSize = 1;
    dai.bifYSize = 1;
    dai.bifZSize = 1;
    dai.bifX[0] = 0;
    dai.bifY[0] = 1;
    dai.bifZ[0] = 2;

#endif
    if(solHead != NULL)
    {
        solutionp cur = solHead;
        while(cur)
        {
            cur=cur->next;
            delete solHead;
            solHead = cur;
        }
    }

#ifndef R3B
    setShow3D         = false;
#else
    setShow3D         = true;
#endif
    solHead           = NULL;
    animationLabel    = 0;
    orbitSpeed        = 1.0;
#ifndef R3B
    satSpeed          = 0.5;
#else
    satSpeed          = 1.0;
#endif
    lineWidthScaler   = 1.0;
#ifndef R3B
    coloringMethod    = -1;
    satRadius         = 1.0;
#endif
    sphereRadius      = 1.0;
    numPeriodAnimated = 1.0;

#ifdef R3B
    coloringMethod    = -5;
    satRadius         = 1.0;
    largePrimRadius   = 1.0;
    smallPrimRadius   = 1.0;
    blMassDependantOption = false;
//
#endif
    mySolNode.npar= 1;
    mySolNode.parID[0] = 10;
#ifdef R3B
    dai.solXSize = 1; dai.solX[0] = 1;
    dai.solYSize = 1; dai.solY[0] = 2;
    dai.solZSize = 1; dai.solZ[0] = 3;
    dai.bifXSize = 1; dai.bifX[0] = 4;
    dai.bifYSize = 1; dai.bifY[0] = 5;
    dai.bifZSize = 1; dai.bifZ[0] = 6;

#endif
    setShow3DSol = setShow3D;
    setShow3DBif = setShow3D;
    coloringMethodType[SOLUTION] = coloringMethodType[BIFURCATION] =
        coloringMethod;

    char * buf;
    if((buf=getenv("AUTO_DIR")) != NULL)
    {
       for(int il = 0; il < strlen(buf); ++il)
          autoDir[il] = buf[il];
    }
    else
    {
       autoDir[0]='.';
       autoDir[1]='\n';
    }
}


////////////////////////////////////////////////////////////////////////
//
//  main - init Inventor and Xt, set up a scene graph and the main window,
//  display this window, and loop forever...
//
int
main(int argc, char *argv[])
//
////////////////////////////////////////////////////////////////////////
{
// usage:
// drawme --- using the default file with name "s.text" as input file.
// drawme mu fileName --- using 'fileName' as input, file1 must be a AUTO s file.

// NOTE
// if the s.xxx file does exist, the mass read from it will be used to substitue
// that from the command line
// even there are different. Because sometime the command line input mu may be
// not as accurate as the one read from the s.xxx file
// so the mu read from command line is not important in general. It can be used
// only if the s.xxx file does not exist or the system fails to read it.

    FILE *inFile;
    int  total, rows;

    strcpy(sFileName,"fort.8");
    strcpy(bFileName,"fort.7");
    strcpy(dFileName,"fort.9");

    if( argc > 1 )
    {
        if(strcmp(argv[1], "97")==0)
        {
            if( argc == 4)  
            {
                strcpy(sFileName,argv[argc-2]);
                strcat(sFileName,"/q.");
                strcat(sFileName,argv[argc-1]);

                strcpy(bFileName,argv[argc-2]);
                strcat(bFileName,"/p.");
                strcat(bFileName,argv[argc-1]);

                strcpy(dFileName,argv[argc-2]);
                strcat(dFileName,"/d.");
                strcat(dFileName,argv[argc-1]);

            } 
            else if( argc == 3)
            {
                strcpy(sFileName,"q.");
                strcat(sFileName,argv[argc-1]);

                strcpy(bFileName,"p.");
                strcat(bFileName,argv[argc-1]);

                strcpy(dFileName,"d.");
                strcat(dFileName,argv[argc-1]);
            }
            else if( argc == 2)
            {
                strcpy(sFileName,"fort.8");
                strcpy(bFileName,"fort.7");
                strcpy(dFileName,"fort.9");
            }
        }
        else
        {
            if( argc == 3)  
            {
                strcpy(sFileName,argv[argc-2]);
                strcat(sFileName,"/s.");
                strcat(sFileName,argv[argc-1]);

                strcpy(bFileName,argv[argc-2]);
                strcat(bFileName,"/b.");
                strcat(bFileName,argv[argc-1]);

                strcpy(dFileName,argv[argc-2]);
                strcat(dFileName,"/d.");
                strcat(dFileName,argv[argc-1]);

            } 
            else if( argc == 2)
            {
                strcpy(sFileName,"s.");
                strcat(sFileName,argv[argc-1]);

                strcpy(bFileName,"b.");
                strcat(bFileName,argv[argc-1]);

                strcpy(dFileName,"d.");
                strcat(dFileName,argv[argc-1]);
            }
        }
    }
    else if( argc == 1)
    {
        strcpy(sFileName,"fort.8");
        strcpy(bFileName,"fort.7");
        strcpy(dFileName,"fort.9");
    }
    else
    {
#ifndef R3B
        printf(" usage: plaut04 [version] [path] [name]\n");
#else
        printf(" usage: r3bplaut04 [path] [name]\n");
#endif
        printf(" For example:\n");
#ifndef R3B
        printf("      plaut04            --- view the fort.7, fort.8 in the current directory \n");
        printf("      plaut04 H1         --- view s.H1, b.H1 in the current directory \n");
        printf("      plaut04 /home/he/myR3B/me H1    --- view s.H1, b.H1 in the /home/he/myR3B/me directory \n");
        printf("      plaut04 97 H1                   --- view AUTO 97 files: q.H1, p.H1 in the current directory \n");
        printf("      plaut04 97 /home/he/myR3B/me H1 --- view AUTO 97 files: q.H1, p.H1 in the /home/he/myR3B/me directory \n");
#else
        printf("      r3bplaut04            --- view the fort.7, fort.8 in the current directory \n");
        printf("      r3bplaut04 H1         --- view s.H1, b.H1 in the current directory \n");
        printf("      r3bplaut04 /home/he/myR3B/me H1    --- view s.H1, b.H1 in the /home/he/myR3B/me directory \n");
        printf("      r3bplaut04 97 H1                   --- view AUTO 97 files: q.H1, p.H1 in the current directory \n");
        printf("      r3bplaut04 97 /home/he/myR3B/me H1 --- view AUTO 97 files: q.H1, p.H1 in the /home/he/myR3B/me directory \n");
#endif
        exit(1) ;
    }

    setVariableDefaultValues();

    readResourceParameters();

    readSolutionAndBifurcationData(1);

    initCoordAndLableListItems();
    initTempVariables();

    soxtmain(argv);

    return 0;
}


//////////////////////////////////////////////////////////////////////////
//
void
postDeals()
//
//////////////////////////////////////////////////////////////////////////
{

    delete [] mySolNode.time;
    delete [] mySolNode.xyzCoords;
    delete [] mySolNode.xAxisItems;
    delete [] mySolNode.yAxisItems;
    delete [] mySolNode.zAxisItems;

    delete [] myBifNode.xyzCoords;
    delete [] myBifNode.ptStability;

    delete [] clientData.multipliers;
    delete [] clientData.solMax;
    delete [] clientData.solMin;

    for(int i=0; i<mySolNode.totalNumPoints; ++i)
        delete [] clientData.solData[i];
    mySolNode.totalNumPoints  = 0;
    delete [] clientData.solData;
    for(int i=0; i<myBifNode.totalNumPoints; ++i)
        delete [] clientData.bifData[i];
    myBifNode.totalNumPoints = 0;
    delete [] clientData.bifData;

    delete solHead;
}


//////////////////////////////////////////////////////////////////////////
//
int
writePreferValuesToFile()
//
//////////////////////////////////////////////////////////////////////////
{
    int state = 0;

    char buffer[256];
    float lineColors[13][3];
    float aVector[3];
    unsigned long linePatterns[13];
    unsigned long stability;
    unsigned long aHex;
    char aString[256], *strTemp;

    float data;
    FILE * outFile;
#ifndef R3B
    outFile = fopen("plaut04.rc.out", "w");
#else
    outFile = fopen("r3bplaut04.rc.out", "w");
#endif

    if (!outFile)
    {
#ifndef R3B
        printf("Unable to open the  resource file. I will use the default values.\n");
#else
        printf("Unable to open the  resource file.\n");
#endif
        state = 1;
        return state;
    }

// write header
    fprintf(outFile,"#version 0.0\n\n");
    fprintf(outFile,"# Line colors are represented by RGB values from 0 to 1.0.\n");
    fprintf(outFile,"# DEFAULT color is also used when animationLabel == 0, i.e.,\n");
    fprintf(outFile,"# when showing all solutions and highlighting the solutions changes.\n");
    fprintf(outFile,"# Point Type         RED  GREEN  BLUE\n");

// write line color and pattern
    for(int i = 0; i<NUM_SP_POINTS; ++i)
    {
        fprintf(outFile,"%-15.15s    = %3.1f,   %3.1f,   %3.1f, 0x%lx\n",typeTokenNames[i],
            lineColor[i][0], lineColor[i][1],lineColor[i][2],linePattern[i]);
    }

    fprintf(outFile, "\n# Initialize the line pattern for showing stability:\n");
    for(int i = 0; i<2; ++i)
    {
        fprintf(outFile,"%-21.21s = 0x%lx\n", hexdecimalVarNames[i], stabilityLinePattern[i]);
    }

    fprintf(outFile, "\n#  Initialize the default options\n");
    for(int i = 0; i<XtNumber(graphWidgetItems); ++i)
    {
        fprintf(outFile, "%-21.21s = ",graphWidgetItems[i]);
        options[i] ? fprintf(outFile, " Yes\n") : fprintf(outFile, " No\n");
    }

    for(int i = 0; i < XtNumber(intVariableNames); ++i)
    {
        switch (i)
        {
            case 0:
                // OK
                fprintf(outFile, "\n# Initialize the default graph type: \n"); 
                fprintf(outFile, "#  0 --- Solution (fort.8)\n");
                fprintf(outFile, "#  1 --- Bifurcation (fort.7)\n");
                fprintf(outFile, "%-25.25s =  %i\n", intVariableNames[i],whichType);
                break;
            case 1:
//OK
                fprintf(outFile, "\n# Initialize the default graph style:\n");
                fprintf(outFile, "#  0 --- LINES,  1 --- TUBES, 2 ---- SURFACE \n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n",whichStyle);
                break;
            case 2:
                fprintf(outFile, "\n# Set the window width and height:\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n",winWidth);
                break;
            case 3:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n",winHeight);
                break;
            case 4:
                fprintf(outFile, "\n# Set coloring method.\n");
                fprintf(outFile, "#   -6 --- STABILITY\n");
                fprintf(outFile, "#   -5 --- POINT\n");
                fprintf(outFile, "#   -4 --- BRANCH\n");
                fprintf(outFile, "#   -3 --- TYPE\n");
                fprintf(outFile, "#   -2 --- LABEL\n");
                fprintf(outFile, "#   -1 --- COMPONENT\n");
                fprintf(outFile, "# Otherwise, according to the data in the ith column of the solution file.\n");
                fprintf(outFile, "# It can only be set to an integer value.\n");
                break;
            case 5:
                fprintf(outFile, "# For the solution diagram:\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", coloringMethodType[SOLUTION]);
                break;
            case 6:
                fprintf(outFile, "# For the bifurcation diagram:\n");
                fprintf(outFile, "%-27.27s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", coloringMethodType[BIFURCATION]);
                break;
            case 7:
                fprintf(outFile, "\n# Line Width Scaler adjusts the thickness of curves:\n"); 
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", lineWidthScaler);
                break;
            case 8:
                fprintf(outFile, "\n# The AniLine Thickness Scaler sets the thickness of animated solution curves:\n"); 
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", aniLineScaler);
                break;
            case 9:
                fprintf(outFile, "\n# Set the maximum and minimum animation speed:\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", MAX_SAT_SPEED);
                break;
            case 10:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", MIN_SAT_SPEED);
                break;
            case 11:
                fprintf(outFile, "\n# Set the maximum and minimum highlighting animation speed:\n"); 
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", MAX_ORBIT_SPEED);
                break;
            case 12:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", MIN_ORBIT_SPEED);
                break;
            case 13:
                fprintf(outFile, "\n# Initialize the default coordinate axes:\n");
                fprintf(outFile, "#  0 --- None,\n");
                fprintf(outFile, "#  1 --- at geometry center or origin,\n");
                fprintf(outFile, "#  2 --- at left and behind,\n");
                fprintf(outFile, "#  3 --- at left and ahead. \n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", whichCoord);
                break;
            case 14:
                fprintf(outFile, "\n# Background pictures transparency");
                fprintf(outFile, "\n# [0.0, 1.0] \n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", bgTransparency );
                break;
            case 15:
#ifdef R3B
                fprintf(outFile, "\n# Set default number of periods animated\n");
                fprintf(outFile, "\n# The value should be power of 2.\n");
#else
                fprintf(outFile, "\n# set default number of periods showing in inertial frame\n");
#endif
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%f\n", numPeriodAnimated);
                break;
            case 16:
                fprintf(outFile, "\n# Set the radius of the spheres used for labels:\n");
                fprintf(outFile, "# The normal size is 1.0.\n");
                fprintf(outFile, "# For smaller radius, use 0.xxx\n");
                fprintf(outFile, "# For bigger radius, use  X.XXX\n"); 
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", sphereRadius);
                break;
            case 17:
#ifdef R3B
                fprintf(outFile, "\n# set the radius of  satellite, large primary, small primary\n");
#else
                fprintf(outFile, "\n# Set the radius of the animation object:\n");
#endif
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", satRadius);
                break;
#ifdef R3B
            case 18:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", largePrimRadius);
                break;
            case 19:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", smallPrimRadius);
                break;
            case 20:
                fprintf(outFile, "%s = ", intVariableNames[i]);
                fprintf(outFile, "%f\n", libPtScaler);
                break;
            case 21:
                fprintf(outFile, "\n# Disk Transparency [0, 1] \n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", diskTransparency );
                break;
            case 22:
                fprintf(outFile, "\n# Read Disk From File \n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                (diskFromFile) ? fprintf(outFile, "Yes \n"): fprintf(outFile, "No\n");
                break;
            case 23:
                fprintf(outFile, "\n# initialize the default coordinate system");
                fprintf(outFile, "\n#  0 --- Rotating, 1 --- inertial Bary Centered,");
                fprintf(outFile, "\n#  2 --- Big Primary Centered, 3 --- Small Primary Centered\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n",whichCoordSystem);
                break;
            case 24:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", numOfStars);
                break;
#endif
        }
    }

    for(int i = 0; i<XtNumber(nDataVarNames); ++i)
    {
        switch ( i ){
            case 0 :
                fprintf(outFile, "# Background color:\n");
                break;
            case 1 :
                fprintf(outFile, "# X-Axis color:\n");
                break;
            case 2 :
                fprintf(outFile, "# Y-Axis color:\n");
                break;
            case 3 :
                fprintf(outFile, "# Z-Axis color:\n");
                break;
            case 4 :
#ifdef R3B
                fprintf(outFile,"\n# sat, large prim, large prim line, small prim, small prim line\n");
                /* fall though */
            case 5 :
            case 6 :
            case 7 :
            case 8 :
                break;
            case 9 :
#else
                fprintf(outFile, "# Color of the animation object:\n"); 
                break;
            case 5 :
#endif
                fprintf(outFile, "# Surface color:\n");
                break;
#ifdef R3B
            case 10 :
#else
            case 6 :
#endif
                fprintf(outFile, "# Unstable solution Color:\n");
                break;
#ifdef R3B
            case 11 :
#else
            case 7 :
#endif
                fprintf(outFile, "# Stable solution Color:\n");
                break;
        }

        fprintf(outFile, "%-25.25s = %4.1f, %4.1f, %4.1f\n\n",nDataVarNames[i],envColors[i][0], envColors[i][1],envColors[i][2]);
    }

//----------Labels --- ?
    fprintf(outFile, "# Labeled solutions:\n");
    fprintf(outFile, "#   -3 = Show all labeled solutions\n");
    fprintf(outFile, "#   -2 = Show HALF labeled solutions\n");
    fprintf(outFile, "#   -1 = Show SPEC labeled solutions\n");
    fprintf(outFile, "#   0 = Show NONE of the solutions\n");
    fprintf(outFile, "# Otherwise, show the specified solution(s)\n");
    for(int is=0; is < (lblChoice[is] <= 0 ? 1 : lblIdxSize); ++is)
    {
        fprintf(outFile, "Labels   = %i", lblChoice[is]); 
    }
    fprintf(outFile, "\n");

    fprintf(outFile,"\n# Draw Scale:\n");
    fprintf(outFile, "Draw Scale = ");
    (blDrawTicker) ? fprintf(outFile, " YES \n") : fprintf(outFile, " NO \n");

// deal with parameter IDs
    fprintf(outFile,"\n# Set the active AUTO parameter indices:\n"); 
    fprintf(outFile, "parameter ID  = ");
    for(int is=0; is<mySolNode.npar; ++is)
    {
        fprintf(outFile, " %i, ", mySolNode.parID[is]);
    }
    fprintf(outFile, " \n");

// turn 3D/2D
    fprintf(outFile,"\n# Choose 3D or 2D graph for the bifurcation diagram:\n"); 
    fprintf(outFile, "3DBif = %s\n", setShow3DBif ? "YES" : "No");
    fprintf(outFile,"\n# Choose 3D or 2D graph for the solution diagram:\n"); 
    fprintf(outFile, "3DSol = %s\n", setShow3DSol ? "YES" : "No");

    fprintf(outFile,"\n# Set X, Y, Z axes for the solution diagram:\n");
    fprintf(outFile,"# 0 is Time for X,Y,Z.\n");
    fprintf(outFile,"%-25.25s = ", axesNames[0]);
    for(int is=0; is<dai.solXSize; ++is)
    {
        fprintf(outFile, " %i ", dai.solX[is]);
        fprintf(outFile, (is != dai.solXSize-1) ? "," : "\n");
    }

    fprintf(outFile,"%-25.25s = ", axesNames[1]);
    for(int is=0; is<dai.solYSize; ++is)
    {
        fprintf(outFile, " %i ", dai.solY[is]);
        fprintf(outFile, (is != dai.solYSize-1) ? "," : "\n");
    }

    fprintf(outFile,"%-25.25s = ", axesNames[2]);
    for(int is=0; is<dai.solZSize; ++is)
    {
        fprintf(outFile, " %i ", dai.solZ[is]);
        fprintf(outFile, (is != dai.solZSize-1) ? "," : "\n");
    }

    fprintf(outFile,"\n# Set X, Y, Z axes for the bifurcation diagram:\n");
    fprintf(outFile,"# 0 is Time for X,Y,Z.\n");
    fprintf(outFile,"%-25.25s = ", axesNames[3]);
    for(int is=0; is<dai.bifXSize; ++is)
    {
        fprintf(outFile, " %i ", dai.bifX[is]);
        fprintf(outFile, (is != dai.bifXSize-1) ? "," : "\n");
    }

    fprintf(outFile,"%-25.25s = ", axesNames[4]);
    for(int is=0; is<dai.bifYSize; ++is)
    {
        fprintf(outFile, " %i ", dai.bifY[is]);
        fprintf(outFile, (is != dai.bifYSize-1) ? "," : "\n");
    }

    fprintf(outFile,"%-25.25s = ", axesNames[5]);
    for(int is=0; is<dai.bifZSize; ++is)
    {
        fprintf(outFile, " %i ", dai.bifZ[is]);
        fprintf(outFile, (is != dai.bifZSize-1) ? "," : "\n");
    }

    fclose(outFile);
    return state;
}


//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
createStarryBackground(int total, float diameter)
//
//////////////////////////////////////////////////////////////////////////
{
    static bool starsCreated = false;
    static SoSeparator * myBg = new SoSeparator;
    if(!starsCreated)
    {
        starsCreated = true;
        srand(time(NULL));
        float div = floor(diameter);
        SoSphere * star = new SoSphere;
        star->radius = 0.002*(rand()%5+1);
        float x, y, z;
        for(int i=0; i<total; ++i)
        {
            SoSeparator * aStar = new SoSeparator;
            SoTranslation *starTrans = new SoTranslation;
            x = ((rand()%200)/100.0-1)*diameter;
            y = ((rand()%200)/100.0-1)*diameter;
            z = ((rand()%200)/100.0-1)*diameter;
            starTrans->translation.setValue(x, y, z);
            aStar->addChild(starTrans);
            aStar->addChild(star);
            myBg->addChild(aStar);
        }
    }
    myBg->unrefNoDelete();
    return myBg;
}




////////////////////////////////////////////////////////////////////////
//
//        detach everything and nuke the existing scene.
//
void
deleteScene()
//
////////////////////////////////////////////////////////////////////////
{
#ifdef R3B
    root->deselectAll();

#endif
    for (int i = root->getNumChildren(); i>0; i--)
        root->removeChild(i-1);
}


////////////////////////////////////////////////////////////////////////
//
bool 
parseFileName(const char *fname,char * sFileName, char * bFileName, char * dFileName)
//
////////////////////////////////////////////////////////////////////////
{
    char * path;
    char * name;
    char * filename = strdup(fname);
    char myName[256];
    char myPath[256];

    path = strtok(filename, "/");
    myPath[0]='\0';
    while(path !=NULL)
    {
        strcat(myPath,"/");
        strcat(myPath, path);
        strcpy(myName, path);
        path = strtok(NULL,"/");
    }

    strcpy(sFileName,myPath);
    strcpy(bFileName,myPath);
    strcpy(dFileName,myPath);

    int j = strlen(myPath);
    int i =strlen(myName);
    sFileName[j-i]='s'; sFileName[j-i+1]='.';
    bFileName[j-i]='b'; bFileName[j-i+1]='.';
    dFileName[j-i]='d'; dFileName[j-i+1]='.';

    free(filename);
    return true;
}


////////////////////////////////////////////////////////////////////////
//
//        Reads the given file and insert the geometry under the selection
//  node. If the node didn't have any children, the viewAll() method is
//  automatically called.
//
SbBool
readFile(const char *filename)
//
////////////////////////////////////////////////////////////////////////
{
    SbBool hadNoChildren = (root->getNumChildren() == 0);

    SbString errorMessage;
    SoSeparator *sep ;

    postDeals();
    parseFileName(filename, sFileName, bFileName, dFileName);
#ifdef R3B
    readResourceParameters();
#endif
    bool rs = readSolutionAndBifurcationData(0);
    if(!rs)
    {
        SoText2 * errMsg = new SoText2;
        errMsg->string.setValue(" Open File Error! Maybe it is not a correct AUTO data file. ");
        root->addChild(errMsg);
        return false;
    }
    initCoordAndLableListItems();
    setListValue();
    initTempVariables();

    updateScene();

    return TRUE;
}


/////////////////////////////////////////////////////////////////////////////////
//
static FILE *
convertToInventor(const char *filename)
//
////////////////////////////////////////////////////////////////////////
{
#define BUFSIZE 512
#define destinationFileType "Inventor2.1File"

    char routeprintCmd[BUFSIZE];
    char conversionCmd[BUFSIZE];
    FILE *pipeFile;
    FILE *ivDataPipe = NULL;

    sprintf(routeprintCmd, "/usr/sbin/routeprint -d %s %s 2> /dev/null",
        destinationFileType, filename);

    if (NULL != (pipeFile = popen(routeprintCmd, "r")))
    {
        (void) fgets(conversionCmd, BUFSIZE, pipeFile);
        if (0 != pclose(pipeFile))
        {
            return ivDataPipe;
        }
        ivDataPipe = popen(conversionCmd, "r");
    }

    return ivDataPipe;
}


/////////////////////////////////////////////////////////////////////////////
//
// Read all objects from the given file and return under one separator.
//
SoSeparator *
MyFileRead(const char *filename, SbString &errorMessage)
//
/////////////////////////////////////////////////////////////////////////////
{
    SoInput in;
    SbBool needToClosePipe = FALSE;
    FILE *ivDataPipe;

    if (0 != access(filename, R_OK))
    {
        errorMessage = "Error opening the file\n\"";
        errorMessage += filename;
        errorMessage += "\".";
        errorMessage += "\n\nThe file is unreadable or does not exist.";
        errorMessage += "\nYou may not have read permission on this file.";
        return NULL;
    }

    if (! in.openFile(filename, TRUE))
    {
        errorMessage = "Error opening the file\n\"";
        errorMessage += filename;
        errorMessage += "\".";
        errorMessage += "\n\nInventor was unable to read that file.";
        errorMessage += "\nYou may not have permission to read this file.";
        return NULL;
    }


    if (! in.isValidFile() )
    {
        if ((ivDataPipe = convertToInventor(filename)) != NULL)
        {
            in.setFilePointer(ivDataPipe);
            needToClosePipe = TRUE;
        }
        else
        {
            errorMessage = "Unable to read the file\n\"";
            errorMessage += filename;
            errorMessage += "\".";
            errorMessage += "\n\nInventor was unable to read that file, and";
            errorMessage += "\nthat file could not be translated to Inventor format.\n\n";
            errorMessage += "Are you sure that this is a supported filetype?";
            errorMessage += "\nYou may not have the proper translators installed,";
            errorMessage += "\nor the desktop filetype database may not be built.";
            return NULL;
        }
    }


    SoSeparator *sep = SoDB::readAll(&in);

    if (sep == NULL)
    {
        errorMessage = "Inventor encountered an error while reading\n\"";
        errorMessage += filename;
        errorMessage += "\".";
        errorMessage += "\n\nThis may not be a valid Inventor file.";
        return NULL;
    }

    in.closeFile();
    if (TRUE == needToClosePipe)
    {
        fclose(ivDataPipe);
    }
    return sep;
}
