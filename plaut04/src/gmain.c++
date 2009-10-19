#define XtNumber(arr) ((sizeof(arr) / sizeof(arr[0])))

#include "gplaut04.h"
#include "gVarNames.h"
#include "tube.h"
#include <float.h>

#define TIME_IS_OFF  0 
#define TIME_ON_X    1
#define TIME_ON_Y    2
#define TIME_ON_Z    3

float STATIONARY_POINT_RADIUS = 0.01;
int specialColorItems = CL_SP_ITEMS;

extern void  rounding(double &, double &);

float fmData[12];
const char *autoDir;

SbColor lineColor[NUM_SP_POINTS];
SbColor lineColorTemp[NUM_SP_POINTS];
SbColor lineColorOld[NUM_SP_POINTS];
#ifndef R3B
SbColor envColors[8];
#else
SbColor envColors[12];
#endif

struct DefaultAxisItems dai;

unsigned long linePattern[NUM_SP_POINTS];
unsigned long linePatternTemp[NUM_SP_POINTS];
unsigned long linePatternOld[NUM_SP_POINTS];

unsigned long stabilityLinePattern[2];
unsigned long stabilityLinePatternTemp[2];

#ifndef R3B
bool blDrawTicker = false;
#else
bool blDrawTicker = true;
#endif

float diskTransparency = 0.7;
bool diskFromFile = false;
float diskRotation[4] = {1,0,0,M_PI_2};
float diskPosition[3];
float diskRadius = 1.0;
float diskHeight = 0.001;

float sphereTransparency = 0.7;
bool sphereFromFile = false;
float spherePosition[3];
float sphereRadius = 1.0;

int whichType= 0 ;
int whichTypeTemp= 0 ;
int whichTypeOld = 0 ;

int whichStyle= 0 ;
int whichStyleTemp= 0 ;
int whichStyleOld = 0 ;

#ifndef R3B
int whichCoord = 0 ;
#else
int whichCoord = 3 ;
#endif
int whichCoordTemp = 0 ;
int whichCoordOld = 0 ;
int whichCoordSystem = 0 ;
int whichCoordSystemTemp = 0 ;
int whichCoordSystemOld = 0 ;
int time_on = 0;

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
long int animationLabel = MY_ALL;
int maxComponent = 1;
int curComponent = 1;

int winWidth, winHeight;

int xCoordIndices[MAX_LIST], xCoordIdxSize;
int yCoordIndices[MAX_LIST], yCoordIdxSize;
int zCoordIndices[MAX_LIST], zCoordIdxSize;
int lblIndices[MAX_LABEL], lblChoice[MAX_LABEL], lblIdxSize;

int MAX_SAT_SPEED = 100;
int MIN_SAT_SPEED = 0;
int MAX_ORBIT_SPEED = 100;
int MIN_ORBIT_SPEED = 0;

float orbitSpeed = 1.0;
float satSpeed   = 0.5;
float satRadius  = 1.0;
float lineWidthScaler = 1.0;
float aniLineScaler = 2.0;
static float labelRadius = 1.0;

int   coloringMethod = -1;
int   coloringMethodType[2] = {-1, -1};
float bgTransparency = 0;
float numPeriodAnimated = 1.0;

long int numLabels;

double legendScaleValues[2];

SoSeparator *userScene;
SoSeparator *root;
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

static SoSeparator * drawAStrip(float stripSet[][3], int size);
#if 0
static SoSeparator * drawATube(TubeNode cnode);
static SoSeparator * MyFileRead(const char *filename, SbString &errorMessage);
#endif
static void setLegendValues(double* values);

static void lookForThePoint(float position[],long int &bIdx, long int &sIdx);

#if 0
static SoSeparator * createStarryBackground(int total,float diameter);
#endif
static SoGroup * setLineColorBlendingByStability(float * vertices, long int size, int stab, float scaler);

static SoSeparator * addLegend();

static SoSeparator * createSolutionSceneWithWidgets();
static SoSeparator * createBifurcationScene();
static SoSeparator * renderSolution();
static SoSeparator * renderBifurcation();
static SoSeparator * animateSolutionUsingTubes(bool aniColoring);
static SoSeparator * animateSolutionUsingLines(bool aniColoring);
static SoSeparator * animateSolutionUsingPoints(int style, bool aniColoring);
static SoSeparator * animateOrbitWithTail(int iBranch, long int j,long  int si);
static SoSeparator * drawAnOrbitUsingLines(int iBranch, long int l, long int si, float scaler, int stability, int type, bool coloring);
static SoSeparator * drawAnOrbitUsingPoints(int style, int iBranch,  long int l, long int si, float scaler, int stability, int type, bool aniColoring);
static SoSeparator * drawAnOrbitUsingNurbsCurve(int iBranch, long int l, long int si, float scaler, int stability, int type);
static SoSeparator * drawAnOrbitUsingTubes(int iBranch, long int l, long int si, float scaler, int stability, int type);
static SoSeparator * drawABifBranchUsingLines(int iBranch, long int l, long int si, float scaler);
static SoSeparator * drawABifBranchUsingNurbsCurve(int iBranch,long int l, long int si, float scaler);
#if 0
static SoSeparator * drawABifLabelInterval(long int l, long int si, float scaler, int stability, int type);
static SoSeparator * drawABifLabelIntervalUsingNurbsCurve(long int l, long int si, float scaler, int stability, int type);
#endif
#if 0
static SoSeparator * drawASphereWithColor(float color[], float position[], float size);
#endif
static SoSeparator * drawASolBranchUsingSurface(long obStart, long obEnd, long numVert);

static SoMaterial  * setLabelMaterial(int lblType);
static SoSeparator * drawStarryBackground(char * bgFileName);

static int readResourceParameters();
static void copySolDataToWorkArray(int varIndices[]);
static void searchForMaxMin(int component, int  varIndices[]);
static void copyBifDataToWorkArray(int varIndices[]);

#if 0
static void processPrinting(char* filename );
#endif

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


#if 0
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
#endif


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
    char *command = new char [strlen(autoDir) + 50];
    sprintf(command, "%s%s", autoDir, "/plaut04/doc/userguide.pdf");
    if (access(command, R_OK) != 0)
    {
        system("xmessage 'Sorry, could not find "
            "userguide.pdf' > /dev/null");
        return;
    }

    int err = system("which xpdf> /dev/null");
    if (err)
    {
        system("xmessage 'You must install xpdf"
            " for this function to work' > /dev/null");
        return;
    }

    sprintf(command, "%s%s%s", "xpdf ",autoDir,"/plaut04/doc/userguide.pdf &");
    system(command);
    delete [] command;
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

    for(int i=0; i<mx; i++)
    {
        curComponent = i+1;
        varIndices[0]=xCoordIndices[(i>=xCoordIdxSize)?(i%xCoordIdxSize):(i)];
        varIndices[1]=yCoordIndices[(i>=yCoordIdxSize)?(i%yCoordIdxSize):(i)];
        varIndices[2]=zCoordIndices[(i>=zCoordIdxSize)?(i%zCoordIdxSize):(i)];

        if (varIndices[0] == 0) time_on = TIME_ON_X; 
        if (varIndices[1] == 0) time_on = TIME_ON_Y; 
        if (varIndices[2] == 0) time_on = TIME_ON_Z; 

        if(whichType != BIFURCATION)
        {
            animationLabel = myLabels[lblIndices[0]];
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
    SoSeparator *result = new SoSeparator;

    if(options[OPT_NORMALIZE_DATA])
    {
        normalizeSolData();
    }

#ifdef R3B
    result->addChild(createR3BPoints(mySolNode.min, mySolNode.max));
    if(whichCoordSystem == ROTATING_F)
#endif
    {
        if(whichCoord != NO_COORD)
        {
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
                if (time_on == TIME_ON_X) asMax[0] = 1;
                else if (time_on == TIME_ON_Y) asMax[1] = 1;
                else if (time_on == TIME_ON_Z) asMax[2] = 1;
            }
            else
            {
                asMax[0] = asMax[1] = asMax[2] = 1;
                asMin[0] = asMin[1] = asMin[2] = -1;
            }

            coordSep->addChild(createCoordinates(setShow3D, whichCoord, asMax, asMin, tickers, &envColors[1]));
            result->addChild(coordSep);
        }

//  create solution scene
        result->addChild(renderSolution());

    }

//  create starry background
    if(options[OPT_BACKGROUND])
    {
        char *bgFileName = new char [strlen(autoDir) + 34];
        sprintf(bgFileName, "%s%s",autoDir, "/plaut04/widgets/background.jpg");
        result->addChild(drawStarryBackground(bgFileName));
        delete [] bgFileName;
    }

//  add legend
    if(options[OPT_LEGEND])
    {
        result->addChild(addLegend());
    }

#ifndef R3B
    if (options[OPT_REF_PLAN])
      result->addChild(createDisk(diskPosition,diskRadius));
#endif
    if (options[OPT_REF_SPHERE])
      result->addChild(createSphere(spherePosition,sphereRadius));

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
        color[1].setValue(0,0,1);
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


//////////////////////////////////////////////////////////////////////////
//
// Routine to create a scene graph representing an auto bifurcation
//
SoSeparator *
createBifurcationScene()
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator *result = new SoSeparator;
    result->ref();

    if(whichCoord != NO_COORD)
    {
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
            asMax[0]=myBifNode.max[0]; asMax[1]=myBifNode.max[1];asMax[2]=myBifNode.max[2];
            asMin[0]=myBifNode.min[0]; asMin[1]=myBifNode.min[1];asMin[2]=myBifNode.min[2];
        }
        else
        {
            asMax[0] = asMax[1] = asMax[2] = 1;
            asMin[0] = asMin[1] = asMin[2] = -1;
        }

        coordSep->addChild(createCoordinates(setShow3D, whichCoord, asMax, asMin, tickers, &envColors[1]));

        result->addChild(coordSep);
    }

#ifdef R3B
    result->addChild(createR3BPoints(myBifNode.min, myBifNode.max));
#endif

// create bifurcation graph
    SoSeparator * bifBranchSep = renderBifurcation();
    result->addChild(bifBranchSep);

// create starry background
    if(options[OPT_BACKGROUND])
    {
        char *bgFileName = new char [strlen(autoDir) + 34];
        sprintf(bgFileName, "%s%s",autoDir, "/plaut04/widgets/background.rgb");
        result->addChild(drawStarryBackground(bgFileName));
        delete [] bgFileName;
    }

// add legend
    if(options[OPT_LEGEND])
    {
        result->addChild(addLegend());
    }

#ifndef R3B
    if (options[OPT_REF_PLAN])
      result->addChild(createDisk(diskPosition,diskRadius));
#endif
    if (options[OPT_REF_SPHERE])
      result->addChild(createSphere(spherePosition,sphereRadius));

    return result;
}

///////////////////////////////////////////////////////////////////////////
//
static SoSeparator *
drawALabel(int row, float xoffset, long int label)
//
///////////////////////////////////////////////////////////////////////////
{
    SoSeparator *result = new SoSeparator;
    SoTranslation *labelTranslate = new SoTranslation;
    SoText2 *labelMsg = new SoText2;
    SoFont *labelFont = new SoFont;
    labelFont->name.setValue("Helvetica");
    labelFont->size.setValue(12);

    float xyz[3], firstxyz[3], secondxyz[3];
    normalizeBifData(row, xyz);

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
    normalizeBifData(first, firstxyz);
    normalizeBifData(second, secondxyz);
    if ((secondxyz[1] - firstxyz[1]) * (secondxyz[0] - firstxyz[0]) >= 0)
    {
        xoffset = -xoffset;
        labelMsg->justification = SoText2::RIGHT;
    }
    labelTranslate->translation.setValue(xyz[0] + xoffset, xyz[1] + yoffset,
                                         xyz[2]);
    char a[30];
    sprintf(a, "%ld", label);
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

    if(lbl == MY_ALL)
    {
        int k = 0;
        do
        {
            SoMaterial *lblMtl;
            row = clientData.labelIndex[k][1];
            lbType = clientData.labelIndex[k][2];
    
            lblMtl = setLabelMaterial(lbType);
            result->addChild(lblMtl);
            normalizeBifData(row, position);

            float size = dis*0.005*labelRadius;
            size *= lineWidthScaler;
            result->addChild( drawASphere(position, size));
            if(options[OPT_LABEL_NUMBERS])
                result->addChild( drawALabel(row, size, myBifNode.labels[k]));

            ++k;
        } while( k < clientData.totalLabels);
    }
    else if(lbl != MY_NONE)
    {
        for(int i=0; i<lblIdxSize; ++i)
        {
            SoMaterial *lblMtl;

            int k = lblIndices[i];
            row = clientData.labelIndex[k][1];
            lbType = clientData.labelIndex[k][2];

            lblMtl = setLabelMaterial(lbType);
            result->addChild(lblMtl);

            normalizeBifData(row, position);

	    float size = dis*0.005*labelRadius;
            size *= lineWidthScaler;
            result->addChild( drawASphere(position, size));
            if(options[OPT_LABEL_NUMBERS])
                result->addChild( drawALabel(row, size, myBifNode.labels[k]));
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
long int sumX, float scaler)
//
//
/////////////////////////////////////////////////////////////////
{
    SoSeparator * tSep = new SoSeparator;
    long int upperlimit = myBifNode.numVerticesEachBranch[l];

    if(upperlimit <= 1)
    {
        return tSep;
    }

    float (*path)[3] = new float[upperlimit][3];
    float *colorBase = new float[upperlimit*11];
    Tube tube;

    for(long int i=0; i<upperlimit; i++)
    {
        long int idx = i+sumX;
        normalizeBifData(idx, path[i]);
        if(coloringMethod>=0)
            for(int k=0; k<11; ++k)
                colorBase[i*11+k]  = clientData.bifData[idx*myBifNode.nar +
							coloringMethod];
        else if(coloringMethod==CL_POINT_NUMBER)
            for(int k=0; k<11; ++k)
                colorBase[i*11+k]  = i;
        else if(coloringMethod == CL_STABILITY)
            for(int k=0; k<11; ++k)
                colorBase[i*11+k] = myBifNode.ptStability[idx];
    }

    if(coloringMethod == CL_BRANCH_NUMBER)
    {
#ifdef R3B
        iBranch = myBifNode.branchID[iBranch];
#endif
        tSep->addChild(setLineAttributesByBranch(iBranch, 0, scaler));
    }
    else if(coloringMethod == CL_STABILITY)
        tSep->addChild(setLineColorBlendingByStability(colorBase, upperlimit*11, 0, scaler));
    else /* CL_POINT_NUMBER or >= 0 */
        tSep->addChild(setLineColorBlending(colorBase,
            upperlimit*11, 0, scaler));

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
renderBifurcation()
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator *bifSep = new SoSeparator;
    SoMaterial *bifMtl= new SoMaterial;

    bifMtl->ambientColor.setValue(0.5,0.5,0.5);
    bifMtl->diffuseColor.setValue(0.9,1.0,0.9);
    bifMtl->specularColor.setValue(0.0,0.5,0.5);
    bifMtl->shininess = 0.5;
    bifMtl->transparency = 0.0;

    SoGroup *bifGroup = new SoGroup;

    if(whichStyle == TUBE )
    {
        long int si = 0, k = 0;
        for(int ka=0; ka<myBifNode.numBranches; ka++)
        {
            bifGroup->addChild(drawABifBranchUsingTubes(ka, k, si, 1*lineWidthScaler));
            k = k+1;
            si += myBifNode.numVerticesEachBranch[ka];
        }
    }
    else if (whichStyle == NURBS)
    {
        long int si = 0, k = 0;
        for(int ka=0; ka<myBifNode.numBranches; ka++)
        {
            bifGroup->addChild(drawABifBranchUsingNurbsCurve(ka, k, si, 1*lineWidthScaler));
            k = k+1;
            si += myBifNode.numVerticesEachBranch[ka];
        }

    }
    else 
    {
        long int si = 0, k = 0;
        for(int ka=0; ka<myBifNode.numBranches; ka++)
        {
            bifGroup->addChild(drawABifBranchUsingLines(ka, k, si, 1*lineWidthScaler));
            k = k+1;
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

    long int sumX = 0;
    int iBranch = 0;
    int curBranchID = mySolNode.branchID[iBranch];
    int sumOrbit    = mySolNode.numOrbitsInEachBranch[iBranch];

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

                int stability=clientData.labelIndex[j][3];
                int type =clientData.labelIndex[j][2];
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

                float ver[2][3];
        
                if(time_on != TIME_IS_OFF)
                {
		    int32_t myint[10];
                    if(time_on == TIME_ON_X)
                    {
                        ver[0][0] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0;
			//-(mySolNode.max[0]-mySolNode.min[0]) : 0;
                        ver[0][1] = mySolNode.xyzCoords[idx][1];
                        ver[0][2] = 0;
                        ver[1][0] = (mySolNode.max[0] <= 1.0) ? 1.0 : mySolNode.max[0];
                        ver[1][1] = mySolNode.xyzCoords[idx][1];
                        ver[1][2] = 0;
                    }
                    else if(time_on == TIME_ON_Y)
                    {
                        ver[0][0] = mySolNode.xyzCoords[idx][0];
                        ver[0][1] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0;
			//-(mySolNode.max[1]-mySolNode.min[1]) : 0;
                        ver[0][2] = 0;
                        ver[1][0] = mySolNode.xyzCoords[idx][0]; 
                        ver[1][1] = ( mySolNode.max[1] <= 1.0 ) ? 1.0 : mySolNode.max[1];
                        ver[1][2] = 0;
                    }
                    else if(time_on == TIME_ON_Z)
                    {
                        ver[0][0] = mySolNode.xyzCoords[idx][0];
                        ver[0][1] = 0; 
                        ver[0][2] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0; 
			//-(mySolNode.max[2]-mySolNode.min[2]) : 0;
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
                }
                else
                {
                    SoTransform * aTrans = new SoTransform;
                    aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], 
        		           mySolNode.xyzCoords[idx][1], mySolNode.xyzCoords[idx][2]);
                    ptSep->addChild(aTrans);
                    SoSphere *aPoint = new SoSphere;
                    aPoint->radius = dis * STATIONARY_POINT_RADIUS;
                    ptSep->addChild(aPoint);
                }
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
                int stability=clientData.labelIndex[j][3];
                int type =clientData.labelIndex[j][2];
                float scaler = lineWidthScaler;

		if(coloringMethod == CL_BRANCH_NUMBER)
                {
#ifdef R3B
                    tubeSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
#else
                    tubeSep->addChild(setLineAttributesByBranch(mySolNode.branchID[iBranch], stability, scaler));
#endif
                }
                else if(coloringMethod == CL_STABILITY)
                    tubeSep->addChild(setLineAttributesByStability(stability, scaler));
                else if(coloringMethod == CL_ORBIT_TYPE)
                    tubeSep->addChild(setLineAttributesByType(stability, type, scaler));
                else if(coloringMethod == CL_LABELS)
                    tubeSep->addChild(setLineAttributesByParameterValue(
                            j, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                            stability, scaler));
                else if(coloringMethod == CL_COMPONENT)
                    tubeSep->addChild(setLineAttributesByParameterValue(
                            curComponent, maxComponent, maxComponent/2.0, 0,
                            stability, scaler));
                else if(coloringMethod >= mySolNode.nar)
                    tubeSep->addChild(setLineAttributesByParameterValue(
                          mySolNode.par[j][mySolNode.parID[coloringMethod-mySolNode.nar]],
                          mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                          mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                          mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                          stability, scaler));
                else
                    tubeSep->addChild(setLineColorBlending(colorBase,
                        upperlimit*11,stability, scaler));

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
	    printf(" Only one point in the period, no surface can be drawn!\n");
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

// draw every orbit by using giving tube thickness and color.
    if(animationLabel == MY_ALL)
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
                if(myLabels[ka]>=animationLabel) break;
                k = k+1;
                si += mySolNode.numVerticesEachPeriod[ka];
            }

            if(options[OPT_SAT_ANI])
            {
                solGroup->addChild(animateOrbitWithTail(iBranch, k, si));
            }
            else
            {
                solGroup->addChild(drawAnOrbitUsingTubes(iBranch, k, si, 1*lineWidthScaler,
                 clientData.labelIndex[k][3], clientData.labelIndex[k][2]));
            }
        }
    }

    if(options[OPT_PERIOD_ANI])
    {
        bool aniColoring;
        aniColoring = (animationLabel == MY_ALL) ? false : true;
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
    solMtl->diffuseColor.setValue(envColors[4]);
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
        if(myLabels[ka]>=animationLabel) break;
        k = k+1;
        si += mySolNode.numVerticesEachPeriod[ka];
    }

    if(options[OPT_SAT_ANI])
        solGroup->addChild(animateOrbitWithTail(iBranch, k, si));
    else
        solGroup->addChild(drawAnOrbitUsingTubes(iBranch, k, si, 1*lineWidthScaler,
            clientData.labelIndex[k][3], clientData.labelIndex[k][2]));
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

    if(animationLabel == MY_ALL)
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

            solGroup->addChild(drawAnOrbitUsingPoints(style, iBranch, /*curBranchID,*/ k, si, lineWidthScaler,
                clientData.labelIndex[k][3], clientData.labelIndex[k][2], true));
            k = k+1;
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

                if(myLabels[ka]>=animationLabel) break;
                k = k+1;
                si += mySolNode.numVerticesEachPeriod[ka];
            }
            if(options[OPT_SAT_ANI])
            {
                solGroup->addChild(animateOrbitWithTail(iBranch, k, si));
            }
            else
            {
                solGroup->addChild(drawAnOrbitUsingPoints(style, iBranch, k, si, lineWidthScaler,
                    clientData.labelIndex[k][3], clientData.labelIndex[k][2], true));
            }
        }
    }

    if(options[OPT_PERIOD_ANI])
    {
        bool coloring;
        coloring = (animationLabel == MY_ALL) ? false : true;
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

    if(animationLabel == MY_ALL)
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

            solGroup->addChild(drawAnOrbitUsingLines(iBranch, k, si, lineWidthScaler,
                clientData.labelIndex[k][3], clientData.labelIndex[k][2], true));
            k = k+1;
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

                if(myLabels[ka]>=animationLabel) break;
                k = k+1;
                si += mySolNode.numVerticesEachPeriod[ka];
            }

            if(options[OPT_SAT_ANI])
            {
                solGroup->addChild(animateOrbitWithTail(iBranch, k, si));
            }
            else
            {
                solGroup->addChild(drawAnOrbitUsingLines(iBranch, k, si, lineWidthScaler,
                    clientData.labelIndex[k][3], clientData.labelIndex[k][2], true));
            }
        }
    }

    if(options[OPT_PERIOD_ANI])
    {
        bool coloring;
        coloring = (animationLabel == MY_ALL) ? false : true;
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

    if(animationLabel == MY_ALL)
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
            solGroup->addChild(drawAnOrbitUsingNurbsCurve(iBranch, k+1, si, lineWidthScaler,
                clientData.labelIndex[k][3], clientData.labelIndex[k][2]));
            k = k+1;
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
                if(myLabels[ka]>=animationLabel) break;
                k = k+1;
                si += mySolNode.numVerticesEachPeriod[ka];
            }

            if(options[OPT_SAT_ANI])
            {
                solGroup->addChild(animateOrbitWithTail(iBranch, k, si));
            }
            else
            {
                solGroup->addChild(drawAnOrbitUsingNurbsCurve(iBranch, k, si, lineWidthScaler,
                    clientData.labelIndex[k][3], clientData.labelIndex[k][2]));
            }
        }
    }

    if(options[OPT_PERIOD_ANI])
    {
        bool coloring = (animationLabel == MY_ALL) ? true : false;
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
            solStand->addChild(drawAnOrbitUsingPoints(style, iBranch,/* curBranchID,*/ l, si, aniLineScaler*lineWidthScaler,
                clientData.labelIndex[l][3], clientData.labelIndex[l][2], aniColoring));
        else
            solBlinker->addChild(drawAnOrbitUsingPoints(style, iBranch,/* curBranchID,*/ l, si, aniLineScaler*lineWidthScaler,
                clientData.labelIndex[l][3], clientData.labelIndex[l][2], aniColoring));
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
            solStand->addChild(drawAnOrbitUsingLines(iBranch, l, si, aniLineScaler*lineWidthScaler,
                clientData.labelIndex[l][3], clientData.labelIndex[l][2], aniColoring));
        else
            solBlinker->addChild(drawAnOrbitUsingLines(iBranch, l, si, aniLineScaler*lineWidthScaler,
                clientData.labelIndex[l][3], clientData.labelIndex[l][2], aniColoring));
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
        solBlinker->addChild(drawAnOrbitUsingNurbsCurve(iBranch, l, si, lineWidthScaler,
            clientData.labelIndex[l][3], clientData.labelIndex[l][2]));
        si+=mySolNode.numVerticesEachPeriod[l];
    }
    solGroup->addChild(solBlinker);
    return solGroup;
}


/////////////////////////////////////////////////////////////////
//                  create solution orbits scene
SoSeparator *
renderSolution()
//
/////////////////////////////////////////////////////////////////
{
    SoSeparator *solSep = new SoSeparator;
    SoGroup *solGroup = new SoGroup;

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


//////////////////////////////// START ///////////////////////////////
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
    static float maxColor[3];
    static float minColor[3]; 

    for(int i=0; i<3; ++i)
    {
        maxColor[i] = envColors[5][i];
        minColor[i] = envColors[6][i];
    }

    for(int i=0; i<size; ++i)
    {
        if(vertices[i]== 1 || vertices[i]== 3)
            for(int j=0; j<3; ++j) colors[i][j] = maxColor[j];
        else
            for(int j=0; j<3; ++j) colors[i][j] = minColor[j];
    }

    SoGroup * result = new SoGroup ;

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
        lineMtl->diffuseColor.setValue(envColors[5]);
    }
    else
    {
        lineStyle->linePattern = stabilityLinePattern[1];
        lineMtl->diffuseColor.setValue(envColors[6]);
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
setLineColorBlending(float * vertices, long int size, int stability, float scaler)
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
        (coloringMethod != CL_POINT_NUMBER && 
         (animationLabel == MY_ALL || options[OPT_PERIOD_ANI]) ))
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

    float dis = !options[OPT_NORMALIZE_DATA] ? 
	               (max(max(fabs(mySolNode.max[0]-mySolNode.min[0]),
        fabs(mySolNode.max[1]-mySolNode.min[1])),
        fabs(mySolNode.max[2]-mySolNode.min[2]))) : 2.0;

    long numVertices = mySolNode.numVerticesEachPeriod[l];
    if(numVertices == 1 )
    {
        long int idx = si;
        SoSeparator * ptSep = new SoSeparator;

        if(coloringMethod == CL_BRANCH_NUMBER)
            ptSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
        else if(coloringMethod == CL_STABILITY)
            ptSep->addChild(setLineAttributesByStability(stability, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            ptSep->addChild(setLineAttributesByType(stability, type, scaler));
        else if(coloringMethod == CL_LABELS)
        {
            ptSep->addChild(setLineAttributesByParameterValue(
                l, clientData.totalLabels, clientData.totalLabels/2.0, 0,
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

        
        float ver[2][3];

        if(time_on != TIME_IS_OFF)
        {
	    int32_t myint[10];
            if(time_on == TIME_ON_X)
            {
                ver[0][0] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0;
		//-(mySolNode.max[0]-mySolNode.min[0]) : 0;
                ver[0][1] = mySolNode.xyzCoords[idx][1];
                ver[0][2] = 0;
                ver[1][0] = (mySolNode.max[0] <= 1.0) ? 1.0 : mySolNode.max[0];
                ver[1][1] = mySolNode.xyzCoords[idx][1];
                ver[1][2] = 0;
            }
            else if(time_on == TIME_ON_Y)
            {
                ver[0][0] = mySolNode.xyzCoords[idx][0];
                ver[0][1] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0;
		//-(mySolNode.max[1]-mySolNode.min[1]) : 0;
                ver[0][2] = 0;
                ver[1][0] = mySolNode.xyzCoords[idx][0]; 
                ver[1][1] = ( mySolNode.max[1] <= 1.0 ) ? 1.0 : mySolNode.max[1];
                ver[1][2] = 0;
            }
            else if(time_on == TIME_ON_Z)
            {
                ver[0][0] = mySolNode.xyzCoords[idx][0];
                ver[0][1] = 0; 
                ver[0][2] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0;
		//-(mySolNode.max[2]-mySolNode.min[2]) : 0; 
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
            aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], 
		           mySolNode.xyzCoords[idx][1], mySolNode.xyzCoords[idx][2]);
            ptSep->addChild(aTrans);
            SoSphere *aPoint = new SoSphere;
            aPoint->radius = dis * STATIONARY_POINT_RADIUS;
            ptSep->addChild(aPoint);
            anOrbit->addChild(ptSep);
        }
        return anOrbit;
    }

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
    myCoords->point.setValues(0, mySolNode.numVerticesEachPeriod[l], vertices);
    int32_t  myint[10];
    myint[0]=mySolNode.numVerticesEachPeriod[l];

// define the solution line set
    SoLineSet *myLine= new SoLineSet;
    myLine->numVertices.setValues(0,1,myint);
    if(!aniColoring)
    {
        anOrbit->addChild(setLineAttributesByType(stability, 0, scaler));
    }
    else if(coloringMethod == CL_BRANCH_NUMBER)
    {
#ifdef R3B
        iBranch = mySolNode.branchID[iBranch];
#endif
        anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
    }
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
    else if(coloringMethod == CL_LABELS)
    {
        anOrbit->addChild(setLineAttributesByParameterValue(
                          l, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                          stability, scaler));
    }
    else if(coloringMethod == CL_COMPONENT)
        anOrbit->addChild(setLineAttributesByParameterValue(
                          curComponent, maxComponent, maxComponent/2.0, 0,
                          stability, scaler));
    else if(coloringMethod >= mySolNode.nar)
        anOrbit->addChild(setLineAttributesByParameterValue(
                          mySolNode.par[l][mySolNode.parID[coloringMethod-mySolNode.nar]],
                          mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                          mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                          mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                          stability, scaler));
    else
        anOrbit->addChild(setLineColorBlending(colorBase,
                          mySolNode.numVerticesEachPeriod[l],stability, scaler));

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

    long numVertices = mySolNode.numVerticesEachPeriod[l];
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
                l, clientData.totalLabels, clientData.totalLabels/2.0, 0,
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

        if(!aniColoring)
        {
            anOrbit->addChild(setLineAttributesByType(stability, 0, scaler));
        }
        else if(coloringMethod == CL_BRANCH_NUMBER)
            anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
        else if(coloringMethod == CL_STABILITY)
            anOrbit->addChild(setLineAttributesByStability(stability, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
        else if(coloringMethod == CL_LABELS)
        {
            anOrbit->addChild(setLineAttributesByParameterValue(
                              l, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                              stability, scaler));
        }
        else if(coloringMethod == CL_COMPONENT)
            anOrbit->addChild(setLineAttributesByParameterValue(
                              curComponent, maxComponent, maxComponent/2.0, 0,
                              stability, scaler));
        else if(coloringMethod >= mySolNode.nar)
        {
            anOrbit->addChild(setLineAttributesByParameterValue(
                              mySolNode.par[l][mySolNode.parID[coloringMethod-mySolNode.nar]],
                              mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                              mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                              mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                              stability, scaler));
        }
        else
            anOrbit->addChild(setLineColorBlending(colorBase,
                              mySolNode.numVerticesEachPeriod[l],stability, scaler));

        if(style == MESH_POINTS)
        {
            if(m%mySolNode.ncol[l] == 0)
                anOrbit->addChild(drawAPoint(mySolNode.xyzCoords[idx][0], mySolNode.xyzCoords[idx][1],
                    mySolNode.xyzCoords[idx][2], dis, STATIONARY_POINT_RADIUS*0.5));
        }else
        anOrbit->addChild(drawAPoint(mySolNode.xyzCoords[idx][0], mySolNode.xyzCoords[idx][1],
                mySolNode.xyzCoords[idx][2], dis, STATIONARY_POINT_RADIUS*0.5));
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
    vertices = new float[mySolNode.numVerticesEachPeriod[l]][3];
    for(int m=0; m<mySolNode.numVerticesEachPeriod[l]; m++)
    {
        vertices[m][0]=mySolNode.xyzCoords[si+m][0];
        vertices[m][1]=mySolNode.xyzCoords[si+m][1];
        vertices[m][2]=mySolNode.xyzCoords[si+m][2];
    }
    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, mySolNode.numVerticesEachPeriod[l], vertices);
    myint[0]=mySolNode.numVerticesEachPeriod[l];

    int number = mySolNode.numVerticesEachPeriod[l];
    float * knots = new float[number+4];
    for (int i=0; i<4; ++i) knots[i]=0, knots[i+number]=number-3;
    for(int i=4; i<number; ++i) knots[i]=i-3;
    SoNurbsCurve *myCurve = new SoNurbsCurve;
    myCurve->numControlPoints = mySolNode.numVerticesEachPeriod[l];
    myCurve->knotVector.setValues(0, number+4, knots);

    if(coloringMethod == CL_BRANCH_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch,stability,scaler));
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_LABELS)
    {
        anOrbit->addChild(setLineAttributesByParameterValue(
           l, clientData.totalLabels, clientData.totalLabels/2.0, 0,
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
    float dis = !options[OPT_NORMALIZE_DATA] ? (max(max(fabs(mySolNode.max[0]-mySolNode.min[0]),
        fabs(mySolNode.max[1]-mySolNode.min[1])),
        fabs(mySolNode.max[2]-mySolNode.min[2]))) : 2.0 ;

    SoSeparator * anOrbit = new SoSeparator;
    long int numVertices = mySolNode.numVerticesEachPeriod[l];
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
                l, clientData.totalLabels, clientData.totalLabels/2.0, 0,
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
            int32_t  myint[10];
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
            aTrans->translation.setValue(mySolNode.xyzCoords[idx][0], 
		           mySolNode.xyzCoords[idx][1], mySolNode.xyzCoords[idx][2]);
            ptSep->addChild(aTrans);
            SoSphere *aPoint = new SoSphere;
            aPoint->radius = dis * STATIONARY_POINT_RADIUS;
            ptSep->addChild(aPoint);
            anOrbit->addChild(ptSep);
        }
        return anOrbit;
    }
    else if( numVertices < 1 )
    {
        return anOrbit;
    }

    float (*vertices)[3] = new float[mySolNode.numVerticesEachPeriod[l]][3];
    float *colorBase = new float[mySolNode.numVerticesEachPeriod[l]*11];
    Tube tube;
    for(int m=0; m<mySolNode.numVerticesEachPeriod[l]; m++)
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
    tube = Tube(mySolNode.numVerticesEachPeriod[l], vertices, lineWidthScaler*0.005, 10);

    if(coloringMethod == CL_BRANCH_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_BRANCH_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
    else if(coloringMethod == CL_LABELS)
//          always set the first label blue, the last red, namely look all
//          branches as one.
        anOrbit->addChild(setLineAttributesByParameterValue(
                 l, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                 stability, scaler));
    else if(coloringMethod == CL_COMPONENT)
        anOrbit->addChild(setLineAttributesByParameterValue(
                curComponent, maxComponent, maxComponent/2.0, 0,
                stability, scaler));
    else if(coloringMethod >= mySolNode.nar)
        anOrbit->addChild(setLineAttributesByParameterValue(
                    mySolNode.par[l][mySolNode.parID[coloringMethod-mySolNode.nar]],
                    mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                    mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                    mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                    stability, scaler));
    else
        anOrbit->addChild(setLineColorBlending(colorBase,
                mySolNode.numVerticesEachPeriod[l]*11,stability, scaler));

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
drawABifBranchUsingLines(int iBranch, long int l, long int si, float scaler)
//
//////////////////////////////////////////////////////////////////////////
{
    int32_t  myint[10];

    SoSeparator * aBranch = new SoSeparator;
    long int size         = myBifNode.numVerticesEachBranch[l];
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
        normalizeBifData(idx, vertices[curSize]);
        if(coloringMethod >= 0)
            colorBase[curSize]  = clientData.bifData[idx * myBifNode.nar +
						     coloringMethod];
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
        {
#ifdef R3B
            iBranch = myBifNode.branchID[iBranch];
#endif
            aBranch->addChild(setLineAttributesByBranch(iBranch, lastStab, scaler));
        }
        else if(coloringMethod == CL_STABILITY)
            aBranch->addChild(setLineColorBlendingByStability(colorBase, curSize, lastStab, scaler));
        else /* CL_POINT_NUMBER or >= 0 */
            aBranch->addChild(setLineColorBlending(colorBase, curSize,
                lastStab, scaler));

        aBranch->addChild(myCoords);
        aBranch->addChild(myLine);

        curSize = 0;
        normalizeBifData(idx-1, vertices[curSize]);
        if(coloringMethod >= 0)
            colorBase[curSize]  = clientData.bifData[(idx-1) * myBifNode.nar +
						     coloringMethod];
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
long int si, float scaler)
//
//////////////////////////////////////////////////////////////////////////
{
    int32_t  myint[10];

    SoSeparator * aBranch = new SoSeparator;
    long int size = myBifNode.numVerticesEachBranch[l];
    float (*vertices)[3] = new float[size][3];
    float *colorBase = new float[size];
    for(long int m=0; m<size; ++m) 
    {
        long idx = si+m;
        normalizeBifData(idx, vertices[m]);
        if(coloringMethod>=0)
            colorBase[m]  =clientData.bifData[idx * myBifNode.nar + 
					      coloringMethod];
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
        aBranch->addChild(setLineAttributesByBranch(iBranch, 0, scaler));
    else if(coloringMethod == CL_STABILITY)
        aBranch->addChild(setLineAttributesByStability(0, scaler));
    else /* CL_POINT_NUMBER or >= 0 */
        aBranch->addChild(setLineColorBlending(colorBase, size,
            0, scaler));
    aBranch->addChild(myCoords);
    aBranch->addChild(cvGrp);

    delete [] vertices;
    delete [] colorBase;
    delete [] knots;
    return aBranch;
}

#if 0
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
    long int size = myBifNode.numVerticesEachLabelInterval[l];
    float *colorBase = new float[size];
    float (*vertices)[3] = new float[size][3];
    for(int m=0; m<size; m++)
    {
        long int idx = si+m;
        normalizeBifData(idx, vertices[m]);
        if(coloringMethod>=0)
            colorBase[m]  =clientData.bifData[idx * myBifNode.nar +
					      coloringMethod];
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
            stability, scaler));

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
    vertices = new float[myBifNode.numVerticesEachLabelInterval[l]][3];
    for(int m=0; m<myBifNode.numVerticesEachLabelInterval[l]; m++)
    {
        vertices[m][0]=myBifNode.xyzCoords[si+m][0];
        vertices[m][1]=myBifNode.xyzCoords[si+m][1];
        vertices[m][2]=myBifNode.xyzCoords[si+m][2];
    }
    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, myBifNode.numVerticesEachLabelInterval[l], vertices);
    myint[0]=myBifNode.numVerticesEachLabelInterval[l];

    SoGroup *cvGrp = new SoGroup;

    int number = myBifNode.numVerticesEachLabelInterval[l];
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
            aTrans->translation.setValue(mySolNode.xyzCoords[idx][0],
                                         mySolNode.xyzCoords[idx][1],
                                         mySolNode.xyzCoords[idx][2]);
            ptSep->addChild(aTrans);
            ptSep->addChild(aPoint);
            solGroup->addChild(ptSep);
        }
        else
        {
            float (*path)[3] = new float[upperlimit][3];
            float *colorBase = new float[upperlimit*11];
            int stability=clientData.labelIndex[j][3];
            int type =clientData.labelIndex[j][2];
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
                anOrbit->addChild(setLineAttributesByParameterValue(
                    j, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                    stability, lineWidthScaler));
            else if(coloringMethod >= mySolNode.nar)
                anOrbit->addChild(setLineAttributesByParameterValue(
                        mySolNode.par[j][mySolNode.parID[coloringMethod-mySolNode.nar]],
                        mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                        mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                        mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                        stability, lineWidthScaler));
            else if(coloringMethod == CL_COMPONENT)
                 anOrbit->addChild(setLineAttributesByParameterValue(
                     curComponent, maxComponent, maxComponent/2.0, 0,
                     stability, lineWidthScaler));
            else
                anOrbit->addChild(setLineColorBlending(colorBase,
                    upperlimit*11,stability, lineWidthScaler));

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


#if 0
///////////////////////////////////////////////////////////////////////////
//
SoSeparator *
drawATube(TubeNode cnode)
//
///////////////////////////////////////////////////////////////////////////
{
    SoSeparator *tSep = new SoSeparator;
    SoTransform *tXform = new SoTransform;
    tXform->translation.setValue(cnode.translation.x,
        cnode.translation.y,
        cnode.translation.z);
    tXform->rotation.setValue(SbVec3f(cnode.axis.x,
        cnode.axis.y,
        cnode.axis.z),
        cnode.angle);
    SoCylinder *tCyl = new SoCylinder;
    tCyl->radius = 0.005;
    tCyl->height = cnode.height;
    tCyl->parts = SoCylinder::SIDES;

    tSep->addChild(tXform);
    tSep->addChild(tCyl);
    return tSep;
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
    SoSeparator *satGroup = new SoSeparator;

    float distance = !options[OPT_NORMALIZE_DATA] ? (max(max(fabs(mySolNode.max[0]-mySolNode.min[0]),
        fabs(mySolNode.max[1]-mySolNode.min[1])),
        fabs(mySolNode.max[2]-mySolNode.min[2]))) : 2.0;
    int stability = clientData.labelIndex[j][3];
    int type = clientData.labelIndex[j][2];

    long int upperlimit = mySolNode.numVerticesEachPeriod[j];
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
    long int arrSize = orbitSize;

    double *time = new double[upperlimit+1];
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

    float dis = distance;// fabs(max(max((maxV[0]-minV[0]), (maxV[1]-minV[1])), (maxV[2]-minV[2])));
    float (*myVertices)[3]= new float[arrSize+1][3];
    float *myColorBase = new float [arrSize+1];

    myVertices[0][0] = myVertices[arrSize][0] = mySolNode.xyzCoords[idx][0];
    myVertices[0][1] = myVertices[arrSize][1] = mySolNode.xyzCoords[idx][1];
    myVertices[0][2] = myVertices[arrSize][2] = mySolNode.xyzCoords[idx][2];
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

    SoDrawStyle *satStyle = new SoDrawStyle;
    satStyle->style = SoDrawStyle::FILLED;
    satGroup->addChild(satStyle);

    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, arrSize+1, myVertices);
    satGroup->addChild(myCoords);

    
    SoTimeCounter *myCounter = new SoTimeCounter;
    myCounter->max = arrSize+1;
    myCounter->min = 0;
   
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

//------------------------------------------Begin-----------------------------------------
    float scaler = lineWidthScaler;

//------------------------------------------End-----------------------------------------
    if(coloringMethod == CL_BRANCH_NUMBER)
        satGroup->addChild(setLineAttributesByBranch(iBranch, stability, lineWidthScaler));
    else if(coloringMethod == CL_STABILITY)
        satGroup->addChild(setLineAttributesByStability(stability, lineWidthScaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        satGroup->addChild(setLineAttributesByType(stability, type, lineWidthScaler));
    else if(coloringMethod == CL_LABELS)
        satGroup->addChild(setLineAttributesByParameterValue(
                j-1, clientData.totalLabels, clientData.totalLabels/2.0, 0,
                stability, lineWidthScaler));
    else if(coloringMethod == CL_COMPONENT)
        satGroup->addChild(setLineAttributesByParameterValue(
                curComponent, maxComponent, maxComponent/2.0, 0,
                stability, scaler));
    else if(coloringMethod >= mySolNode.nar)
        satGroup->addChild(setLineAttributesByParameterValue(
                mySolNode.par[j][mySolNode.parID[coloringMethod-mySolNode.nar]],
                mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                stability, lineWidthScaler));
    else
        satGroup->addChild(setLineColorBlending(myColorBase, arrSize,
                                                stability, lineWidthScaler));

// define the solution line set
    SoLineSet *myLine= new SoLineSet;
    myLine->numVertices.connectFrom(&myCounter->output);
    satGroup->addChild(myLine);

    SoSphere * mySat = new SoSphere;
    mySat->radius = satRadius*dis*0.005;


    SoTranslation * satTrans = new SoTranslation;
    SoMaterial * satMtl = new SoMaterial;
    satMtl->diffuseColor.setValue(envColors[7]);
    satGroup->addChild(satMtl);
    satGroup->addChild(satTrans);
    satGroup->addChild(mySat);

    SoTimeCounter *myCounter2 = new SoTimeCounter;
    myCounter2->max = arrSize;
    myCounter2->min = 0;
    if (numPeriodAnimated != 0)
        myCounter2->frequency =  0.1*satSpeed;//*numPeriodAnimated;
    else
        myCounter2->frequency =  0.1*satSpeed;
    for(iduty = 0; iduty < arrSize; ++iduty)
         myCounter2->duty.set1Value(iduty, 1) ;

    SoSelectOne *mysel = new SoSelectOne(SoMFVec3f::getClassTypeId());
    mysel->index.connectFrom(&myCounter2->output);
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


#if 0
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
#endif


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
    bool blOpenSolFile, blOpenBifFile;

    solHead = parseSolution(sFileName, blOpenSolFile, total, rows);
    if(!blOpenSolFile)
        printf(" Solution file does not exist.\n");

    mySolNode.time = new double[mySolNode.totalNumPoints];
    mySolNode.xyzCoords = new float[mySolNode.totalNumPoints][3];
    mySolNode.numVerticesEachBranch = new int32_t[mySolNode.numBranches];
    mySolNode.numOrbitsInEachBranch = new int32_t[mySolNode.numBranches];
    mySolNode.branchID = new long[mySolNode.numBranches];
    mySolNode.parMax = new double[mySolNode.numBranches][MAX_PAR];
    mySolNode.parMin = new double[mySolNode.numBranches][MAX_PAR];
    mySolNode.parMid = new double[mySolNode.numBranches][MAX_PAR];
    mySolNode.numAxis   = 3;

    clientData.solMax = new float [mySolNode.nar+1];
    clientData.solMin = new float [mySolNode.nar+1];

    clientData.solData = new float*[mySolNode.totalNumPoints];
    if (mySolNode.totalNumPoints > 0) {
        clientData.solData[0] = new float[mySolNode.totalNumPoints*mySolNode.nar];
	for(int ml=1; ml<mySolNode.totalNumPoints; ++ml)
	    clientData.solData[ml] = &clientData.solData[0][ml*mySolNode.nar];
    }

    blOpenBifFile = parseBifurcation(bFileName) ? true : false;
    if(!blOpenBifFile) printf(" Bifurcation file does not exist!\n");

    if((!blOpenBifFile) && (!blOpenSolFile) && (!blFirstRead)) return false;
    else if((!blOpenBifFile) && (!blOpenSolFile))
    {
//        printf(" Target files do not exist!\n");
        exit(1);
    }

    myBifNode.ptStability = new unsigned char[myBifNode.totalNumPoints];
    myBifNode.numVerticesEachBranch = new int32_t[myBifNode.numBranches];
    myBifNode.branchID = new long[myBifNode.numBranches];

    clientData.bifData = new float[myBifNode.totalNumPoints*myBifNode.nar];

    int varIndices[3];

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

    if( mySolNode.numOrbits > 0 )
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

    if((!blOpenBifFile) && (!blOpenSolFile))
    {
        exit(1);
    }

    int st = readFM(dFileName, myBifNode.totalNumPoints);
    if(st!=0)
        printf(" Failed to read the diagnostic file.\n");

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
    for(int k=0; k<3; k++)
    {
        for(long int row=0; row<mySolNode.totalNumPoints; ++row)
        {
            mySolNode.time[row] = clientData.solData[row][0];
            if(varIndices[k]>=0)
            {
                float dummy = clientData.solData[row][varIndices[k]];
                mySolNode.xyzCoords[row][k] = dummy;
            }
            else if(varIndices[k]<0)
            {
                mySolNode.xyzCoords[row][k]=0.0;
            }
        }
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
        myBifNode.varIndices[k] = varIndices[k];
        for(long int row=0; row<myBifNode.totalNumPoints; ++row)
        {
            if(varIndices[k]>=0)
            {
                float dummy = clientData.bifData[row*myBifNode.nar + 
						 varIndices[k]];

                if(dummy>myBifNode.max[k] || row==0 )
                    myBifNode.max[k] = dummy;
                if(dummy<myBifNode.min[k] || row==0 )
                    myBifNode.min[k] = dummy;
            }
            else if(varIndices[k]<0)
            {
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


////////////////////////////////////////////////////////////////////////
//
void
lookForThePoint(float position[],long int &bIdx, long int &sIdx)
//
////////////////////////////////////////////////////////////////////////
{

    int varIndices[3];
    int mx = max(max(xCoordIdxSize, yCoordIdxSize), max(yCoordIdxSize, zCoordIdxSize));
    float minDis = FLT_MAX;
    long int index = 0;
    long int ib = 0;
    float distance;
    float *data;
    sIdx = bIdx = 0;
    for(int i=0; i<mx; i++)
    {
        varIndices[0]=xCoordIndices[(i>=xCoordIdxSize)?(i%xCoordIdxSize):(i)];
        varIndices[1]=yCoordIndices[(i>=yCoordIdxSize)?(i%yCoordIdxSize):(i)];
        varIndices[2]=zCoordIndices[(i>=zCoordIdxSize)?(i%zCoordIdxSize):(i)];
        long int lblidx = lblIndices[0];
        long int maxp, sumup = 0;
        if(whichType == BIFURCATION)
        {
            maxp = myBifNode.totalNumPoints;
        }
        else 
        {
	    animationLabel = myLabels[lblIndices[0]];
            if(animationLabel == MY_ALL || lblIdxSize >1)
                maxp = mySolNode.totalNumPoints;
            else
            {
                for(int j=0; j<lblidx; ++j)
                    sumup += mySolNode.numVerticesEachPeriod[j];
                maxp = mySolNode.numVerticesEachPeriod[lblidx];
            }
        }
        for(long int j=0; j<maxp; ++j)
	{
            if(whichType == BIFURCATION)
                data = &clientData.bifData[j*myBifNode.nar];
            else
                data = clientData.solData[sumup+j];
            distance = 0;
            for(int k=0; k<3; ++k)
                if(varIndices[k] != -1) 
                {
                    float diff = position[k]-data[varIndices[k]];
                    distance += diff * diff;
                }
            if(minDis > distance)
            {
                minDis = distance;
                index  = j+sumup;
            }
        }
        if(whichType != BIFURCATION)
        {
            if(animationLabel == MY_ALL || lblIdxSize >1)
            {
                for (ib = 0; ib < clientData.totalLabels; ib++)
                {
                    sumup += mySolNode.numVerticesEachPeriod[ib];
                    if (sumup > index) break;
                }
            }
            else
            {
                ib = lblidx;  
            }
            sIdx = index;
            bIdx = clientData.labelIndex[ib][1];
        }
        else
        {
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

    float *data;
    if(whichType != BIFURCATION)
    {
        if(sIdx > mySolNode.totalNumPoints || sIdx < 0)
            return false;
        size = mySolNode.nar;
        data = clientData.solData[sIdx];
    }
    else {
        if (bIdx > myBifNode.totalNumPoints || bIdx < 0)
            return false;
        size = myBifNode.nar;
        data = &clientData.bifData[bIdx * myBifNode.nar];
    }

    for(int ms=0; ms<abs(clientData.numFM[bIdx]); ++ms)
    {
        fmData[2*ms]   = clientData.multipliers[bIdx*clientData.maxndim+ms][0];
        fmData[2*ms+1] = clientData.multipliers[bIdx*clientData.maxndim+ms][1];
    }

    popupFloquetMultiplierDialog(data, size, clientData.numFM[bIdx]);
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
            sprintf(coloringMethodList[i], "PAR(%d)", mySolNode.parID[i-(mySolNode.nar+sp)]+1);
        }
    }
    else
    {
        for(i=0; i<MAX_LIST; i++)
            sprintf(coloringMethodList[i+sp], "%d", i);
    }
    specialColorItems = sp;

    if(mySolNode.numOrbits > 0)
    {
// the solution file does exist.
        numLabels = mySolNode.numOrbits;
        for(int j=0; j<numLabels; j++) myLabels[j] = mySolNode.labels[j];
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
        for(int j=0; j<numLabels; j++) myLabels[j] = myBifNode.labels[j];
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
    myLabels[numLabels + MY_NONE] = MY_NONE; // -1
    myLabels[numLabels + MY_SPEC] = MY_SPEC; // -2
    myLabels[numLabels + MY_HALF] = MY_HALF; // -3
    myLabels[numLabels + MY_ALL] = MY_ALL; // -4
    for( i=0; i<numLabels; ++i)
    {
        int jmii = i + SP_LBL_ITEMS;
        sprintf(labels[jmii], "%d", myLabels[i]);
        switch (clientData.labelIndex[i][2])
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
    if( lblChoice[0] == MY_ALL) // -4
    {
        lblIndices[0] = numLabels + MY_ALL;
        half = 2;
        iLbl = lblIdxSize;
    }
    else if(lblChoice[0] == MY_HALF)  // -3
    {
        for(int j = 0; j < numLabels - SP_LBL_ITEMS; j++)
        {
            if(abs(clientData.labelIndex[j][2])!= 4 || (j+1)%half == 0)
                lblIndices[iLbl++] = j;
        }

        half *= 2;
    }
    else if(lblChoice[0] == MY_SPEC) // -2
    {
        for(int j = 0; j < numLabels - SP_LBL_ITEMS; j++)
        {
            if(clientData.labelIndex[j][2] !=  TYPE_UZ    && clientData.labelIndex[j][2] != TYPE_RG &&
               clientData.labelIndex[j][2] != TYPE_EP_ODE && clientData.labelIndex[j][2] != TYPE_MX)
                lblIndices[iLbl++] = j;
        }
        half = 2;
    }
    else if(lblChoice[0] == MY_NONE)  // -1
    {
        lblIndices[iLbl++] = numLabels + MY_NONE;
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
double
fortranatof(char* word)
//
///////////////////////////////////////////////////////////////////
{
    // check for numbers such as 2.8430486351-315
    char *p;
    size_t len = strlen(word);
    double d = strtod(word, &p);
    if (len >= 6 && p == word+len-4 &&
	(*p == '-' || *p == '+' || *p++ == 'D')) {
        p[-2] = '1';
        p[-1] = 'E';
        d*=atof(p-2);
    }
    return d;
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
    float aVector[3];
    unsigned long aHex;
    char aString[256], *strTemp;
    char resource[256];

    FILE * inFile;

    strcpy(resource, autoDir);
#ifndef R3B
    strcat(resource,"/plaut04/plaut04.rc");
    inFile = fopen("plaut04.rc", "r");
#else
    strcat(resource,"/plaut04/r3bplaut04.rc");
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
        if(buffer[0] != '#')
// else it is a comment line, discard it. Nothing need to do here.
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
                for(unsigned i = 0; i<XtNumber(graphWidgetItems); ++i)
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
                for(unsigned i = 0; i<XtNumber(blWidgetName); ++i)
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
                for(unsigned i = 0; i < XtNumber(intVariableNames) && (!blDealt); ++i)
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
                                satSpeed = atoi(aString)/100.0;
                                break;
                            case 10:
                                MAX_SAT_SPEED = atoi(aString);
                                break;
                            case 11:
                                MIN_SAT_SPEED = atoi(aString);
                                break;
                            case 12:
                                orbitSpeed = atoi(aString)/50.0;
                                break;
                            case 13:
                                MAX_ORBIT_SPEED = atoi(aString);
                                break;
                            case 14:
                                MIN_ORBIT_SPEED = atoi(aString);
                                break;
                            case 15:
                                whichCoord = atoi(aString);
                                break;
                            case 16:
                                bgTransparency = atof(aString);
                                break;
                            case 17:
                                numPeriodAnimated = atof(aString);
                                break;
                            case 18:
                                labelRadius = atof(aString);
                                break;
                            case 19:
			    {
                                diskRotation[0] = atof(aString);
                                int sizerot = 3;
			        readNData(buffer, &diskRotation[1], sizerot);
                                break;
                            }
                            case 20:
			    {
                                diskPosition[0] = atof(aString);
                                int sizeyz = 2;
			        readNData(buffer, &diskPosition[1], sizeyz);
                                break;
                            }
                            case 21:
                                diskRadius = atof(aString);
                                break;
                            case 22:
                                diskHeight = atof(aString);
                                break;
                            case 23:
                                diskTransparency = atof(aString);
                                break;
			    case 24:
                                diskFromFile = (strcasecmp(aString,"Yes")==0) ? true : false;
                                break;
                            case 25:
			    {
                                spherePosition[0] = atof(aString);
		                int sizeyz = 2;
			        readNData(buffer, &spherePosition[1], sizeyz);
                                break;
                            }
                            case 26:
                                sphereRadius = atof(aString);
                                break;
                            case 27:
                                sphereTransparency = atof(aString);
                                break;
			    case 28:
                                sphereFromFile = (strcasecmp(aString,"Yes")==0) ? true : false;
                                break;
                            case 29:
                                satRadius = atof(aString);
                                break;
#ifdef R3B
                            case 30:
                                largePrimRadius = atof(aString);
                                break;
                            case 31:
                                smallPrimRadius = atof(aString);
                                break;
                            case 32:
                                libPtScaler = atof(aString);
                                break;
                            case 33:
                                whichCoordSystem = atoi(aString);
                                break;
                            case 34:
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
                for(unsigned i = 0; i<XtNumber(nDataVarNames); ++i)
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
                    lblChoice[is] = lblIdx[is] - 1;
                }
            }

//----------------------- End ---------------------------OCT 7, 04

            if(!blDealt)
            {
                for(unsigned i = 0; i < XtNumber(axesNames) && (!blDealt); ++i)
                {
                    if(strcasecmp(strTemp, axesNames[i]) == 0)
                    {
                        int size = -1;
                        int pars[MAX_PAR];
                        readNIntData(buffer, pars, size);
                        mySolNode.npar = size;
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
    for(unsigned i = 0; i<XtNumber (graphWidgetItems); ++i)
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

    whichCoordSystemTemp = whichCoordSystem;
    whichCoordSystemOld  = whichCoordSystem ;

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

    static const float linecolor[][3] = {
      {1.0, 1.0, 1.0}, {1.0, 0.0, 0.0}, {0.0, 1.0, 0.0}, {0.0, 0.0, 1.0},
      {1.0, 1.0, 0.0}, {0.5, 0.5, 0.0}, {0.0, 0.0, 0.5}, {0.0, 0.5, 0.5}, 
      {1.0, 0.0, 1.0}, {0.0, 1.0, 0.0}, {0.3, 0.0, 0.3}, {0.6, 0.0, 0.6},
      {1.0, 1.0, 1.0} };

    static const float envcolors[][3] = {
      {0.0, 0.0, 0.0}, {1.0, 0.0, 0.0}, {0.0, 1.0, 0.0}, {0.0, 0.0, 1.0},
      {0.0, 1.0, 0.0}, {1.0, 0.0, 0.0}, {0.0, 0.0, 1.0}, {1.0, 0.0, 0.0}, 
#ifdef R3B
      {0.0, 1.0, 0.0}, {1.0, 0.0, 0.0}, {0.0, 0.0, 1.0}, {1.0, 0.0, 1.0}
#endif
    };

    for(unsigned i = 0; i < sizeof(envcolors)/sizeof(envcolors[0]); ++i)
        envColors[i] = SbColor(envcolors[i]);

    for(int i=0; i<NUM_SP_POINTS; ++i) {
        lineColor[i] = SbColor(linecolor[i]);
        linePattern[i]   = 0xffff;
    }

    stabilityLinePattern[0]   = 0x1111;
    stabilityLinePattern[1]   = 0xffff;

// set options.
    for(unsigned i = 0; i < sizeof(options); ++i)
        options[i] = false;

#ifndef R3B
    options[OPT_NORMALIZE_DATA] = true;
#endif

// set default graph type/style specification
    whichCoordSystem = ROTATING_F;
    whichStyle      = 0;  
#ifdef R3B
    whichCoord         = 3;
#endif
    whichType       = SOLUTION; 

    lblIdxSize       = 1;

    lblIndices[0]    = 0;   

#ifdef R3B
    dai.solXSize = 1; dai.solX[0] = 1;
    dai.solYSize = 1; dai.solY[0] = 2;
    dai.solZSize = 1; dai.solZ[0] = 3;
    dai.bifXSize = 1; dai.bifX[0] = 4;
    dai.bifYSize = 1; dai.bifY[0] = 5;
    dai.bifZSize = 1; dai.bifZ[0] = 6;
#else
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

    for(solutionp cur = solHead; solHead != NULL; cur = solHead) {
        solHead = cur->next;
        delete cur;
    }

#ifndef R3B
    setShow3D         = false;
#else
    setShow3D         = true;
#endif
    solHead           = NULL;
    animationLabel    = MY_ALL;
    orbitSpeed        = 1.0;
#ifndef R3B
    satSpeed          = 0.5;
#else
    satSpeed          = 1.0;
#endif
    lineWidthScaler   = 1.0;
    labelRadius      = 1.0;
    numPeriodAnimated = 1.0;

#ifdef R3B
    coloringMethod    = CL_STABILITY;
    largePrimRadius   = 1.0;
    smallPrimRadius   = 1.0;
    blMassDependantOption = false;
#else
    coloringMethod    = CL_COMPONENT;
#endif
    satRadius         = 1.0;

    mySolNode.npar= 1;
    mySolNode.parID[0] = 10;

    setShow3DSol = setShow3D;
    setShow3DBif = setShow3D;
    coloringMethodType[SOLUTION] = coloringMethodType[BIFURCATION] =
        coloringMethod;

    if((autoDir=getenv("AUTO_DIR")) == NULL)
        autoDir=(char *)".";
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
    delete [] mySolNode.numVerticesEachBranch;
    delete [] mySolNode.numOrbitsInEachBranch;
    delete [] mySolNode.branchID;
    delete [] mySolNode.parMax;
    delete [] mySolNode.parMin;
    delete [] mySolNode.parMid;

    delete [] myBifNode.ptStability;
    delete [] myBifNode.numVerticesEachBranch;
    delete [] myBifNode.branchID;

    delete [] clientData.multipliers;
    delete [] clientData.numFM;
    delete [] clientData.solMax;
    delete [] clientData.solMin;

    if (mySolNode.totalNumPoints > 0)
        delete [] clientData.solData[0];
    delete [] clientData.solData;
    mySolNode.totalNumPoints  = 0;
    delete [] clientData.bifData;

    for(solutionp cur = solHead; solHead != NULL; cur = solHead) {
        solHead = cur->next;
        delete cur;
    }
}


//////////////////////////////////////////////////////////////////////////
//
int
writePreferValuesToFile()
//
//////////////////////////////////////////////////////////////////////////
{
    int state = 0;

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
    fprintf(outFile,"# DEFAULT color is also used when animationLabel == MY_ALL, i.e.,\n");
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

    fprintf(outFile, "\n# Initialize the default options:\n");
    for(unsigned i = 0; i<XtNumber(graphWidgetItems); ++i)
    {
        fprintf(outFile, "%-21.21s = ",graphWidgetItems[i]);
        options[i] ? fprintf(outFile, " Yes\n") : fprintf(outFile, " No\n");
    }

    for(unsigned i = 0; i < XtNumber(intVariableNames); ++i)
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
                fprintf(outFile, "\n# Set the initial, maximum and minimum animation speed:\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", (int)floor(satSpeed*100+0.5));
            case 10:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", MAX_SAT_SPEED);
                break;
            case 11:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", MIN_SAT_SPEED);
                break;
            case 12:
                fprintf(outFile, "\n# Set the initial, maximum and minimum highlighting animation speed:\n"); 
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", (int)floor(orbitSpeed*50+0.5));
            case 13:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", MAX_ORBIT_SPEED);
                break;
            case 14:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", MIN_ORBIT_SPEED);
                break;
            case 15:
                fprintf(outFile, "\n# Initialize the default coordinate axes:\n");
                fprintf(outFile, "#  0 --- None,\n");
                fprintf(outFile, "#  1 --- at geometry center or origin,\n");
                fprintf(outFile, "#  2 --- at left and behind,\n");
                fprintf(outFile, "#  3 --- at left and ahead. \n");
                fprintf(outFile, "#  4 --- always at origin. \n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n", whichCoord);
                break;
            case 16:
                fprintf(outFile, "\n# Background pictures transparency");
                fprintf(outFile, "\n# [0.0, 1.0] \n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", bgTransparency );
                break;
            case 17:
#ifdef R3B
                fprintf(outFile, "\n# Set default number of periods animated\n");
                fprintf(outFile, "\n# The value should be power of 2.\n");
#else
                fprintf(outFile, "\n# set default number of periods showing in inertial frame\n");
#endif
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%f\n", numPeriodAnimated);
                break;
            case 18:
                fprintf(outFile, "\n# Set the radius of the spheres used for labels:\n");
                fprintf(outFile, "# The normal size is 1.0.\n");
                fprintf(outFile, "# For smaller radius, use 0.xxx\n");
                fprintf(outFile, "# For bigger radius, use  X.XXX\n"); 
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", labelRadius);
                break;
            case 19:
                fprintf(outFile, "\n# Disk Rotation\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%f, %f, %f, %f\n", diskRotation[0], 
			diskRotation[1], diskRotation[2], diskRotation[3] );
                break;
            case 20:
                fprintf(outFile, "\n# Disk Position\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%f, %f, %f\n", diskPosition[0], 
			diskPosition[1], diskPosition[2] );
                break;
            case 21:
                fprintf(outFile, "\n# Disk Radius\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%f\n", diskRadius );
                break;
            case 22:
                fprintf(outFile, "\n# Disk Height\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%f\n", diskHeight );
                break;
            case 23:
                fprintf(outFile, "\n# Disk Transparency [0, 1] \n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", diskTransparency );
                break;
            case 24:
                fprintf(outFile, "\n# Read Disk From File \n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                (diskFromFile) ? fprintf(outFile, "Yes \n"): fprintf(outFile, "No\n");
                break;
            case 25:
                fprintf(outFile, "\n# Sphere Position\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%f, %f, %f\n", spherePosition[0], 
			spherePosition[1], spherePosition[2] );
                break;
            case 26:
                fprintf(outFile, "\n# Sphere Radius\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%f\n", sphereRadius );
                break;
            case 27:
                fprintf(outFile, "\n# Sphere Transparency [0, 1] \n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", sphereTransparency );
                break;
            case 28:
                fprintf(outFile, "\n# Read Sphere From File \n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                (sphereFromFile) ? fprintf(outFile, "Yes \n"): fprintf(outFile, "No\n");
                break;
            case 29:
#ifdef R3B
                fprintf(outFile, "\n# set the radius of  satellite, large primary, small primary\n");
#else
                fprintf(outFile, "\n# Set the radius of the animation object:\n");
#endif
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", satRadius);
                break;
#ifdef R3B
            case 30:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", largePrimRadius);
                break;
            case 31:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", smallPrimRadius);
                break;
            case 32:
                fprintf(outFile, "%s = ", intVariableNames[i]);
                fprintf(outFile, "%f\n", libPtScaler);
                break;
            case 33:
                fprintf(outFile, "\n# initialize the default coordinate system");
                fprintf(outFile, "\n#  0 --- Rotating, 1 --- inertial Bary Centered,");
                fprintf(outFile, "\n#  2 --- Big Primary Centered, 3 --- Small Primary Centered\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%i\n",whichCoordSystem);
                break;
            case 34:
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%d\n", numOfStars);
                break;
#endif
        }
    }

    for(unsigned i = 0; i<XtNumber(nDataVarNames); ++i)
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
                fprintf(outFile, "# Surface color:\n");
                break;
            case 5 :
                fprintf(outFile, "# Unstable solution Color:\n");
                break;
            case 6 :
                fprintf(outFile, "# Stable solution Color:\n");
                break;
            case 7 :
#ifdef R3B
                fprintf(outFile,"\n# sat, large prim, large prim line, small prim, small prim line\n");
                /* fall though */
            case 8 :
            case 9 :
            case 10 :
            case 11 :
#else
                fprintf(outFile, "# Color of the animation object:\n"); 
#endif
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
    for(int is=0; is < (lblChoice[is] < 0 ? 1 : lblIdxSize); ++is)
    {
        fprintf(outFile, "Labels   = %i", lblChoice[is]+1);
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

////////////////////////////////////////////////////////////////////////
//
//        detach everything and nuke the existing scene.
//
void
deleteScene()
//
////////////////////////////////////////////////////////////////////////
{
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
    SbString errorMessage;

    postDeals();
    parseFileName(filename, sFileName, bFileName, dFileName);
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


#if 0
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
    FILE *ivDataPipe = NULL;

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
    if (ivDataPipe != NULL)
    {
        fclose(ivDataPipe);
    }
    return sep;
}
#endif

