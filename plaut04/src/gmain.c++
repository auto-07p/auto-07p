#define XtNumber(arr) ((sizeof(arr) / sizeof(arr[0])))

#include <fstream>
#include <sstream>
#include <float.h>

#include "gplaut04.h"
#include "gVarNames.h"
#include "createDisk.h"
#include "createSphere.h"
#include "createLegend.h"
#include "readFM.h"
#include "stringtrim.h"
#include "solution.h"
#include "bifurcation.h"

int specialColorItems = CL_SP_ITEMS;

float fmData[12];
const char *autoDir;

SbColor lineColor[NUM_SP_POINTS];
SbColor lineColorTemp[NUM_SP_POINTS];
SbColor lineColorOld[NUM_SP_POINTS];
SbColor envColors[12];

struct DefaultAxisItems dai;

unsigned long linePattern[NUM_SP_POINTS];
unsigned long linePatternTemp[NUM_SP_POINTS];
unsigned long linePatternOld[NUM_SP_POINTS];

unsigned long stabilityLinePattern[2];
unsigned long stabilityLinePatternTemp[2];

bool blDrawTicker = false;
bool useR3B = false;

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

int whichCoord = 0 ;
int whichCoordTemp = 0 ;
int whichCoordOld = 0 ;
int whichCoordSystem = 0 ;
int whichCoordSystemTemp = 0 ;
int whichCoordSystemOld = 0 ;

unsigned long graphWidgetToggleSet     = (unsigned long) 0 ;
unsigned long graphWidgetToggleSetTemp = (unsigned long) 0 ;
unsigned long graphWidgetToggleSetOld  = (unsigned long) 0 ;

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

long int animationLabel = MY_ALL;

int winWidth, winHeight;

std::vector<int> xCoordIndices, yCoordIndices, zCoordIndices;
std::vector<int> lblIndices, lblChoice;

int MAX_SAT_SPEED = 100;
int MIN_SAT_SPEED = 0;
int MAX_ORBIT_SPEED = 100;
int MIN_ORBIT_SPEED = 0;

float orbitSpeed = 1.0;
float satSpeed   = 0.5;
float satRadius  = 1.0;
float lineWidthScaler = 1.0;
float aniLineScaler = 2.0;
float labelRadius = 1.0;

int   coloringMethod = -1;
int   coloringMethodType[2] = {-1, -1};
float bgTransparency = 0;
float numPeriodAnimated = 1.0;

long int numLabels;

double legendScaleValues[2];

SoSeparator *userScene;
SoSeparator *root;
SoSeparator *starryBackground;

std::vector<std::string> xAxis, yAxis, zAxis, labels, coloringMethodList;
int *myLabels;

///////////////////////////////////////////////////////////////
//
//  Function prototypes
//

#if 0
static SoSeparator * drawATube(TubeNode cnode);
static SoSeparator * MyFileRead(const char *filename, SbString &errorMessage);
#endif
static void setLegendValues(double* values);

static void lookForThePoint(float position[],long int &bIdx, long int &sIdx);

#if 0
static SoSeparator * createStarryBackground(int total,float diameter);
#endif

static SoSeparator * addLegend();

static SoSeparator * drawStarryBackground(char * bgFileName);

static int readResourceParameters(const char *dir);

#if 0
static void processPrinting(char* filename );
#endif

//
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
//
//  functions
//
////////////////////////////////////////////////////////////////////////

#if 0
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
#endif

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

    std::vector<int>::size_type mx = 
              std::max(std::max(xCoordIndices.size(), yCoordIndices.size()), 
                      std::max(yCoordIndices.size(), zCoordIndices.size()));

    // look for the maximum/minum value in the x-axis, y-axis, z-axis
    if(whichType != BIFURCATION)
    {
        for(std::vector<int>::size_type i=0; i<mx; i++)
        {
            std::vector<int>::size_type component = i+1;
            varIndices[0]=xCoordIndices[
                    (i>=xCoordIndices.size())?(i%xCoordIndices.size()):(i)];
            varIndices[1]=yCoordIndices[
                    (i>=yCoordIndices.size())?(i%yCoordIndices.size()):(i)];
            varIndices[2]=zCoordIndices[
                    (i>=zCoordIndices.size())?(i%zCoordIndices.size()):(i)];
            mySolNode->searchForMaxMin(component, varIndices);
        }
    }
 
    int time_on = TIME_IS_OFF;

    for(std::vector<int>::size_type i=0; i<mx; i++)
    {
        varIndices[0]=xCoordIndices[
                (i>=xCoordIndices.size())?(i%xCoordIndices.size()):(i)];
        varIndices[1]=yCoordIndices[
                (i>=yCoordIndices.size())?(i%yCoordIndices.size()):(i)];
        varIndices[2]=zCoordIndices[
                (i>=zCoordIndices.size())?(i%zCoordIndices.size()):(i)];

        if (varIndices[0] == 0) time_on = TIME_ON_X; 
        if (varIndices[1] == 0) time_on = TIME_ON_Y; 
        if (varIndices[2] == 0) time_on = TIME_ON_Z; 

        SoSeparator *result;
        if(whichType != BIFURCATION)
        {
            animationLabel = myLabels[lblIndices[0]];
            mySolNode->copyDataToWorkArray(varIndices, i+1, mx, time_on);
            result = mySolNode->createSceneWithWidgets();
        }
        else 
        {
            myBifNode->copyDataToWorkArray(varIndices);
            result = myBifNode->createScene();
        }

        newScene->addChild( result );
    }

    // create starry background
    if(options[OPT_BACKGROUND])
    {
        char *bgFileName = new char [strlen(autoDir) + 34];
        sprintf(bgFileName, "%s%s",autoDir, "/plaut04/widgets/background.jpg");
        newScene->addChild(drawStarryBackground(bgFileName));
        delete [] bgFileName;
    }

    // add legend
    if(options[OPT_LEGEND])
    {
        newScene->addChild(addLegend());
    }

    if (!useR3B && options[OPT_REF_PLAN])
        newScene->addChild(createDisk(diskPosition,diskRadius));
    if (options[OPT_REF_SPHERE])
        newScene->addChild(createSphere(spherePosition,sphereRadius));


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

    if(coloringMethod == CL_CURVE_NUMBER || coloringMethod == CL_BRANCH_NUMBER)
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

    if(coloringMethod == CL_BRANCH_NUMBER)
    {
        if(whichType == SOLUTION)
            branchID = mySolNode->branchID(branchID);
        else
            branchID = myBifNode->branchID(branchID);
        branchID--;
    }

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


////////////////////////////////////////////////////////////////////////
//
//  initialize win basic things
//
static bool
readSolutionAndBifurcationData(bool blFirstRead,
                               const std::string& sFileName,
                               const std::string& bFileName,
                               const std::string& dFileName)
//
///////////////////////////////////////////////////////////////////////
{
    bool blOpenSolFile, blOpenBifFile;

    blOpenSolFile = mySolNode->parse(sFileName.c_str());
    if(!blOpenSolFile)
        printf(" Solution file does not exist.\n");

    mySolNode->alloc();

    clientData.solMax = new float [mySolNode->nar()+1];
    clientData.solMin = new float [mySolNode->nar()+1];

    blOpenBifFile = myBifNode->parse(bFileName.c_str());
    if(!blOpenBifFile) printf(" Bifurcation file does not exist!\n");

    if((!blOpenBifFile) && (!blOpenSolFile) && (!blFirstRead)) return false;
    else if((!blOpenBifFile) && (!blOpenSolFile))
    {
//        printf(" Target files do not exist!\n");
        exit(1);
    }

    myBifNode->alloc();

    myLabels = new int[std::max(myBifNode->totalLabels(),
                                mySolNode->totalLabels())+SP_LBL_ITEMS];
    clientData.labelIndex = new long int[std::max(myBifNode->totalLabels(),
                                                  mySolNode->totalLabels())][4];

    int varIndices[3];

    if( blOpenBifFile)
    {
        if (!myBifNode->read(bFileName.c_str(), varIndices))
            printf(" Failed to read the bifurcation file!\n");
    }
    else
    {
        whichType = SOLUTION;
    }

    if( mySolNode->numOrbits() > 0 )
    {
        blOpenSolFile = mySolNode->read(sFileName.c_str(), varIndices);
        if(!blOpenSolFile)
            printf(" Failed to read the solution file!\n");
        else if(myBifNode->totalLabels() != 0 &&
                mySolNode->totalLabels() != myBifNode->totalLabels())
        {
            printf(" total labels in b-file is: %i, in s-file is: %i\n",
                   myBifNode->totalLabels(), mySolNode->totalLabels());
            printf(" The total number of labels in the bifurcation file is different from\n");
            printf(" the total number of labels in the solution file! CHECK IT.\n");
            exit(1);
        }

        if(mySolNode->nar() <= 3)
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

    clientData.maxndim = std::max(myBifNode->maxndim(),mySolNode->nar()-1);
    int st = readFM(dFileName.c_str(), myBifNode->totalNumPoints());
    if(st!=0)
        printf(" Failed to read the diagnostic file.\n");

    return TRUE;
}


////////////////////////////////////////////////////////////////////////
//
void
lookForThePoint(float position[],long int &bIdx, long int &sIdx)
//
////////////////////////////////////////////////////////////////////////
{

    int varIndices[3];
    std::vector<int>::size_type mx = 
             std::max(std::max(xCoordIndices.size(), yCoordIndices.size()),
		      std::max(yCoordIndices.size(), zCoordIndices.size()));
    float minDis = FLT_MAX;
    long int index = 0;
    long int ib = 0;
    float distance;
    float *data;
    sIdx = bIdx = 0;
    for(std::vector<int>::size_type i=0; i<mx; i++)
    {
        varIndices[0]=xCoordIndices[
                 (i>=xCoordIndices.size())?(i%xCoordIndices.size()):(i)];
        varIndices[1]=yCoordIndices[
                 (i>=yCoordIndices.size())?(i%yCoordIndices.size()):(i)];
        varIndices[2]=zCoordIndices[
                 (i>=zCoordIndices.size())?(i%zCoordIndices.size()):(i)];
        long int lblidx = lblIndices[0];
        long int maxp, sumup = 0;
        if(whichType == BIFURCATION)
        {
            maxp = myBifNode->totalNumPoints();
        }
        else 
        {
	    animationLabel = myLabels[lblIndices[0]];
            if(animationLabel == MY_ALL || lblIndices.size() >1)
                maxp = mySolNode->totalNumPoints();
            else
            {
                for(int j=0; j<lblidx; ++j)
                    sumup += mySolNode->numVerticesEachPeriod(j);
                maxp = mySolNode->numVerticesEachPeriod(lblidx);
            }
        }
        for(long int j=0; j<maxp; ++j)
	{
            if(whichType == BIFURCATION)
                data = &myBifNode->data()[j*myBifNode->nar()];
            else
                data = mySolNode->data()[sumup+j];
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
            if(animationLabel == MY_ALL || lblIndices.size() >1)
            {
                for (ib = 0; ib < mySolNode->totalLabels(); ib++)
                {
                    sumup += mySolNode->numVerticesEachPeriod(ib);
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


#if 0
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
#endif


////////////////////////////////////////////////////////////////////
//
static SbBool
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

    if (options[OPT_NORMALIZE_DATA]) {
        if(whichType != BIFURCATION)
        {
            mySolNode->denormalizePosition(position);	  
        }
        else
        {
            myBifNode->denormalizePosition(position);	  
        }
    }

    lookForThePoint(position, bIdx, sIdx);

    float *data;
    if(whichType != BIFURCATION)
    {
        if(sIdx > mySolNode->totalNumPoints() || sIdx < 0)
            return false;
        size = mySolNode->nar();
        data = mySolNode->data()[sIdx];
    }
    else {
        if (bIdx > myBifNode->totalNumPoints() || bIdx < 0)
            return false;
        size = myBifNode->nar();
        data = &myBifNode->data()[bIdx * size];
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
static void
initCoordAndLableListItems()
//
///////////////////////////////////////////////////////////////////
{
    int i = 0;
    int nar = whichType == SOLUTION ? mySolNode->nar() : myBifNode->nar();
    xAxis.clear();
    yAxis.clear();
    zAxis.clear();
    for(i=0; i<nar; i++)
    {
        std::stringstream s;
        s << i;
        xAxis.push_back(s.str());
        yAxis.push_back(s.str());
        zAxis.push_back(s.str());
    }

    coloringMethodList.clear();
    coloringMethodList.push_back("BRAN");
    coloringMethodList.push_back("STAB");
    coloringMethodList.push_back("PONT");
    coloringMethodList.push_back("CURV");
    specialColorItems = 4;

    if(whichType == SOLUTION)
    {
        coloringMethodList.push_back("TYPE");
        coloringMethodList.push_back("LABL");
        coloringMethodList.push_back("COMP");
        specialColorItems = 7;
    }
    for(i=0; i<nar; i++)
    {
        std::stringstream s;
        s << i;
        coloringMethodList.push_back(s.str());
    }
    if(whichType == SOLUTION)
    {
        for(i=0; i<mySolNode->npar(); ++i)
        {
            std::stringstream s;
            s << "PAR(" << mySolNode->parID(i)+1 << ")";
            coloringMethodList.push_back(s.str());
        }
    }

    if(mySolNode->numOrbits() > 0)
    {
// the solution file does exist.
        numLabels = mySolNode->numOrbits();
        for(int j=0; j<numLabels; j++) myLabels[j] = mySolNode->labels(j);
        if(useR3B)
        {
// initial mass dependent options.
            float lastMass = mySolNode->masses(1);
            blMassDependantOption = true;
            for(i=1; i<mySolNode->numOrbits(); i++)
            {
                if(fabs(mySolNode->masses(i)-lastMass)/lastMass > 1.0e-3)
                {
                    blMassDependantOption = false;
                    break;
                }
            }
            if(blMassDependantOption) mass = lastMass;
        }
    }
    else
    {
        numLabels = myBifNode->totalLabels();
        for(int j=0; j<numLabels; j++) myLabels[j] = myBifNode->labels(j);
        if(useR3B) blMassDependantOption = false;
    }

    if(useR3B && !blMassDependantOption)
    {
        options[OPT_PRIMARY ]= false;
        options[OPT_LIB_POINTS]= false;
    }

    options[OPT_LEGEND] = false;
    options[OPT_BACKGROUND] = false;

    numLabels += SP_LBL_ITEMS;
    myLabels[numLabels + MY_NONE] = MY_NONE; // -1
    myLabels[numLabels + MY_SPEC] = MY_SPEC; // -2
    myLabels[numLabels + MY_HALF] = MY_HALF; // -3
    myLabels[numLabels + MY_ALL] = MY_ALL; // -4

    labels.clear();
    labels.push_back("ALL");
    labels.push_back("HALF");
    labels.push_back("SPEC");
    labels.push_back("NONE");

    int nty;
    const char *tylabels[] = {
         "MX","R4","R3","R2","R1","UZ","ZH","CP","BT","  ",
         "BP","LP","HB","  ","LP","BP","PD","TR","EP",
         "GH","LPD","LTR","PTR","TTR"};
    for( i=0; i<numLabels-SP_LBL_ITEMS; ++i)
    {
        std::stringstream s;
        s << myLabels[i];
        nty = 0;
        switch (clientData.labelIndex[i][2])
        {
            case -32:
                nty = 10; // 'GH'
                break;
            case 28:
            case 78:
                nty = 11; // 'LPD'
                break;
            case 23:
            case 83:
                nty = 12; // 'LTR'
                break;
            case 77:
            case 87:
                nty = 13; // 'PTR'
                break;
            case 88:
                nty = 14; // 'TTR'
                break;
            default:
                if (clientData.labelIndex[i][2] >= 0)
                    nty = clientData.labelIndex[i][2] % 10;
                else
                    nty = -(-clientData.labelIndex[i][2] % 10);
                break;
        }
        if (nty != 0 && tylabels[nty+9][0] != ' ')
        {
            s << " " << tylabels[nty+9];
        }
        labels.push_back(s.str());
    }

    if(whichType == SOLUTION)
    {
        xCoordIndices = dai.solX;
        yCoordIndices = dai.solY;
        zCoordIndices = dai.solZ;
    }
    else
    {
        xCoordIndices = dai.bifX;
        yCoordIndices = dai.bifY;
        zCoordIndices = dai.bifZ;
    }

//---------------------- Begin ---------------------------OCT 7, 04

    int half = 2;
    lblIndices.clear();
    //tmp = strtok(manyChoice, ",");
    if( lblChoice[0] == MY_ALL) // -4
    {
        lblIndices.push_back(numLabels + MY_ALL);
        half = 2;
    }
    else if(lblChoice[0] == MY_HALF)  // -3
    {
        for(int j = 0; j < numLabels - SP_LBL_ITEMS; j++)
        {
            if(abs(clientData.labelIndex[j][2])!= 4 || (j+1)%half == 0)
                lblIndices.push_back(j);
        }

        half *= 2;
    }
    else if(lblChoice[0] == MY_SPEC) // -2
    {
        for(int j = 0; j < numLabels - SP_LBL_ITEMS; j++)
        {
            if(clientData.labelIndex[j][2] !=  TYPE_UZ    && clientData.labelIndex[j][2] != TYPE_RG &&
               clientData.labelIndex[j][2] != TYPE_EP_ODE && clientData.labelIndex[j][2] != TYPE_MX)
                lblIndices.push_back(j);
        }
        half = 2;
    }
    else if(lblChoice[0] == MY_NONE)  // -1
    {
        lblIndices.push_back(numLabels + MY_NONE);
        half = 2;
    }
    else // Specified labels
    {
        for(std::vector<int>::size_type idx = 0; idx < lblIndices.size(); idx++)
            lblIndices.push_back(lblChoice[idx]);
        half = 2;
    }

    for(int i=0; i<11; ++i)
    {
        optSol[i] = options[i];
        optBif[i] = options[i];
    }
 

//----------------------- End ----------------------------

    if(!setShow3D)
    {
        for(std::vector<int>::size_type i=0; i<zCoordIndices.size(); i++)
            zCoordIndices[i] = -1;
    }
}


static std::stringstream&
readAString(std::stringstream& buffer, std::string& aString);

///////////////////////////////////////////////////////////////////
//  
//       Read line colors and line pattern from the resource file.
//       each call of this function, read one line
//
static void
readLineColorAndPattern(std::stringstream& buffer, float *lineColor, unsigned long & linePattern)
//
///////////////////////////////////////////////////////////////////
{
    std::string word;
    for(int k=0; k<3; ++k)
    {
        readAString(buffer, word);
        std::stringstream(word) >> lineColor[k];
    }
    readAString(buffer, word);
    std::stringstream(word) >> std::hex >> linePattern;
}


///////////////////////////////////////////////////////////////////
// 
//       Read flex number of numbers.
//       each call of this function, read one line
//
template <class T>
static void
readNData(std::stringstream& buffer, T *data, int size )
//
///////////////////////////////////////////////////////////////////
{
    std::string word;
    for(int k=0; k<size; ++k)
    {
        readAString(buffer, word);
        std::stringstream(word) >> data[k];
    }
}

template <class T>
static void
readNData(std::stringstream& buffer, std::queue<T> &data)
//
///////////////////////////////////////////////////////////////////
{
    std::string word;
    T item;
    while ( readAString(buffer, word) )
    {
        std::stringstream(word) >> item;
        data.push(item);
    }
}


///////////////////////////////////////////////////////////////////
//
//       Read a string from the buffer. Used to parse those boolean
//       variables or sigle value in a line of the resource file.
//
static std::stringstream&
readAString(std::stringstream& buffer, std::string& aString)
//
///////////////////////////////////////////////////////////////////
{
    std::getline(buffer, aString, ',');
    return buffer;
}


///////////////////////////////////////////////////////////////////
//
static void
readAHexdecimal(std::stringstream& buffer, unsigned long & aHexdecimal )
//
///////////////////////////////////////////////////////////////////
{
    std::string aString;
    readAString(buffer, aString);
    std::stringstream(aString) >> std::hex >> aHexdecimal;
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
readResourceParameters(const char *dir)
//
///////////////////////////////////////////////////////////////////
{
    int state = 0;

    float aVector[3];
    unsigned long aHex;
    std::string aString;
    const char *file;

    if(!useR3B)
        file = "plaut04.rc";
    else
        file = "r3bplaut04.rc";
    std::ifstream inFile(file);

    if (!inFile && dir)
    {
        std::string resource = dir;
        resource += "/";
        resource += file;
        inFile.open(resource.c_str());
    }

    if (!inFile)
    {
        std::string resource = autoDir;
        resource += "/plaut04/";
        resource += file;
        inFile.open(resource.c_str());
        if(!inFile)
        {
            printf("Unable to open the  resource file. I will use the default values.\n");
            state = 1;
            return state;
        }
    }

    std::string line;        
    while ( std::getline(inFile, line) )
    {
        if(line[0] != '#')
// else it is a comment line, discard it. Nothing need to do here.
        {
            std::stringstream buffer(line);
            std::string str;
            std::getline(buffer, str, '=');
            strrighttrim(str);
            strlefttrim(str);
            const char *strTemp = str.c_str();

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
                        strrighttrim(aString);
                        strlefttrim(aString);
                        const char *aNewString = aString.c_str();
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
                        strrighttrim(aString);
                        strlefttrim(aString);
                        const char *aNewString = aString.c_str();
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
			case 3:
			    blDrawTicker = (strcasecmp(aNewString,"Yes")==0);
			    break;
			case 4:
			    useR3B = (strcasecmp(aNewString,"Yes")==0);
			    break;
			}
                        blDealt = true;
                        break;
                    }
                }
            }

            if( !blDealt )
            {
                for(unsigned i = 0; i < XtNumber(intVariableNames) && (!blDealt); ++i)
                {
                    if(strcasecmp(strTemp, intVariableNames[i])==0)
                    {
                        switch (i)
                        {
                            case 0:
                                buffer >> whichType;
                                break;
                            case 1:
                                buffer >> whichStyle;
                                break;
                            case 2:
                                buffer >> winWidth;
                                break;
                            case 3:
                                buffer >> winHeight;
                                break;
                            case 4:
                                buffer >> coloringMethodType[BIFURCATION];
                                coloringMethodType[SOLUTION] =
                                coloringMethodType[BIFURCATION];
                                break;
                            case 5:
                                buffer >> coloringMethodType[SOLUTION];
                                break;
                            case 6:
                                buffer >> coloringMethodType[BIFURCATION];
                                break;
                            case 7:
                                buffer >> lineWidthScaler;
                                break;
                            case 8:
                                buffer >> aniLineScaler;
                                break;
                            case 9:
                                buffer >> satSpeed;
                                satSpeed /= 100.0;
                                break;
                            case 10:
                                buffer >> MAX_SAT_SPEED;
                                break;
                            case 11:
                                buffer >> MIN_SAT_SPEED;
                                break;
                            case 12:
                                buffer >> orbitSpeed;
                                orbitSpeed /= 50.0;
                                break;
                            case 13:
                                buffer >> MAX_ORBIT_SPEED;
                                break;
                            case 14:
                                buffer >> MIN_ORBIT_SPEED;
                                break;
                            case 15:
                                buffer >> whichCoord;
                                break;
                            case 16:
                                buffer >> bgTransparency;
                                break;
                            case 17:
                                buffer >> numPeriodAnimated;
                                break;
                            case 18:
                                buffer >> labelRadius;
                                break;
                            case 19:
			    {
			        readNData(buffer, diskRotation, 4);
                                break;
                            }
                            case 20:
			    {
			        readNData(buffer, diskPosition, 3);
                                break;
                            }
                            case 21:
                                buffer >> diskRadius;
                                break;
                            case 22:
                                buffer >> diskHeight;
                                break;
                            case 23:
                                buffer >> diskTransparency;
                                break;
			    case 24:
                                readAString(buffer, aString);
                                strrighttrim(aString);
                                strlefttrim(aString);
                                diskFromFile = (strcasecmp(aString.c_str(),"Yes")==0) ? true : false;
                                break;
                            case 25:
			    {
			        readNData(buffer, spherePosition, 3);
                                break;
                            }
                            case 26:
                                buffer >> sphereRadius;
                                break;
                            case 27:
                                buffer >> sphereTransparency;
                                break;
			    case 28:
                                readAString(buffer, aString);
                                strrighttrim(aString);
                                strlefttrim(aString);
                                sphereFromFile = (strcasecmp(aString.c_str(),"Yes")==0) ? true : false;
                                break;
                            case 29:
                                buffer >> satRadius;
                                break;
                            case 30:
                                buffer >> largePrimRadius;
                                break;
                            case 31:
                                buffer >> smallPrimRadius;
                                break;
                            case 32:
                                buffer >> libPtScaler;
                                break;
                            case 33:
                                buffer >> whichCoordSystem;
                                break;
                            case 34:
                                buffer >> numOfStars;
                                break;
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
                float colors[3];
                for(unsigned i = 0; i<XtNumber(nDataVarNames); ++i)
                {
                    if(strcasecmp(strTemp, nDataVarNames[i])==0)
                    {
                        readNData(buffer, colors, 3);
                        for(int j=0; j<3; ++j) envColors[i][j]=colors[j];
                        blDealt = true;
                        break;
                    }
                }
            }

            if(!blDealt && strcasecmp(strTemp, "parameter ID")==0)
            {
                std::queue<int> parIDs;
                readNData(buffer, parIDs);
                mySolNode->set_parID(parIDs);
                blDealt = true;
            }

//---------------------- Begin ---------------------------OCT 7, 04

            if(!blDealt && strcasecmp(strTemp, "Labels")==0)
            {
                std::queue <int> lblIdx;
                readNData(buffer, lblIdx);
                blDealt = true;
                lblChoice.clear();
                while(!lblIdx.empty())
                {
                    lblChoice.push_back(lblIdx.front() - 1);
                    lblIdx.pop();
                }
            }

//----------------------- End ---------------------------OCT 7, 04

            if(!blDealt)
            {
                for(unsigned i = 0; i < XtNumber(axesNames) && (!blDealt); ++i)
                {
                    if(strcasecmp(strTemp, axesNames[i]) == 0)
                    {
                        std::queue<int> pars;
                        readNData(buffer, pars);
                        int size = pars.size();			
                        blDealt = true;
                        std::vector<int> *xyz = 0;
                        switch ( i )
                        {
                            case 0:
                                xyz = &dai.solX;
                                break;
                            case 1:
                                xyz = &dai.solY;
                                break;
                            case 2:
                                xyz = &dai.solZ;
                                break;
                            case 3:
                                xyz = &dai.bifX;
                                break;
                            case 4:
                                xyz = &dai.bifY;
                                break;
                            case 5:
                                xyz = &dai.bifZ;
                                break;
                        }
                        xyz->clear();
                        for(int is=0; is<size; ++is)
                        {
                            xyz->push_back(pars.front());
                            pars.pop();
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
    return state;
}


/////////////////////////////////////////////////////////////////////
//
//       Set initial values for those temp variables according to
//     their correspondent variables.
//
static void
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
static void
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
      {0.0, 1.0, 0.0}, {1.0, 0.0, 0.0}, {0.0, 0.0, 1.0}, {1.0, 0.0, 1.0}
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

    if(!useR3B)
        options[OPT_NORMALIZE_DATA] = true;

// set default graph type/style specification
    whichCoordSystem = ROTATING_F;
    whichStyle      = 0;  
    if(useR3B)
    {
        whichCoord         = 3;
        blDrawTicker = true;
    }
    whichType       = SOLUTION; 

    lblIndices.push_back(0);

    if (useR3B)
    {
        dai.solX.push_back(1);
        dai.solY.push_back(2);
        dai.solZ.push_back(3);
        dai.bifX.push_back(4);
        dai.bifY.push_back(5);
        dai.bifZ.push_back(6);
        setShow3D         = true;
        satSpeed          = 1.0;
        coloringMethod    = CL_STABILITY;
        largePrimRadius   = 1.0;
        smallPrimRadius   = 1.0;
        blMassDependantOption = false;
        intVariableNames[9] = "Sat Animation Speed";
        intVariableNames[10] = "Sat Max Animation Speed";
        intVariableNames[11] = "Sat Min Animation Speed";
        intVariableNames[29] = "Satellite Radius";
        nDataVarNames[7] = "satellite Color";
	graphWidgetItems[OPT_PERIOD_ANI] = "Orbit Animation";
	graphWidgetItems[OPT_SAT_ANI] = "Satellite Animation";
    }
    else {
        dai.solX.push_back(0);
        dai.solY.push_back(1);
        dai.solZ.push_back(2);

        dai.bifX.push_back(0);
        dai.bifY.push_back(1);
        dai.bifZ.push_back(2);
        setShow3D         = false;
        satSpeed          = 0.5;
        coloringMethod    = CL_COMPONENT;
    }
    animationLabel    = MY_ALL;
    orbitSpeed        = 1.0;
    lineWidthScaler   = 1.0;
    labelRadius      = 1.0;
    numPeriodAnimated = 1.0;

    satRadius         = 1.0;

    std::queue<int> parIDs;
    parIDs.push(10);
    mySolNode->set_parID(parIDs);

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

    char **pargv = argv;
    char *dir = 0;
    int argcleft = argc - 1;
    std::string sFileName, bFileName, dFileName;

    if( argcleft > 0 )
    {
        pargv = &argv[1];

        if( strcmp(*pargv, "-r3b")==0 )
        {
            useR3B = true;
            pargv++;
            argcleft--;
        }
    }
    if( argcleft > 0 )
    {
        bool is97 = false;

        if( argcleft > 1 && strcmp(*pargv, "97")==0 )
        {
            is97 = true;
            pargv++;
            argcleft--;
        }
        if( argcleft > 1)
        {
            sFileName = *pargv;
            bFileName = *pargv;
            dFileName = *pargv;
	    dir = *pargv;
            pargv++;
            argcleft--;
            sFileName += "/";
            bFileName += "/";
            dFileName += "/";
        }
        if ( is97 )
        {
            sFileName += "q.";
            bFileName += "p.";
            dFileName += "d.";
        }
        else
        {
            sFileName += "s.";
            bFileName += "b.";
            dFileName += "d.";
        }
        sFileName += *pargv;
        bFileName += *pargv;
        dFileName += *pargv;
        argcleft--;
    }
    else
    {
        sFileName = "fort.8";
        bFileName = "fort.7";
        dFileName = "fort.9";
    }
    if( argcleft > 0)
    {
        if(!useR3B)
            printf(" usage: plaut04 [version] [path] [name]\n");
        else
            printf(" usage: r3bplaut04 [path] [name]\n");
        printf(" For example:\n");
        if(!useR3B)
        {
            printf("      plaut04            --- view the fort.7, fort.8 in the current directory \n");
            printf("      plaut04 H1         --- view s.H1, b.H1 in the current directory \n");
            printf("      plaut04 /home/he/myR3B/me H1    --- view s.H1, b.H1 in the /home/he/myR3B/me directory \n");
            printf("      plaut04 97 H1                   --- view AUTO 97 files: q.H1, p.H1 in the current directory \n");
            printf("      plaut04 97 /home/he/myR3B/me H1 --- view AUTO 97 files: q.H1, p.H1 in the /home/he/myR3B/me directory \n");
        }
        else
        {
            printf("      r3bplaut04            --- view the fort.7, fort.8 in the current directory \n");
            printf("      r3bplaut04 H1         --- view s.H1, b.H1 in the current directory \n");
            printf("      r3bplaut04 /home/he/myR3B/me H1    --- view s.H1, b.H1 in the /home/he/myR3B/me directory \n");
            printf("      r3bplaut04 97 H1                   --- view AUTO 97 files: q.H1, p.H1 in the current directory \n");
            printf("      r3bplaut04 97 /home/he/myR3B/me H1 --- view AUTO 97 files: q.H1, p.H1 in the /home/he/myR3B/me directory \n");
        }
        exit(1) ;
    }

    mySolNode = new Solution;
    myBifNode = new Bifurcation;

    setVariableDefaultValues();

    readResourceParameters(dir);

    readSolutionAndBifurcationData(true, sFileName, bFileName, dFileName);

    initCoordAndLableListItems();
    initTempVariables();

    soxtmain(argc,argv);

    return 0;
}


//////////////////////////////////////////////////////////////////////////
//
void
postDeals()
//
//////////////////////////////////////////////////////////////////////////
{
    mySolNode->dealloc();
    myBifNode->dealloc();

    delete [] clientData.multipliers;
    delete [] clientData.numFM;
    delete [] clientData.solMax;
    delete [] clientData.solMin;

    delete [] myLabels;
    delete [] clientData.labelIndex;
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
    if(!useR3B)
        outFile = fopen("plaut04.rc.out", "w");
    else
        outFile = fopen("r3bplaut04.rc.out", "w");

    if (!outFile)
    {
        if(!useR3B)
            printf("Unable to open the  resource file. I will use the default values.\n");
        else
            printf("Unable to open the  resource file.\n");
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
                fprintf(outFile, "#   -7 --- BRANCH\n");
                fprintf(outFile, "#   -6 --- STABILITY\n");
                fprintf(outFile, "#   -5 --- POINT\n");
                fprintf(outFile, "#   -4 --- CURVE\n");
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
                if(useR3B)
                {
                    fprintf(outFile, "\n# Set default number of periods animated\n");
                    fprintf(outFile, "\n# The value should be power of 2.\n");
                }
                else
                    fprintf(outFile, "\n# set default number of periods showing in inertial frame\n");
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
                if(useR3B)
                    fprintf(outFile, "\n# set the radius of  satellite, large primary, small primary\n");
                else
                    fprintf(outFile, "\n# Set the radius of the animation object:\n");
                fprintf(outFile, "%-25.25s = ", intVariableNames[i]);
                fprintf(outFile, "%4.1f\n", satRadius);
                break;
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
                fprintf(outFile,"\n# sat, large prim, large prim line, small prim, small prim line\n");
                /* fall though */
            case 8 :
            case 9 :
            case 10 :
            case 11 :
                fprintf(outFile, "# Color of the animation object:\n"); 
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
    for(std::vector<int>::size_type is=0; is < (lblChoice[is] < 0 ? 1 : lblIndices.size()); ++is)
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
    for(int is=0; is<mySolNode->npar(); ++is)
    {
        fprintf(outFile, " %i, ", mySolNode->parID(is));
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
    for(std::vector<int>::size_type is=0; is<dai.solX.size(); ++is)
    {
        fprintf(outFile, " %i ", dai.solX[is]);
        fprintf(outFile, (is != dai.solX.size()-1) ? "," : "\n");
    }

    fprintf(outFile,"%-25.25s = ", axesNames[1]);
    for(std::vector<int>::size_type is=0; is<dai.solY.size(); ++is)
    {
        fprintf(outFile, " %i ", dai.solY[is]);
        fprintf(outFile, (is != dai.solY.size()-1) ? "," : "\n");
    }

    fprintf(outFile,"%-25.25s = ", axesNames[2]);
    for(std::vector<int>::size_type is=0; is<dai.solZ.size(); ++is)
    {
        fprintf(outFile, " %i ", dai.solZ[is]);
        fprintf(outFile, (is != dai.solZ.size()-1) ? "," : "\n");
    }

    fprintf(outFile,"\n# Set X, Y, Z axes for the bifurcation diagram:\n");
    fprintf(outFile,"# 0 is Time for X,Y,Z.\n");
    fprintf(outFile,"%-25.25s = ", axesNames[3]);
    for(std::vector<int>::size_type is=0; is<dai.bifX.size(); ++is)
    {
        fprintf(outFile, " %i ", dai.bifX[is]);
        fprintf(outFile, (is != dai.bifX.size()-1) ? "," : "\n");
    }

    fprintf(outFile,"%-25.25s = ", axesNames[4]);
    for(std::vector<int>::size_type is=0; is<dai.bifY.size(); ++is)
    {
        fprintf(outFile, " %i ", dai.bifY[is]);
        fprintf(outFile, (is != dai.bifY.size()-1) ? "," : "\n");
    }

    fprintf(outFile,"%-25.25s = ", axesNames[5]);
    for(std::vector<int>::size_type is=0; is<dai.bifZ.size(); ++is)
    {
        fprintf(outFile, " %i ", dai.bifZ[is]);
        fprintf(outFile, (is != dai.bifZ.size()-1) ? "," : "\n");
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
static bool
parseFileName(const char *fname, std::string& sFileName,
              std::string& bFileName, std::string& dFileName)
//
////////////////////////////////////////////////////////////////////////
{
    std::stringstream filename(fname);
    std::string myPath, myName, path;

    while(getline(filename, path, '/'))
    {
        myPath += "/";
        myPath += path;
        myName = path;
    }

    sFileName = myPath;
    bFileName = myPath;
    dFileName = myPath;

    int j = myPath.length();
    int i = myName.length();
    sFileName[j-i]='s'; sFileName[j-i+1]='.';
    bFileName[j-i]='b'; bFileName[j-i+1]='.';
    dFileName[j-i]='d'; dFileName[j-i+1]='.';

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
    std::string sFileName, bFileName, dFileName;

    postDeals();
    parseFileName(filename, sFileName, bFileName, dFileName);
    bool rs = readSolutionAndBifurcationData(false, sFileName, bFileName,
                                             dFileName);
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

