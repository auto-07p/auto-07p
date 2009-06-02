#include "gplaut04.h"
#include "tube.h"

#if 0
static SoSeparator * animateOrbitCalSteps(long int n,long int si);
static SoSeparator * animateOrbitWithNurbsCurveTail(long int j,long int si);
static SoSeparator * animateIner2(long int j, long int si);
static SoSeparator * animateOrbitMovement(long int n, long int si);
static SoSeparator * inertialSystemAnimation(int coordSys, SolNode mySolNode, \
float numAnimatedPeriod, int kth, int idx, float mass, \
float distance, float sPrimPeriod, float  gravity);
#endif
static SoSeparator * createSolutionInertialFrameScene(float dis);

float libPtScaler = 1.0;
float smallPrimRadius=1.0;
float largePrimRadius=1.0;
int   numOfStars = 100;
float diskTransparency = 0;
bool diskFromFile = false;
static float distance = 1;
static float sPrimPeriod  = 31558118.4;
static float gravity = 9.18;
double mass = 0.0;
bool blMassDependantOption = false;

SoSeparator *createR3BPoints(float nodemin[], float nodemax[])
{
    SoSeparator *result = new SoSeparator;
    float dis = max(max((nodemax[0]-nodemin[0]),(nodemax[1]-nodemin[1])),
                    (nodemax[2]-nodemin[2]));

    if(whichType == SOLUTION && whichCoordSystem != ROTATING_F)
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
        return result;
    }

    if(whichType == BIFURCATION) dis = 1.0;
    if(options[OPT_LIB_POINTS])
    {
        SoSeparator * libPtsSep = createLibrationPoint(mass, dis, libPtScaler,
                                                       whichCoordSystem);
        result->addChild(libPtsSep);
    }

// create reference DISK
    if(options[OPT_REF_PLAN])
    {
        float position[3];
        if(whichType == BIFURCATION)
        {
            position[0]=position[1]=position[2]=0;
        }
        else
        {
            position[0]=-mass;
            position[1]=position[2]=0;
        }
        SoSeparator *diskSep = createDisk(position,1.0);
        result->addChild(diskSep);
    }

// create the primaries
    if(options[OPT_PRIMARY])
    {
        double pos1 = 1-mass;
        double pos2 = -mass;
        char *txtureFileName = new char [strlen(autoDir) + 30];
        sprintf(txtureFileName, "%s%s", autoDir, "/plaut04/widgets/large.jpg");
        result->addChild(createPrimary(1-mass, pos2, 0.25*largePrimRadius, txtureFileName));
        sprintf(txtureFileName, "%s%s", autoDir, "/plaut04/widgets/small.jpg");
        result->addChild(createPrimary(mass, pos1, 0.25*smallPrimRadius, txtureFileName));
        delete [] txtureFileName;
    }
    return result;
}

///////////////////////////////////////////////////////////////////////////
//
//   Using a red ball to simulate the movement of a sattelite and using
//   white lines to simulate the trace of the sattelite.
//
static SoSeparator *
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
                iOrbit, bMax, (bMax+bMin)/2.0, bMin,
                stability, scaler));
        }
        else if(coloringMethod >= mySolNode.nar)
            satGroup->addChild(setLineAttributesByParameterValue(
                    mySolNode.par[iOrbit][mySolNode.parID[coloringMethod-mySolNode.nar]],
                    mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                    mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                    mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                    stability, scaler));
        else
            satGroup->addChild(setLineColorBlending(myColorBase, size,
                stability, scaler));
        satGroup->addChild(myLine);
    }

    SoMaterial * satMtl = new SoMaterial;
    SoSphere * mySat = new SoSphere;
    mySat->radius = dis*0.005*satRadius;

    SoTranslation * satTrans = new SoTranslation;
    satMtl->diffuseColor.setValue(envColors[7]);
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
    smallPrimLineMtl->diffuseColor.setValue(envColors[11]);
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
    largePrimLineMtl->diffuseColor.setValue(envColors[9]);
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
            iOrbit, bMax, (bMax+bMin)/2.0, bMin,
            stability, scaler));
    }
    else if(coloringMethod >= mySolNode.nar)
        anOrbit->addChild(setLineAttributesByParameterValue(
                mySolNode.par[iOrbit][mySolNode.parID[coloringMethod-mySolNode.nar]],
                mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                stability, scaler));
    else
        anOrbit->addChild(setLineColorBlending(myColorBase, arrSize,
            stability, scaler));
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
            iOrbit, bMax, (bMax+bMin)/2.0, bMin,
            stability,  tubeRadiusScaler));
    }
    else if(coloringMethod >= mySolNode.nar)
        anOrbit->addChild(setLineAttributesByParameterValue(
                mySolNode.par[iOrbit][mySolNode.parID[coloringMethod-mySolNode.nar]],
                mySolNode.parMax[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMid[iBranch][coloringMethod-mySolNode.nar],
                mySolNode.parMin[iBranch][coloringMethod-mySolNode.nar],
                stability, tubeRadiusScaler));
    else
        anOrbit->addChild(setLineColorBlending(myColorBase, arrSize*11,
            stability, tubeRadiusScaler));

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
            iOrbit, bMax, (bMax+bMin)/2.0, bMin,
            stability, scaler));
    }
    else if(coloringMethod >= mySolNode.nar)
        anOrbit->addChild(setLineAttributesByParameterValue(
                mySolNode.par[iOrbit][mySolNode.parID[coloringMethod-mySolNode.nar]],
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
    int stability, type;
    float satPeriod = 1;
    long int  arrSize = 0;

    if(animationLabel == MY_ALL)
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

            convertDataToInertialSystem(myVertices, time, myColorBase, arrSize, orbitSize, k, si);
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
            while(kno<mySolNode.numOrbits && myLabels[kno]<animationLabel)
            {
                si += mySolNode.numVerticesEachPeriod[kno];
                if(kno >= sumOrbit)
                {
                    curBranchID = mySolNode.branchID[++iBranch];
                    sumOrbit   += mySolNode.numOrbitsInEachBranch[iBranch];
                }
            }

            satPeriod = clientData.solPeriod[kno];
            long int orbitSize = mySolNode.numVerticesEachPeriod[kno];
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
        char *txtureFileName = new char [strlen(autoDir) + 30];
        sprintf(txtureFileName, "%s%s", autoDir, "/plaut04/widgets/large.rgb");
        aSep->addChild(createPrimary(1-mass, pos1, 0.25*largePrimRadius, txtureFileName));
        sprintf(txtureFileName, "%s%s", autoDir, "/plaut04/widgets/small.rgb");
        aSep->addChild(createPrimary(mass, pos2, 0.25*smallPrimRadius, txtureFileName));
        delete [] txtureFileName;
    }

// create the libration points
    SoSeparator * libPtsSep = createLibrationPoint(mass, dis, libPtScaler,
                                                   whichCoordSystem);
    if(options[OPT_LIB_POINTS])
    {
        aSep->addChild(libPtsSep);
    }

// create solution coordinate axis
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

        coordSep->addChild(createCoordinates(setShow3D, cdtype, asMax, asMin, tickers, whichCoord, &envColors[1]));

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

            convertDataToInertialSystem(myVertices, time, myColorBase, arrSize, orbitSize, k, si);

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


#if 0
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
    satMtl->diffuseColor.setValue(envColors[7]);
    satMtl->transparency = 0.0;

    SoDrawStyle *satStyle = new SoDrawStyle;
    satStyle->style = SoDrawStyle::FILLED;
    satGroup->addChild(satStyle);

    SoSeparator * satSep = new SoSeparator;
    SoBlinker *satBlker = new SoBlinker;

    int upperlimit = mySolNode.numVerticesEachPeriod[lblJ];
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


///////////////////////////////////////////////////////////////////////////
//
//   Using a red sphere to simulate the movement of a sattelite.
//
SoSeparator *
animateOrbitMovement(long int j, long int si)
//
///////////////////////////////////////////////////////////////////////////
{
    float ptb[3];
    SoSeparator *satGroup = new SoSeparator;

    SoMaterial *satMtl = new SoMaterial;
    satMtl->diffuseColor.setValue(1.0,0.0,0.0);
    satMtl->transparency = 0.0;

    SoDrawStyle *satStyle = new SoDrawStyle;
    satStyle->style = SoDrawStyle::FILLED;
    satGroup->addChild(satStyle);

    SoBlinker *satBlker = new SoBlinker;

    int upperlimit = mySolNode.numVerticesEachPeriod[j];
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
#endif


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


#if 0
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
    float ptb[3];
    SoSeparator *satGroup = new SoSeparator;
    SoMaterial *satMtl = new SoMaterial;
    satMtl->diffuseColor.setValue(envColors[7]);
    satMtl->transparency = 0.0;

    SoDrawStyle *satStyle = new SoDrawStyle;
    satStyle->style = SoDrawStyle::FILLED;
    satGroup->addChild(satStyle);

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
    SoSeparator *satGroup = new SoSeparator;
    SoMaterial *tailMtl = new SoMaterial;
    tailMtl->diffuseColor.setValue(1.0,1.0,1.0);
    tailMtl->transparency = 0.0;

    SoDrawStyle *satStyle = new SoDrawStyle;
    satStyle->style = SoDrawStyle::FILLED;
    satGroup->addChild(satStyle);

    int upperlimit = mySolNode.numVerticesEachPeriod[j];
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
    satMtl->diffuseColor.setValue(envColors[7]);
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
    animationLabel = MY_ALL;
    orbitSpeed = 1.0;
    satSpeed   = 0.5;
}

#if 0
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
#endif
