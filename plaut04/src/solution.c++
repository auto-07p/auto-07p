#include "solution.h"

#include "createCoords.h"
#include "normalizeSolData.h"
#include "tube.h"

SolNode mySolNode;

const float STATIONARY_POINT_RADIUS = 0.01;

static int maxComponent = 1;
static int curComponent = 1;
static int time_on = 0;

////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
//
// Routine to create a scene graph representing an auto solution
//
SoSeparator *
Solution::createSceneWithWidgets()
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
        result->addChild(render());

    }
    return result;
}

//////////////////////////////////////////////////////////////////////////
//
// Description: Draw the whole solution family using TUBES.
// Draw the whole solution family using TUBES.
//
SoSeparator *
Solution::drawUsingTubes()
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator * tubeSep = new SoSeparator;

    float dis = (!options[OPT_NORMALIZE_DATA]) ? (std::max(std::max(
        fabs(mySolNode.max[0]-mySolNode.min[0]),
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
                        j, mySolNode.totalLabels, mySolNode.totalLabels/2.0, 0,
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
                            colorBase[i*11+k]  = mySolNode.data[idx][coloringMethod];
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
                            j, mySolNode.totalLabels, mySolNode.totalLabels/2.0, 0,
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
Solution::drawABranchUsingSurface(long obStart, long obEnd, long numVert)
//
//////////////////////////////////////////////////////////////////////////
{
    float dis = (!options[OPT_NORMALIZE_DATA]) ? (std::max(std::max(
        fabs(mySolNode.max[0]-mySolNode.min[0]),
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
Solution::renderTubes()
//
//////////////////////////////////////////////////////////////////////////
{
    SoGroup * solGroup = new SoGroup;

// draw every orbit by using giving tube thickness and color.
    if(animationLabel == MY_ALL)
        solGroup->addChild(drawUsingTubes());
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
        solGroup->addChild(animateUsingTubes(aniColoring));
    }
    return solGroup;
}


//////////////////////////////////////////////////////////////////////////
//
SoGroup *
Solution::renderSurface()
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
        SoSeparator * as = drawABranchUsingSurface(start+1, end, numVert);
        if(as !=NULL)
            solGroup->addChild(as);
        start += mySolNode.numOrbitsInEachBranch[iBranch];
        sumOrbit += mySolNode.numOrbitsInEachBranch[iBranch];
    }

    if(options[OPT_PERIOD_ANI])
        solGroup->addChild(animateUsingTubes(true));

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
Solution::renderPoints(int style)
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
        solGroup->addChild(animateUsingPoints(style, coloring));
    }

    return solGroup;
}


///////////////////////////////////////////////////////////////////////
//
//           draw the solutions by lines
//
SoGroup *
Solution::renderLines()
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
        solGroup->addChild(animateUsingLines(coloring));
    }

    return solGroup;
}


///////////////////////////////////////////////////////////////////////
//
//           draw the solutions using NURBS CURVE lines
//
SoGroup *
Solution::renderNurbsCurve()
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
        solGroup->addChild(animateUsingLines(coloring));
    }
    return solGroup;
}


//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
Solution::animateUsingPoints(int style, bool aniColoring)
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
Solution::animateUsingLines(bool aniColoring)
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


#if 0
//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
Solution::animateUsingNurbsCurve()
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
#endif

/////////////////////////////////////////////////////////////////
//                  create solution orbits scene
SoSeparator *
Solution::render()
//
/////////////////////////////////////////////////////////////////
{
    SoSeparator *solSep = new SoSeparator;
    SoGroup *solGroup = new SoGroup;

    if(whichStyle==TUBE)
    {
        solGroup->addChild(renderTubes());
    }
    else if(whichStyle==SURFACE)
    {
        solGroup->addChild(renderSurface());
    }
    else if(whichStyle==NURBS) 
    {
        solGroup->addChild(renderNurbsCurve());
    }
    else if(whichStyle==MESH_POINTS || whichStyle== ALL_POINTS)
    {
        solGroup->addChild(renderPoints(whichStyle));
    }
    else 
    {
        solGroup->addChild(renderLines());
    }
    solSep->addChild(solGroup);
    return solSep;
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
Solution::drawAnOrbitUsingLines(int iBranch,  long int l, long int si, 
       float scaler, int stability, int type, bool aniColoring)
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator * anOrbit = new SoSeparator;

    float dis = !options[OPT_NORMALIZE_DATA] ? 
	               (std::max(std::max(
        fabs(mySolNode.max[0]-mySolNode.min[0]),
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
                l, mySolNode.totalLabels, mySolNode.totalLabels/2.0, 0,
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
        if(coloringMethod>=0)colorBase[m]  = mySolNode.data[idx][coloringMethod];
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
                          l, mySolNode.totalLabels, mySolNode.totalLabels/2.0, 0,
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
Solution::drawAnOrbitUsingPoints(int style, int iBranch,  long int l, 
     long int si, float scaler, int stability, int type, bool aniColoring)
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator * anOrbit = new SoSeparator;

    float dis = !options[OPT_NORMALIZE_DATA] ? (std::max(std::max(
        fabs(mySolNode.max[0]-mySolNode.min[0]),
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
                l, mySolNode.totalLabels, mySolNode.totalLabels/2.0, 0,
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
        if(coloringMethod>=0)colorBase[m]  = mySolNode.data[idx][coloringMethod];
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
                              l, mySolNode.totalLabels, mySolNode.totalLabels/2.0, 0,
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
Solution::drawAnOrbitUsingNurbsCurve(int iBranch, long int l, long int si,
                                     float scaler, int stability, int type)
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
           l, mySolNode.totalLabels, mySolNode.totalLabels/2.0, 0,
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
Solution::drawAnOrbitUsingTubes(int iBranch, long int l, long int si,
                                float scaler, int stability, int type)
//
//////////////////////////////////////////////////////////////////////////
{
    float dis = !options[OPT_NORMALIZE_DATA] ? (std::max(std::max(
        fabs(mySolNode.max[0]-mySolNode.min[0]),
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
                l, mySolNode.totalLabels, mySolNode.totalLabels/2.0, 0,
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
                colorBase[m*11+j]  = mySolNode.data[si+m][coloringMethod];
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
                 l, mySolNode.totalLabels, mySolNode.totalLabels/2.0, 0,
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
// animate solution by using tubes. 
// animate solution by using tubes. This version use much memory and
// much slower.
//
SoSeparator *
Solution::animateUsingTubes(bool aniColoring)
//
///////////////////////////////////////////////////////////////////////////
{
    long int sumX = 0;
    SoSeparator *solGroup = new SoSeparator;

    float dis = !options[OPT_NORMALIZE_DATA] ? (std::max(std::max(
        fabs(mySolNode.max[0]-mySolNode.min[0]),
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
                        colorBase[i*11+j]  = mySolNode.data[idx][coloringMethod];
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
                    j, mySolNode.totalLabels, mySolNode.totalLabels/2.0, 0,
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


///////////////////////////////////////////////////////////////////////////
//   Using a red ball to simulate the movement of a sattelite and using
//   white lines to simulate the trace of the sattelite.
//
SoSeparator *
Solution::animateOrbitWithTail(int iBranch, long int j, long int si)
//
///////////////////////////////////////////////////////////////////////////
{
    SoSeparator *satGroup = new SoSeparator;

    float distance = !options[OPT_NORMALIZE_DATA] ? (std::max(std::max(
        fabs(mySolNode.max[0]-mySolNode.min[0]),
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

    float dis = distance;// fabs(std::max(std::max((maxV[0]-minV[0]), (maxV[1]-minV[1])), (maxV[2]-minV[2])));
    float (*myVertices)[3]= new float[arrSize+1][3];
    float *myColorBase = new float [arrSize+1];

    myVertices[0][0] = myVertices[arrSize][0] = mySolNode.xyzCoords[idx][0];
    myVertices[0][1] = myVertices[arrSize][1] = mySolNode.xyzCoords[idx][1];
    myVertices[0][2] = myVertices[arrSize][2] = mySolNode.xyzCoords[idx][2];
    if(coloringMethod>=0)myColorBase[0]  = mySolNode.data[idx][coloringMethod];
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

        if(coloringMethod>=0)myColorBase[i]  = mySolNode.data[idx+m][coloringMethod];
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
                j-1, mySolNode.totalLabels, mySolNode.totalLabels/2.0, 0,
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

////////////////////////////////////////////////////////////////////////
//
//
//
void
Solution::copyDataToWorkArray(int  varIndices[], int cur, int mx, int to)
//
////////////////////////////////////////////////////////////////////////
{
    curComponent = cur;
    maxComponent = mx;
    time_on = to;
    for(int k=0; k<3; k++)
    {
        for(long int row=0; row<mySolNode.totalNumPoints; ++row)
        {
            mySolNode.time[row] = mySolNode.data[row][0];
            if(varIndices[k]>=0)
            {
                float dummy = mySolNode.data[row][varIndices[k]];
                mySolNode.xyzCoords[row][k] = dummy;
            }
            else if(varIndices[k]<0)
            {
                mySolNode.xyzCoords[row][k]=0.0;
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
Solution::drawAPoint(float x, float y, float z, float size, float scale)
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

//////////////////////////////// START ///////////////////////////////
//
// draw a strip by giving the stripset
SoSeparator *
Solution::drawAStrip(float stripSet[][3], int size)
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
