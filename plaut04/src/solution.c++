#include "solution.h"

#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <ctype.h>
#include <math.h>

#include "gplaut04.h"
#include "createCoords.h"
#include "tube.h"
#include "rounding.h"

#define MAX_LINE_LENGTH 256

extern float *tv;
extern int whichCoordSystem;
extern UserData clientData;

Solution *mySolNode;

const float STATIONARY_POINT_RADIUS = 0.01;

static int maxComponent = 1;
static int curComponent = 1;
static int time_on = 0;

const float distance = 1;
const float sPrimPeriod  = 31558118.4;
const float gravity = 9.18;

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
        normalizeData();
    }

    if(useR3B)
        result->addChild(createR3BPoints(min_, max_));
    if(!useR3B || whichCoordSystem == ROTATING_F)
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
                asMin[0]=min_[0]; asMin[1]=min_[1]; asMin[2]=min_[2];
                asMax[0]=max_[0]; asMax[1]=max_[1]; asMax[2]=max_[2];
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
        fabs(max_[0]-min_[0]),
        fabs(max_[1]-min_[1])),
        fabs(max_[2]-min_[2]))) : 2;

    long int sumX = 0;
    int iBranch = 0;
    int curBranchID = branchID_[iBranch];
    int sumOrbit    = numOrbitsInEachBranch_[iBranch];

    for(long int j=0; j<numOrbits_; ++j)
    {
        if(j >= sumOrbit)
        {
            curBranchID = branchID_[++iBranch];
            sumOrbit   += numOrbitsInEachBranch_[iBranch];
        }
        long int upperlimit = orbits_[j].numVerticesEachPeriod;

        if(upperlimit >0 )
        {
            if(upperlimit == 1)
            {
                int idx = sumX;
                SoSeparator * ptSep = new SoSeparator;

                int stability=clientData.labelIndex[j][3];
                int type =clientData.labelIndex[j][2];
                float scaler = lineWidthScaler;

                if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
                    ptSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
                else if(coloringMethod == CL_STABILITY)
                    ptSep->addChild(setLineAttributesByStability(stability, scaler));
                else if(coloringMethod == CL_ORBIT_TYPE)
                    ptSep->addChild(setLineAttributesByType(stability, type, scaler));
                else if(coloringMethod == CL_LABELS)
                {
                    ptSep->addChild(setLineAttributesByParameterValue(
                        j, totalLabels_, totalLabels_/2.0, 0,
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
			//-(max_[0]-min_[0]) : 0;
                        ver[0][1] = xyzCoords_[idx][1];
                        ver[0][2] = 0;
                        ver[1][0] = (max_[0] <= 1.0) ? 1.0 : max_[0];
                        ver[1][1] = xyzCoords_[idx][1];
                        ver[1][2] = 0;
                    }
                    else if(time_on == TIME_ON_Y)
                    {
                        ver[0][0] = xyzCoords_[idx][0];
                        ver[0][1] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0;
			//-(max_[1]-min_[1]) : 0;
                        ver[0][2] = 0;
                        ver[1][0] = xyzCoords_[idx][0]; 
                        ver[1][1] = ( max_[1] <= 1.0 ) ? 1.0 : max_[1];
                        ver[1][2] = 0;
                    }
                    else if(time_on == TIME_ON_Z)
                    {
                        ver[0][0] = xyzCoords_[idx][0];
                        ver[0][1] = 0; 
                        ver[0][2] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0; 
			//-(max_[2]-min_[2]) : 0;
                        ver[1][0] = xyzCoords_[idx][0];
                        ver[1][1] = 0; 
                        ver[1][2] = ( max_[2] <= 1.0 ) ? 1.0 : max_[2]; 
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
                    aTrans->translation.setValue(xyzCoords_[idx][0], 
        		           xyzCoords_[idx][1], xyzCoords_[idx][2]);
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
                    path[i][0]=xyzCoords_[idx][0];
                    path[i][1]=xyzCoords_[idx][1];
                    path[i][2]=xyzCoords_[idx][2];
                    if(coloringMethod>=0)
                        for(int k=0; k<11; ++k)
                            colorBase[i*11+k]  = data_[idx][coloringMethod];
                    if(coloringMethod==CL_POINT_NUMBER)
                        for(int k=0; k<11; ++k)
                            colorBase[i*11+k]  = i;

                }
                int stability=clientData.labelIndex[j][3];
                int type =clientData.labelIndex[j][2];
                float scaler = lineWidthScaler;

                if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
                    tubeSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
                else if(coloringMethod == CL_STABILITY)
                    tubeSep->addChild(setLineAttributesByStability(stability, scaler));
                else if(coloringMethod == CL_ORBIT_TYPE)
                    tubeSep->addChild(setLineAttributesByType(stability, type, scaler));
                else if(coloringMethod == CL_LABELS)
                    tubeSep->addChild(setLineAttributesByParameterValue(
                            j, totalLabels_, totalLabels_/2.0, 0,
                            stability, scaler));
                else if(coloringMethod == CL_COMPONENT)
                    tubeSep->addChild(setLineAttributesByParameterValue(
                            curComponent, maxComponent, maxComponent/2.0, 0,
                            stability, scaler));
                else if(coloringMethod >= nar_)
                    tubeSep->addChild(setLineAttributesByParameterValue(
                          orbits_[j].par[parID_[coloringMethod-nar_]],
                          parMax_[iBranch][coloringMethod-nar_],
                          parMid_[iBranch][coloringMethod-nar_],
                          parMin_[iBranch][coloringMethod-nar_],
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
        fabs(max_[0]-min_[0]),
        fabs(max_[1]-min_[1])),
        fabs(max_[2]-min_[2]))) : 2.0;

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
                strip[i*2][0] = xyzCoords_[i][0];
                strip[i*2][1] = xyzCoords_[i][1];
                strip[i*2][2] = xyzCoords_[i][2];
            }
        }
        else
        {
	    printf(" Only one point in the period, no surface can be drawn!\n");
            long int idx = obStart;
            SoSeparator * ptSep = new SoSeparator;
            SoTransform * aTrans = new SoTransform;
            aTrans->translation.setValue(xyzCoords_[idx][0], 
			            xyzCoords_[idx][1], xyzCoords_[idx][2]);
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
            npt= orbits_[j].numVerticesEachPeriod;
            if(npt>1)
            {
                for(int i=0; i<npt; i++)
                {
                    strip[i*2+1][0] = xyzCoords_[i+sum][0];
                    strip[i*2+1][1] = xyzCoords_[i+sum][1];
                    strip[i*2+1][2] = xyzCoords_[i+sum][2];
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
                aTrans->translation.setValue(xyzCoords_[idx][0], 
				           xyzCoords_[idx][1], xyzCoords_[idx][2]);
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
        for(std::vector<int>::size_type n=0; n<lblIndices.size(); ++n)
        {
            animationLabel=myLabels[lblIndices[n]];

            int si = 0, k = 0;
            int iBranch = 0;
            int curBranchID = branchID_[iBranch];
            int sumOrbit    = numOrbitsInEachBranch_[iBranch];

            for(int ka=0; ka<numOrbits_; ka++)
            {
                if(ka >= sumOrbit)
                {
                    curBranchID = branchID_[++iBranch];
                    sumOrbit   += numOrbitsInEachBranch_[iBranch];
                }
                if(myLabels[ka]>=animationLabel) break;
                k = k+1;
                si += orbits_[ka].numVerticesEachPeriod;
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
    for(int iBranch = 0; iBranch < numBranches_; ++iBranch)
    {
        end += numOrbitsInEachBranch_[iBranch];
        numVert = orbits_[sumOrbit].numVerticesEachPeriod;
        SoSeparator * as = drawABranchUsingSurface(start+1, end, numVert);
        if(as !=NULL)
            solGroup->addChild(as);
        start += numOrbitsInEachBranch_[iBranch];
        sumOrbit += numOrbitsInEachBranch_[iBranch];
    }

    if(options[OPT_PERIOD_ANI])
        solGroup->addChild(animateUsingTubes(true));

    int si = 0, k = 0;
    int iBranch = 0;
    int curBranchID = branchID_[iBranch];
    sumOrbit    = numOrbitsInEachBranch_[iBranch];
    for(int ka=0; ka<numOrbits_; ka++)
    {
        if(ka >= sumOrbit)
        {
            curBranchID = branchID_[++iBranch];
            sumOrbit   += numOrbitsInEachBranch_[iBranch];
        }
        if(myLabels[ka]>=animationLabel) break;
        k = k+1;
        si += orbits_[ka].numVerticesEachPeriod;
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
        int curBranchID = branchID_[iBranch];
        int sumOrbit = numOrbitsInEachBranch_[iBranch];

        for(long int ka=0; ka<numOrbits_; ka++)
        {
            if(ka >= sumOrbit)
            {
                curBranchID = branchID_[++iBranch];
                sumOrbit+= numOrbitsInEachBranch_[iBranch];
            }

            solGroup->addChild(drawAnOrbitUsingPoints(style, iBranch, /*curBranchID,*/ k, si, lineWidthScaler,
                clientData.labelIndex[k][3], clientData.labelIndex[k][2], true));
            k = k+1;
            si += orbits_[ka].numVerticesEachPeriod;
        }
    }
    else if(animationLabel != MY_NONE)
    {
        for(std::vector<int>::size_type n=0; n<lblIndices.size(); ++n)
        {
            animationLabel=myLabels[lblIndices[n]];
            int si = 0, k = 0;
            int iBranch = 0;
            int curBranchID = branchID_[iBranch];
            int sumOrbit    = numOrbitsInEachBranch_[iBranch];
            for(int ka=0; ka<numOrbits_; ka++)
            {
                if(ka >= sumOrbit)
                {
                    curBranchID = branchID_[++iBranch];
                    sumOrbit+= numOrbitsInEachBranch_[iBranch];
                }

                if(myLabels[ka]>=animationLabel) break;
                k = k+1;
                si += orbits_[ka].numVerticesEachPeriod;
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
        int curBranchID = branchID_[iBranch];
        int sumOrbit = numOrbitsInEachBranch_[iBranch];

        for(long int ka=0; ka<numOrbits_; ka++)
        {
            if(ka >= sumOrbit)
            {
                curBranchID = branchID_[++iBranch];
                sumOrbit+= numOrbitsInEachBranch_[iBranch];
            }

            solGroup->addChild(drawAnOrbitUsingLines(iBranch, k, si, lineWidthScaler,
                clientData.labelIndex[k][3], clientData.labelIndex[k][2], true));
            k = k+1;
            si += orbits_[ka].numVerticesEachPeriod;
        }
    }
    else if(animationLabel != MY_NONE)
    {
        for(std::vector<int>::size_type n=0; n<lblIndices.size(); ++n)
        {
            animationLabel=myLabels[lblIndices[n]];
            int si = 0, k = 0;
            int iBranch = 0;
            int curBranchID = branchID_[iBranch];
            int sumOrbit    = numOrbitsInEachBranch_[iBranch];
            for(int ka=0; ka<numOrbits_; ka++)
            {
                if(ka >= sumOrbit)
                {
                    curBranchID = branchID_[++iBranch];
                    sumOrbit+= numOrbitsInEachBranch_[iBranch];
                }

                if(myLabels[ka]>=animationLabel) break;
                k = k+1;
                si += orbits_[ka].numVerticesEachPeriod;
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
        int curBranchID = branchID_[iBranch];
        int sumOrbit    = numOrbitsInEachBranch_[iBranch];
        for(long int ka=0; ka<numOrbits_; ka++)
        {
            if(ka >= sumOrbit)
            {
                curBranchID = branchID_[++iBranch];
                sumOrbit   += numOrbitsInEachBranch_[iBranch];
            }
            solGroup->addChild(drawAnOrbitUsingNurbsCurve(iBranch, k+1, si, lineWidthScaler,
                clientData.labelIndex[k][3], clientData.labelIndex[k][2]));
            k = k+1;
            si += orbits_[ka].numVerticesEachPeriod;
        }
    }
    else if(animationLabel != MY_NONE)
    {
        for(std::vector<int>::size_type n=0; n<lblIndices.size(); ++n)
        {
            animationLabel=myLabels[lblIndices[n]];
            int si = 0, k = 0;
            int iBranch = 0;
            int curBranchID = branchID_[iBranch];
            int sumOrbit    = numOrbitsInEachBranch_[iBranch];
            for(int ka=0; ka<numOrbits_; ka++)
            {
                if(ka >= sumOrbit)
                {
                    curBranchID = branchID_[++iBranch];
                    sumOrbit   += numOrbitsInEachBranch_[iBranch];
                }
                if(myLabels[ka]>=animationLabel) break;
                k = k+1;
                si += orbits_[ka].numVerticesEachPeriod;
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
    int curBranchID = branchID_[iBranch];
    int sumOrbit = numOrbitsInEachBranch_[iBranch];
    for(int l=0; l<numOrbits_; l++)
    {
        long numVertices = orbits_[l].numVerticesEachPeriod;
        if(l >= sumOrbit)
        {
            curBranchID = branchID_[++iBranch];
            sumOrbit+= numOrbitsInEachBranch_[iBranch];
        }

        if(numVertices == 1)
            solStand->addChild(drawAnOrbitUsingPoints(style, iBranch,/* curBranchID,*/ l, si, aniLineScaler*lineWidthScaler,
                clientData.labelIndex[l][3], clientData.labelIndex[l][2], aniColoring));
        else
            solBlinker->addChild(drawAnOrbitUsingPoints(style, iBranch,/* curBranchID,*/ l, si, aniLineScaler*lineWidthScaler,
                clientData.labelIndex[l][3], clientData.labelIndex[l][2], aniColoring));
        si+=orbits_[l].numVerticesEachPeriod;
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
    int curBranchID = branchID_[iBranch];
    int sumOrbit = numOrbitsInEachBranch_[iBranch];
    for(int l=0; l<numOrbits_; l++)
    {
        long numVertices = orbits_[l].numVerticesEachPeriod;
        if(l >= sumOrbit)
        {
            curBranchID = branchID_[++iBranch];
            sumOrbit+= numOrbitsInEachBranch_[iBranch];
        }

        if(numVertices == 1)
            solStand->addChild(drawAnOrbitUsingLines(iBranch, l, si, aniLineScaler*lineWidthScaler,
                clientData.labelIndex[l][3], clientData.labelIndex[l][2], aniColoring));
        else
            solBlinker->addChild(drawAnOrbitUsingLines(iBranch, l, si, aniLineScaler*lineWidthScaler,
                clientData.labelIndex[l][3], clientData.labelIndex[l][2], aniColoring));
        si+=orbits_[l].numVerticesEachPeriod;
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
    int curBranchID = branchID_[iBranch];
    int sumOrbit    = numOrbitsInEachBranch_[iBranch];
    for(long int l=0; l<numOrbits_; l++)
    {
        if(l >= sumOrbit)
        {
            curBranchID = branchID_[++iBranch];
            sumOrbit   += numOrbitsInEachBranch_[iBranch];
        }
        solBlinker->addChild(drawAnOrbitUsingNurbsCurve(iBranch, l, si, lineWidthScaler,
            clientData.labelIndex[l][3], clientData.labelIndex[l][2]));
        si+=orbits_[l].numVerticesEachPeriod;
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
        fabs(max_[0]-min_[0]),
        fabs(max_[1]-min_[1])),
        fabs(max_[2]-min_[2]))) : 2.0;

    long numVertices = orbits_[l].numVerticesEachPeriod;
    if(numVertices == 1 )
    {
        long int idx = si;
        SoSeparator * ptSep = new SoSeparator;

        if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
            ptSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
        else if(coloringMethod == CL_STABILITY)
            ptSep->addChild(setLineAttributesByStability(stability, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            ptSep->addChild(setLineAttributesByType(stability, type, scaler));
        else if(coloringMethod == CL_LABELS)
        {
            ptSep->addChild(setLineAttributesByParameterValue(
                l, totalLabels_, totalLabels_/2.0, 0,
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
		//-(max_[0]-min_[0]) : 0;
                ver[0][1] = xyzCoords_[idx][1];
                ver[0][2] = 0;
                ver[1][0] = (max_[0] <= 1.0) ? 1.0 : max_[0];
                ver[1][1] = xyzCoords_[idx][1];
                ver[1][2] = 0;
            }
            else if(time_on == TIME_ON_Y)
            {
                ver[0][0] = xyzCoords_[idx][0];
                ver[0][1] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0;
		//-(max_[1]-min_[1]) : 0;
                ver[0][2] = 0;
                ver[1][0] = xyzCoords_[idx][0]; 
                ver[1][1] = ( max_[1] <= 1.0 ) ? 1.0 : max_[1];
                ver[1][2] = 0;
            }
            else if(time_on == TIME_ON_Z)
            {
                ver[0][0] = xyzCoords_[idx][0];
                ver[0][1] = 0; 
                ver[0][2] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0;
		//-(max_[2]-min_[2]) : 0; 
                ver[1][0] = xyzCoords_[idx][0];
                ver[1][1] = 0; 
                ver[1][2] = ( max_[2] <= 1.0 ) ? 1.0 : max_[2]; 
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
            aTrans->translation.setValue(xyzCoords_[idx][0], 
		           xyzCoords_[idx][1], xyzCoords_[idx][2]);
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
        vertices[m][0]=xyzCoords_[idx][0];
        vertices[m][1]=xyzCoords_[idx][1];
        vertices[m][2]=xyzCoords_[idx][2];
        if(coloringMethod>=0)colorBase[m]  = data_[idx][coloringMethod];
        if(coloringMethod==CL_POINT_NUMBER)colorBase[m]  = m;
    }

    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, orbits_[l].numVerticesEachPeriod, vertices);
    int32_t  myint[10];
    myint[0]=orbits_[l].numVerticesEachPeriod;

// define the solution line set
    SoLineSet *myLine= new SoLineSet;
    myLine->numVertices.setValues(0,1,myint);
    if(!aniColoring)
    {
        anOrbit->addChild(setLineAttributesByType(stability, 0, scaler));
    }
    else if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
    else if(coloringMethod == CL_LABELS)
    {
        anOrbit->addChild(setLineAttributesByParameterValue(
                          l, totalLabels_, totalLabels_/2.0, 0,
                          stability, scaler));
    }
    else if(coloringMethod == CL_COMPONENT)
        anOrbit->addChild(setLineAttributesByParameterValue(
                          curComponent, maxComponent, maxComponent/2.0, 0,
                          stability, scaler));
    else if(coloringMethod >= nar_)
        anOrbit->addChild(setLineAttributesByParameterValue(
                          orbits_[l].par[parID_[coloringMethod-nar_]],
                          parMax_[iBranch][coloringMethod-nar_],
                          parMid_[iBranch][coloringMethod-nar_],
                          parMin_[iBranch][coloringMethod-nar_],
                          stability, scaler));
    else
        anOrbit->addChild(setLineColorBlending(colorBase,
                          orbits_[l].numVerticesEachPeriod,stability, scaler));

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
        fabs(max_[0]-min_[0]),
        fabs(max_[1]-min_[1])),
        fabs(max_[2]-min_[2]))) : 2.0;

    long numVertices = orbits_[l].numVerticesEachPeriod;
    if(numVertices == 1)
    {
        long int idx = si;
        SoSeparator * ptSep = new SoSeparator;
        SoTransform * aTrans = new SoTransform;

        if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
            ptSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
        else if(coloringMethod == CL_STABILITY)
            ptSep->addChild(setLineAttributesByStability(stability, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            ptSep->addChild(setLineAttributesByType(stability, type, scaler));
        else if(coloringMethod == CL_LABELS)
        {
            ptSep->addChild(setLineAttributesByParameterValue(
                l, totalLabels_, totalLabels_/2.0, 0,
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

        aTrans->translation.setValue(xyzCoords_[idx][0], 
		           xyzCoords_[idx][1], xyzCoords_[idx][2]);
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
        vertices[m][0]=xyzCoords_[idx][0];
        vertices[m][1]=xyzCoords_[idx][1];
        vertices[m][2]=xyzCoords_[idx][2];
        if(coloringMethod>=0)colorBase[m]  = data_[idx][coloringMethod];
        if(coloringMethod==CL_POINT_NUMBER)colorBase[m]  = m;

        if(!aniColoring)
        {
            anOrbit->addChild(setLineAttributesByType(stability, 0, scaler));
        }
        else if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
            anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
        else if(coloringMethod == CL_STABILITY)
            anOrbit->addChild(setLineAttributesByStability(stability, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
        else if(coloringMethod == CL_LABELS)
        {
            anOrbit->addChild(setLineAttributesByParameterValue(
                              l, totalLabels_, totalLabels_/2.0, 0,
                              stability, scaler));
        }
        else if(coloringMethod == CL_COMPONENT)
            anOrbit->addChild(setLineAttributesByParameterValue(
                              curComponent, maxComponent, maxComponent/2.0, 0,
                              stability, scaler));
        else if(coloringMethod >= nar_)
        {
            anOrbit->addChild(setLineAttributesByParameterValue(
                              orbits_[l].par[parID_[coloringMethod-nar_]],
                              parMax_[iBranch][coloringMethod-nar_],
                              parMid_[iBranch][coloringMethod-nar_],
                              parMin_[iBranch][coloringMethod-nar_],
                              stability, scaler));
        }
        else
            anOrbit->addChild(setLineColorBlending(colorBase,
                              orbits_[l].numVerticesEachPeriod,stability, scaler));

        if(style == MESH_POINTS)
        {
            if(m%orbits_[l].ncol == 0)
                anOrbit->addChild(drawAPoint(xyzCoords_[idx][0], xyzCoords_[idx][1],
                    xyzCoords_[idx][2], dis, STATIONARY_POINT_RADIUS*0.5));
        }else
        anOrbit->addChild(drawAPoint(xyzCoords_[idx][0], xyzCoords_[idx][1],
                xyzCoords_[idx][2], dis, STATIONARY_POINT_RADIUS*0.5));
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
    vertices = new float[orbits_[l].numVerticesEachPeriod][3];
    for(int m=0; m<orbits_[l].numVerticesEachPeriod; m++)
    {
        vertices[m][0]=xyzCoords_[si+m][0];
        vertices[m][1]=xyzCoords_[si+m][1];
        vertices[m][2]=xyzCoords_[si+m][2];
    }
    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, orbits_[l].numVerticesEachPeriod, vertices);
    myint[0]=orbits_[l].numVerticesEachPeriod;

    int number = orbits_[l].numVerticesEachPeriod;
    float * knots = new float[number+4];
    for (int i=0; i<4; ++i) knots[i]=0, knots[i+number]=number-3;
    for(int i=4; i<number; ++i) knots[i]=i-3;
    SoNurbsCurve *myCurve = new SoNurbsCurve;
    myCurve->numControlPoints = orbits_[l].numVerticesEachPeriod;
    myCurve->knotVector.setValues(0, number+4, knots);

    if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch,stability,scaler));
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_LABELS)
    {
        anOrbit->addChild(setLineAttributesByParameterValue(
           l, totalLabels_, totalLabels_/2.0, 0,
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
        fabs(max_[0]-min_[0]),
        fabs(max_[1]-min_[1])),
        fabs(max_[2]-min_[2]))) : 2.0 ;

    SoSeparator * anOrbit = new SoSeparator;
    long int numVertices = orbits_[l].numVerticesEachPeriod;
    if(numVertices == 1)
    {
        int idx = si;
        SoSeparator * ptSep = new SoSeparator;
        SoSphere * aPoint = new SoSphere;
        aPoint->radius = dis * STATIONARY_POINT_RADIUS;

        if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
            ptSep->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
        else if(coloringMethod == CL_STABILITY)
            ptSep->addChild(setLineAttributesByStability(stability, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            ptSep->addChild(setLineAttributesByType(stability, type, scaler));
        else if(coloringMethod == CL_LABELS)
        {
            anOrbit->addChild(setLineAttributesByParameterValue(
                l, totalLabels_, totalLabels_/2.0, 0,
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
                ver[0][0] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0 ; //-(max_[0]-min_[0]) : 0;
                ver[0][1] = xyzCoords_[idx][1];
                ver[0][2] = 0;
                ver[1][0] = (max_[0] <= 1.0) ? 1.0 : max_[0];
                ver[1][1] = xyzCoords_[idx][1];
                ver[1][2] = 0;
            }
            else if(time_on == TIME_ON_Y)
            {
                ver[0][0] = xyzCoords_[idx][0];
                ver[0][1] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0; //-(max_[1]-min_[1]) : 0;
                ver[0][2] = 0;
                ver[1][0] = xyzCoords_[idx][0]; 
                ver[1][1] = ( max_[1] <= 1.0 ) ? 1.0 : max_[1];
                ver[1][2] = 0;
            }
            else if(time_on == TIME_ON_Z)
            {
                ver[0][0] = xyzCoords_[idx][0];
                ver[0][1] = 0; 
                ver[0][2] = (options[OPT_NORMALIZE_DATA]) ? -1 : 0; //-(max_[2]-min_[2]) : 0; 
                ver[1][0] = xyzCoords_[idx][0];
                ver[1][1] = 0; 
                ver[1][2] = ( max_[2] <= 1.0 ) ? 1.0 : max_[2]; 
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
            aTrans->translation.setValue(xyzCoords_[idx][0], 
		           xyzCoords_[idx][1], xyzCoords_[idx][2]);
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

    float (*vertices)[3] = new float[orbits_[l].numVerticesEachPeriod][3];
    float *colorBase = new float[orbits_[l].numVerticesEachPeriod*11];
    Tube tube;
    for(int m=0; m<orbits_[l].numVerticesEachPeriod; m++)
    {
        vertices[m][0]=xyzCoords_[si+m][0];
        vertices[m][1]=xyzCoords_[si+m][1];
        vertices[m][2]=xyzCoords_[si+m][2];
        if(coloringMethod>=0)
            for(int j=0; j<11; ++j)
                colorBase[m*11+j]  = data_[si+m][coloringMethod];
        if(coloringMethod==CL_POINT_NUMBER)
            for(int j=0; j<11; ++j)
                colorBase[m*11+j]  = m;
    }
    tube = Tube(orbits_[l].numVerticesEachPeriod, vertices, lineWidthScaler*0.005, 10);

    if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
    else if(coloringMethod == CL_LABELS)
//          always set the first label blue, the last red, namely look all
//          branches as one.
        anOrbit->addChild(setLineAttributesByParameterValue(
                 l, totalLabels_, totalLabels_/2.0, 0,
                 stability, scaler));
    else if(coloringMethod == CL_COMPONENT)
        anOrbit->addChild(setLineAttributesByParameterValue(
                curComponent, maxComponent, maxComponent/2.0, 0,
                stability, scaler));
    else if(coloringMethod >= nar_)
        anOrbit->addChild(setLineAttributesByParameterValue(
                    orbits_[l].par[parID_[coloringMethod-nar_]],
                    parMax_[iBranch][coloringMethod-nar_],
                    parMid_[iBranch][coloringMethod-nar_],
                    parMin_[iBranch][coloringMethod-nar_],
                    stability, scaler));
    else
        anOrbit->addChild(setLineColorBlending(colorBase,
                orbits_[l].numVerticesEachPeriod*11,stability, scaler));

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
        fabs(max_[0]-min_[0]),
        fabs(max_[1]-min_[1])),
        fabs(max_[2]-min_[2]))) : 2.0;

    SoBlinker *tubeBlker = new SoBlinker;
    tubeBlker->speed = orbitSpeed;
    int iBranch = 0;
    int curBranchID = branchID_[iBranch];
    int sumOrbit    = numOrbitsInEachBranch_[iBranch];
    for(int j=0; j<numOrbits_; j++)
    {
        long int upperlimit = orbits_[j].numVerticesEachPeriod;
        if(upperlimit == 1)
        {
            int idx = sumX;
            SoSeparator * ptSep = new SoSeparator;
            SoSphere * aPoint = new SoSphere;
            aPoint->radius = dis * STATIONARY_POINT_RADIUS;

            SoTransform * aTrans = new SoTransform;
            aTrans->translation.setValue(xyzCoords_[idx][0],
                                         xyzCoords_[idx][1],
                                         xyzCoords_[idx][2]);
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
                curBranchID = branchID_[++iBranch];
                sumOrbit   += numOrbitsInEachBranch_[iBranch];
            }

            for(int i=0; i<upperlimit; i++)
            {
                int idx = i+sumX;
                path[i][0]=xyzCoords_[idx][0];
                path[i][1]=xyzCoords_[idx][1];
                path[i][2]=xyzCoords_[idx][2];
                if(coloringMethod>=0)
                    for(int j=0; j<11; ++j)
                        colorBase[i*11+j]  = data_[idx][coloringMethod];
                if(coloringMethod==CL_POINT_NUMBER)
                    for(int j=0; j<11; ++j)
                        colorBase[i*11+j]  = i;
            }

            if(!aniColoring)
                anOrbit->addChild(setLineAttributesByType(stability, 0, lineWidthScaler));
            else if(coloringMethod == CL_STABILITY)
                anOrbit->addChild(setLineAttributesByStability(stability, lineWidthScaler));
            else if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
                anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, lineWidthScaler));
            else if(coloringMethod == CL_ORBIT_TYPE)
                anOrbit->addChild(setLineAttributesByType(stability, type, lineWidthScaler));
            else if(coloringMethod == CL_LABELS)
                anOrbit->addChild(setLineAttributesByParameterValue(
                    j, totalLabels_, totalLabels_/2.0, 0,
                    stability, lineWidthScaler));
            else if(coloringMethod >= nar_)
                anOrbit->addChild(setLineAttributesByParameterValue(
                        orbits_[j].par[parID_[coloringMethod-nar_]],
                        parMax_[iBranch][coloringMethod-nar_],
                        parMid_[iBranch][coloringMethod-nar_],
                        parMin_[iBranch][coloringMethod-nar_],
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
        fabs(max_[0]-min_[0]),
        fabs(max_[1]-min_[1])),
        fabs(max_[2]-min_[2]))) : 2.0;
    int stability = clientData.labelIndex[j][3];
    int type = clientData.labelIndex[j][2];

    long int upperlimit = orbits_[j].numVerticesEachPeriod;
    long int idx = si; 
    if(upperlimit == 1)
    {
        SoSeparator * ptSep = new SoSeparator;
        SoTransform * aTrans = new SoTransform;
        aTrans->translation.setValue(xyzCoords_[idx][0], 
                                     xyzCoords_[idx][1], 
                                     xyzCoords_[idx][2]);
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

    maxV[0]=minV[0]=xyzCoords_[idx+0][0];
    maxV[1]=minV[1]=xyzCoords_[idx+0][0];
    maxV[2]=minV[2]=xyzCoords_[idx+0][0];

    time[0] = 0.0;
    for(long int i=1; i<upperlimit; i++)
    {
        if(maxV[0]<xyzCoords_[idx+i][0]) maxV[0]=xyzCoords_[idx+i][0] ;
        if(minV[0]>xyzCoords_[idx+i][0]) minV[0]=xyzCoords_[idx+i][0] ;
        if(maxV[1]<xyzCoords_[idx+i][1]) maxV[1]=xyzCoords_[idx+i][1] ;
        if(minV[1]>xyzCoords_[idx+i][1]) minV[1]=xyzCoords_[idx+i][1] ;
        if(maxV[2]<xyzCoords_[idx+i][2]) maxV[2]=xyzCoords_[idx+i][2] ;
        if(minV[2]>xyzCoords_[idx+i][2]) minV[2]=xyzCoords_[idx+i][2] ;
        time[i] = i*dt;
    }

    float dis = distance;// fabs(std::max(std::max((maxV[0]-minV[0]), (maxV[1]-minV[1])), (maxV[2]-minV[2])));
    float (*myVertices)[3]= new float[arrSize+1][3];
    float *myColorBase = new float [arrSize+1];

    myVertices[0][0] = myVertices[arrSize][0] = xyzCoords_[idx][0];
    myVertices[0][1] = myVertices[arrSize][1] = xyzCoords_[idx][1];
    myVertices[0][2] = myVertices[arrSize][2] = xyzCoords_[idx][2];
    if(coloringMethod>=0)myColorBase[0]  = data_[idx][coloringMethod];
    if(coloringMethod==CL_POINT_NUMBER)myColorBase[0]  = 0;
    for(long int i=1; i<upperlimit; i++)
    {
        double tTemp = time[i];
        long int m = 0;
        while(tTemp > time_[idx+m] && m < upperlimit) ++m;

        if( fabs(tTemp-time_[idx+m]) <= 1.0e-9 ||
            fabs(time_[idx+m]-time_[idx+m-1])<=1.0e-8)
        {
            myVertices[i][0] = xyzCoords_[idx+m][0];
            myVertices[i][1] = xyzCoords_[idx+m][1];
            myVertices[i][2] = xyzCoords_[idx+m][2];
        }
        else
        {
            myVertices[i][0] = (xyzCoords_[idx+m][0] +
                                xyzCoords_[idx+m-1][0])*0.5;
            myVertices[i][1] = (xyzCoords_[idx+m][1] +
                                xyzCoords_[idx+m-1][1])*0.5;
            myVertices[i][2] = (xyzCoords_[idx+m][2] +
                                xyzCoords_[idx+m-1][2])*0.5;
        }

        if(coloringMethod>=0)myColorBase[i]  = data_[idx+m][coloringMethod];
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
    if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
        satGroup->addChild(setLineAttributesByBranch(iBranch, stability, lineWidthScaler));
    else if(coloringMethod == CL_STABILITY)
        satGroup->addChild(setLineAttributesByStability(stability, lineWidthScaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        satGroup->addChild(setLineAttributesByType(stability, type, lineWidthScaler));
    else if(coloringMethod == CL_LABELS)
        satGroup->addChild(setLineAttributesByParameterValue(
                j-1, totalLabels_, totalLabels_/2.0, 0,
                stability, lineWidthScaler));
    else if(coloringMethod == CL_COMPONENT)
        satGroup->addChild(setLineAttributesByParameterValue(
                curComponent, maxComponent, maxComponent/2.0, 0,
                stability, scaler));
    else if(coloringMethod >= nar_)
        satGroup->addChild(setLineAttributesByParameterValue(
                orbits_[j].par[parID_[coloringMethod-nar_]],
                parMax_[iBranch][coloringMethod-nar_],
                parMid_[iBranch][coloringMethod-nar_],
                parMin_[iBranch][coloringMethod-nar_],
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
        for(long int row=0; row<totalNumPoints_; ++row)
        {
            time_[row] = data_[row][0];
            if(varIndices[k]>=0)
            {
                float dummy = data_[row][varIndices[k]];
                xyzCoords_[row][k] = dummy;
            }
            else if(varIndices[k]<0)
            {
                xyzCoords_[row][k]=0.0;
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

////////////////////////////////////////////////////////////////////////
//
//
//
void
Solution::searchForMaxMin(int component, int  varIndices[])
//
////////////////////////////////////////////////////////////////////////
{
    double mx, mi;
    for(int k=0; k<3; k++)
    {
        for(long int row=0; row<totalNumPoints_; ++row)
        {
            if(varIndices[k]>=0)
            {
                float dummy = data_[row][varIndices[k]];
                if(dummy>max_[k] || (row==0 && component==1))
                    max_[k] = dummy;
                if(dummy<min_[k] || (row==0 && component==1))
                    min_[k] = dummy;
            }
            else if(varIndices[k]<0)
            {
                max_[k]= 1;
                min_[k]=-1;
            }
        }
        mx = max_[k];
        mi = min_[k];
        rounding(mx, mi);
        max_[k] = mx;
        min_[k] = mi;
    }
}

///////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
bool Solution::parse( const char* sFileName )
//
///////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
{
    FILE * inFile;
    long int position, i;
    int ibr,ntot,itp,lab,nfpr,isw,ntpl,nar,nrowpr,ntst,ncol,npar1;
    int maxColSize=0;
    long int branchCounter = 0;
    long int lastBranchID  = -999;
    totalLabels_ = 0;
    totalNumPoints_ = 0;

// Open input file
    if((inFile=fopen(sFileName,"r"))==NULL)
    {
        printf(" Cannot open input file : %s ! \n", sFileName);
        nar_ = maxColSize;
        return false;
    }

    long int totalNumPoints= 0;   
    position = ftell(inFile);

    long int total = 0;
    while(fscanf(inFile,"%d %d %d %d %d %d %d %d %d %d %d %d",
		 &ibr,&ntot,&itp,&lab,&nfpr,&isw,&ntpl,&nar,
		 &nrowpr,&ntst,&ncol,&npar1) == 12)
    {
        if(maxColSize < nar) maxColSize = nar;
        if(lastBranchID == -999) lastBranchID = ibr;
        if(lastBranchID != ibr && lastBranchID != -999) {
            branchCounter++;
            lastBranchID = ibr;
	}

        totalNumPoints += (ntpl != 1) ? ntpl : 2;
        positions_.push(position);

        for(i=0;i<nrowpr+1;i++)
            while(fgetc(inFile)!='\n');

        if(ntpl != 0)
        {
            total=total+1;
        }

        position = ftell(inFile);
    }

    fclose(inFile);

    totalLabels_ = total;
    numOrbits_ = total;
    totalNumPoints_ = totalNumPoints;
    numBranches_ = ++branchCounter;
    nar_ = maxColSize;
    return true;
}


///////////////////////////////////////////////////////////////////////
//
// READ solution (orbits) to the array
//
bool Solution::read(const char* sFileName, int varIndices[])
//
///////////////////////////////////////////////////////////////////////

{
    FILE * inFile;
    int ibr,ntot,itp,lab,nfpr,isw,ntpl,nar,nrowpr,ntst,ncol,npar1;
    long int i, j;
    float dummy;
    char line[MAX_LINE_LENGTH];

    if((inFile = fopen(sFileName,"r")) == NULL)
    {
        printf(" Cannot open input file: %s\n", sFileName);
        return false;
    }

    totalNumPoints_ = 0;
    for(i=0; i<3; i++)
    {
        max_[i]=0.0;
        min_[i]=0.0;
    }
    long int counter = 0;
    long int orbitCounter = 0;
    long int row = 0;
    long int branchCounter = 0;
    long int lastBranchID  = -999;
    long int totalNumPointsInEachBranch= 0;
    int lbStability = 0;

    orbits_[0].label=0;
    orbits_[0].ntst=0;
    orbits_[0].ncol=0;

    while(!positions_.empty())
    {
        fseek(inFile,positions_.front(),SEEK_SET);
        positions_.pop();

        fscanf(inFile,"%d %d %d %d %d %d %d %d %d %d %d %d",\
            &ibr,&ntot,&itp,&lab,&nfpr,&isw,&ntpl,&nar,\
            &nrowpr,&ntst,&ncol,&npar1);
        if(lastBranchID == -999) lastBranchID = ibr;
        if(lastBranchID != ibr && lastBranchID != -999)
        {
            numVerticesEachBranch_[branchCounter]=totalNumPointsInEachBranch;
            numOrbitsInEachBranch_[branchCounter]=orbitCounter;
            branchID_[branchCounter++]=lastBranchID;
            totalNumPointsInEachBranch = 0;
            lastBranchID = ibr;
            orbitCounter = 0;
        }
        clientData.labelIndex[counter][0] = row;
        clientData.labelIndex[counter][2] = itp;

        if(ibr > 0)
        {
             lbStability = ((ntot>0) ? 1 : 2);
        }
        else
        {
             lbStability = ((ntot>0) ? 3 : 4);
        }

        clientData.labelIndex[counter][3] = lbStability;

        if( ntpl != 0) 
        {
            orbits_[counter].numVerticesEachPeriod=ntpl;
            orbits_[counter].label = lab;
            orbits_[counter].ntst = ntst;
            orbits_[counter].ncol = ncol;
            orbitCounter++;
        }

        while(fgetc(inFile)!='\n');
        {
            {
                for(i=0; i<ntpl; ++i)
                {
                    ++(totalNumPoints_);
                    ++totalNumPointsInEachBranch;
                    for(j=0; j<nar; ++j)
                    {
// read all the data set to the dynamic array.
                        char dummystr[25];
                        fscanf(inFile,"%24s",dummystr);
                        dummy=fortranatof(dummystr);
                        data_[row][j]=dummy;
                        if(row == 0) clientData.solMax[j] = dummy;
                        else
                        if(clientData.solMax[j] < dummy) clientData.solMax[j] = dummy;
                            if(row == 0) clientData.solMin[j] = dummy;
                        else
                        if(clientData.solMin[j] > dummy) clientData.solMin[j] = dummy;
                    }
                    ++row;
                }

                int nLines = (npar1+6)/7;
		int nrowprsmall = ((nar-1)/7+1)*ntpl + nLines;
		if(ntst != 0 && nrowpr > nrowprsmall)
                {
                    int ndim=nar-1;
                    int nrd=(ndim+6)/7;
                    int nLines = nrd*ntpl + (nfpr+6)/7 + (nfpr+19)/20;
                    for(i=0; i<nLines; ++i) fgets(line, sizeof(line), inFile);
                }

                orbits_[counter].par = new double[npar1];
                for(int nzoo = 0; nzoo<nLines; ++nzoo)
                {
                    fgets(line, sizeof(line), inFile);
                    int xCol = (npar1 - nzoo*7) > 7 ? 7 : npar1 - nzoo*7;
                    for(i=0; i<xCol; ++i)
                    {
                        char dummystr[25];
                        fscanf(inFile,"%*[ \t\n]");
                        fscanf(inFile,"%24[^ \t\n]",dummystr);
                        dummy=fortranatof(dummystr);
                        if(dummy != 0)
                        {
                            orbits_[counter].par[nzoo*7+i] = dummy;
                            if( nzoo*7+i==1 ) orbits_[counter].mass = dummy;
                            else if( nzoo*7+i==10 ) orbits_[counter].period=dummy;
                        }
                    }
                }
            }
        }
        if( ntpl != 0) 
            counter++;
    }
    numVerticesEachBranch_[branchCounter]=totalNumPointsInEachBranch;
    numOrbitsInEachBranch_[branchCounter]=orbitCounter;
    branchID_[branchCounter]=ibr;

    totalLabels_ = counter;
    numOrbits_    = counter;


    double parMax, parMin, parMid;

    for(int jv = 0; jv<npar_; ++jv)
    {
        long int startBranch = 0;
        long int endBranch = 0;
        for(int iBranch=0; iBranch<numBranches_; iBranch++)
        {
            int parid = parID_[jv];	    
            parMax = parMin = parMid = orbits_[startBranch].par[parid];
            endBranch = startBranch+numOrbitsInEachBranch_[iBranch];
            for(int innerLoop = startBranch; innerLoop<endBranch; ++innerLoop)
            {
                float par = orbits_[innerLoop].par[parID_[jv]];
                if(parMax < par)
                    parMax = par;
                if(parMin > par)
                    parMin = par;
            }
            parMax_[iBranch][jv]=parMax;
            parMin_[iBranch][jv]=parMin;
            parMid_[iBranch][jv]=(parMin+parMax)/2.0;
            startBranch = endBranch;
        }
    }


    if(whichCoordSystem != ROTATING_F)
    {
        float r[3] = {0.0,0.0,0.0};
        int center = 0;
        if(whichCoordSystem == INERTIAL_B) center = 0;
        if(whichCoordSystem == INERTIAL_S) center = 1;
        if(whichCoordSystem == INERTIAL_E) center = 2;
        for(int i=0; i<totalNumPoints_; i++)
        {
            xyzCoords_[i][0]=r[0];
            xyzCoords_[i][1]=r[1];
            xyzCoords_[i][2]=r[2];
        }
    }
    fclose(inFile);
    return true;
}

void 
Solution::normalizeData()
{
    // To convert the original data to [-1,1];
    int np = numOrbits_;

#ifdef DEBUG
    cout <<" Max sol 0 :" <<max_[0]<<" Min "<<min_[0]<<endl;
    cout <<" Max sol 1 :" <<max_[1]<<" Min "<<min_[1]<<endl;
    cout <<" Max sol 2 :" <<max_[2]<<" Min "<<min_[2]<<endl;
#endif
    double div[3], con[3];
    for(int k=0; k<3; k++)
    {
        con[k] = 0.0;
        div[k] = (max_[k]-min_[k])/2.0;

        if(div[k]/max_[k]>1.0e-10) 
        {
            div[k] = 1.0/div[k];
            con[k] = div[k]*min_[k];
        }
    }

    long int sump = 0;
    for(int i=0; i<np; i++)
    {
        long int nt = orbits_[i].numVerticesEachPeriod;
        for(int j=0; j<nt; j++)
        {
            for(int k=0; k<3; k++)
            {
                if(div[k]/max_[k]>1.0e-10)
                xyzCoords_[sump+j][k]=
                    xyzCoords_[sump+j][k]*div[k]-con[k]-1.0;
            }
        /**
            if( !((max_[k]<=1.0  && max_[k]>0.5 &&
                 min_[k]>=-1.0 && min_[k]<-0.5 )||
                (div[k]<0.00000001)))
            {
                xyzCoords_[sump+j][k]=
                       (xyzCoords_[sump+j][k]-avg[k])/div[k];
            }
        **/
        }
        sump += nt;
    }
}

void
Solution::alloc()
{
    time_ = new double[totalNumPoints_];
    xyzCoords_ = new float[totalNumPoints_][3];
    numVerticesEachBranch_ = new int32_t[numBranches_];
    numOrbitsInEachBranch_ = new int32_t[numBranches_];
    branchID_ = new long[numBranches_];
    parMax_ = new double*[numBranches_];
    parMin_ = new double*[numBranches_];
    parMid_ = new double*[numBranches_];
    if (numBranches_ > 0)
    {
        parMax_[0] = new double[numBranches_*npar_];
        parMin_[0] = new double[numBranches_*npar_];
        parMid_[0] = new double[numBranches_*npar_];
	for(int i=1; i<numBranches_; ++i)
	{
            parMax_[i] = &parMax_[0][i*npar_];
            parMin_[i] = &parMin_[0][i*npar_];
            parMid_[i] = &parMid_[0][i*npar_];
        }
    }
    numAxis_   = 3;

    data_ = new float*[totalNumPoints_];
    if (totalNumPoints_ > 0) {
        data_[0] = new float[totalNumPoints_*nar_];
	for(int ml=1; ml<totalNumPoints_; ++ml)
	    data_[ml] = &data_[0][ml*nar_];
    }
    orbits_ = new struct orbit[numOrbits_];
}

void
Solution::denormalizePosition(float position[])
{
    for(int k=0; k<3; k++)
    {
        float con = 0.0;
        float div = (max_[k]-min_[k])/2.0;
        if(div/max_[k]>1.0e-10) 
        {
            div = 1.0/div;
            con = div*min_[k];
        }
        if(div/max_[k]>1.0e-10)
            position[k] = (position[k]+con+1.0)/div;
    }
}

void
Solution::set_parID(std::queue<int>& parIDs)
{
    if (npar_ != 0) delete [] parID_;
    npar_ = parIDs.size();
    parID_ = new int[npar_];
    for(int is=0; is < npar_; ++is)
    {
        parID_[is] = parIDs.front();
        parIDs.pop();
    }
}


void
Solution::dealloc()
{
    delete [] time_;
    delete [] xyzCoords_;
    delete [] numVerticesEachBranch_;
    delete [] numOrbitsInEachBranch_;
    delete [] branchID_;
    if (numBranches_ > 0)
    {
        delete [] parMax_[0];
        delete [] parMin_[0];
        delete [] parMid_[0];
    }
    delete [] parMax_;
    delete [] parMin_;
    delete [] parMid_;
    if (totalNumPoints_ > 0)
        delete [] data_[0];
    delete [] data_;
    totalNumPoints_  = 0;
    for (int i = 0; i < numOrbits_; i++)
        delete [] orbits_[i].par;
    delete [] orbits_;
}


///////////////////////////////////////////////////////////////////////////
//
//   Using a red ball to simulate the movement of a sattelite and using
//   white lines to simulate the trace of the sattelite.
//
SoSeparator *
Solution::animateOrbitInertialSysUsingLine(int iBranch,  int iOrbit,
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

    float dis = fabs(std::max(std::max((maxV[0]-minV[0]), (maxV[1]-minV[1])),
                              (maxV[2]-minV[2])));

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

        if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
            satGroup->addChild(setLineAttributesByBranch(iBranch,stability,scaler));
        else if(coloringMethod == CL_STABILITY)
            satGroup->addChild(setLineAttributesByStability(stability, scaler));
        else if(coloringMethod == CL_ORBIT_TYPE)
            satGroup->addChild(setLineAttributesByType(stability, type, scaler));
        else if(coloringMethod == CL_LABELS)
        {
            double bMin = 0;
            for(int ib = 0; ib< iBranch; ++ib)
                bMin +=  numOrbitsInEachBranch_[ib];
            double bMax = bMin+numOrbitsInEachBranch_[iBranch]-1;
            satGroup->addChild(setLineAttributesByParameterValue(
                iOrbit, bMax, (bMax+bMin)/2.0, bMin,
                stability, scaler));
        }
        else if(coloringMethod >= nar_)
            satGroup->addChild(setLineAttributesByParameterValue(
                    orbits_[iOrbit].par[parID_[coloringMethod-nar_]],
                    parMax_[iBranch][coloringMethod-nar_],
                    parMid_[iBranch][coloringMethod-nar_],
                    parMin_[iBranch][coloringMethod-nar_],
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
Solution::drawAnOrbitInertialSysUsingLines(int iBranch,  int iOrbit,
float (*myVertices)[3], float *myColorBase,
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

    if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
    else if(coloringMethod == CL_LABELS)
    {
        double bMin = 0;
        for(int ib = 0; ib< iBranch; ++ib)
            bMin +=  numOrbitsInEachBranch_[ib];
        double bMax = bMin+numOrbitsInEachBranch_[iBranch]-1;
        anOrbit->addChild(setLineAttributesByParameterValue(
            iOrbit, bMax, (bMax+bMin)/2.0, bMin,
            stability, scaler));
    }
    else if(coloringMethod >= nar_)
        anOrbit->addChild(setLineAttributesByParameterValue(
                orbits_[iOrbit].par[parID_[coloringMethod-nar_]],
                parMax_[iBranch][coloringMethod-nar_],
                parMid_[iBranch][coloringMethod-nar_],
                parMin_[iBranch][coloringMethod-nar_],
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
Solution::convertDataToInertialSystem(float (*myVertices)[3], 
float *timeEqualDiv, float *myColorBase,
long int arrSize, long int orbitSize, long int kth, long int sumX)
//
////////////////////////////////////////////////////////////////////////
{
    float (*workArray)[3]  = new float [arrSize][3];
    float *time         = new float [arrSize];
    float satPeriod = orbits_[kth].period;
    float rpp[3], vpp[3];
    float massCurLabeledOrbit = orbits_[kth].mass;

    for(int i=0; i<arrSize; ++i)
    {
        workArray[i][0]=xyzCoords_[sumX+i%orbitSize][0];
        workArray[i][1]=xyzCoords_[sumX+i%orbitSize][1];
        workArray[i][2]=xyzCoords_[sumX+i%orbitSize][2];
        if(whichStyle==TUBE && !options[OPT_SAT_ANI] )
        {
            if(coloringMethod>=0 && coloringMethod < nar_)
                for(int k=0; k<11; ++k)
                    myColorBase[i*11+k]  = data_[sumX+i%orbitSize][coloringMethod];
            else if(coloringMethod==CL_POINT_NUMBER )
                for(int k=0; k<11; ++k)
                    myColorBase[i*11+k]  = i;
        }
        else
        {
            if(coloringMethod>=0 && coloringMethod < nar_)
                myColorBase[i]=data_[sumX+i%orbitSize][coloringMethod];
            else if(coloringMethod==CL_POINT_NUMBER )
                myColorBase[i]=i;
        }

        time[i] = time_[sumX+i%orbitSize]+i/orbitSize;

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
        if( fabs(tTemp-time[m]) <= 1.0e-9 || fabs(time[m]-time[m-1])<=1.0e-8)
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
Solution::drawAnOrbitInertialSysUsingTubes(int iBranch,  int iOrbit,
float (*myVertices)[3], float *myColorBase, const long int arrSize,
const float tubeRadiusScaler, const int stability, const int type)
//
//////////////////////////////////////////////////////////////////////////
{
    SoSeparator * anOrbit = new SoSeparator;

    Tube tube;

    if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, tubeRadiusScaler));
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, tubeRadiusScaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        anOrbit->addChild(setLineAttributesByType(stability, type, tubeRadiusScaler));
    else if(coloringMethod == CL_LABELS)
    {
        double bMin = 0;
        for(int ib = 0; ib< iBranch; ++ib)
            bMin +=  numOrbitsInEachBranch_[ib];
        double bMax = bMin+numOrbitsInEachBranch_[iBranch]-1;
        anOrbit->addChild(setLineAttributesByParameterValue(
            iOrbit, bMax, (bMax+bMin)/2.0, bMin,
            stability,  tubeRadiusScaler));
    }
    else if(coloringMethod >= nar_)
        anOrbit->addChild(setLineAttributesByParameterValue(
                orbits_[iOrbit].par[parID_[coloringMethod-nar_]],
                parMax_[iBranch][coloringMethod-nar_],
                parMid_[iBranch][coloringMethod-nar_],
                parMin_[iBranch][coloringMethod-nar_],
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
Solution::drawAnOrbitInertialSysUsingNurbsCurve(int iBranch, int iOrbit,
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

    if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
        anOrbit->addChild(setLineAttributesByBranch(iBranch, stability, scaler));
    else if(coloringMethod == CL_STABILITY)
        anOrbit->addChild(setLineAttributesByStability(stability, scaler));
    else if(coloringMethod == CL_ORBIT_TYPE)
        anOrbit->addChild(setLineAttributesByType(stability, type, scaler));
    else if(coloringMethod == CL_LABELS)
    {
        double bMin = 0;
        for(int ib = 0; ib< iBranch; ++ib)
            bMin +=  numOrbitsInEachBranch_[ib];
        double bMax = bMin+numOrbitsInEachBranch_[iBranch]-1;
        anOrbit->addChild(setLineAttributesByParameterValue(
            iOrbit, bMax, (bMax+bMin)/2.0, bMin,
            stability, scaler));
    }
    else if(coloringMethod >= nar_)
        anOrbit->addChild(setLineAttributesByParameterValue(
                orbits_[iOrbit].par[parID_[coloringMethod-nar_]],
                parMax_[iBranch][coloringMethod-nar_],
                parMid_[iBranch][coloringMethod-nar_],
                parMin_[iBranch][coloringMethod-nar_],
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
Solution::createInertialFrameScene(float dis)
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
        int curBranchID = branchID_[iBranch];
        int sumOrbit    = numOrbitsInEachBranch_[iBranch];
        for(int k=0; k<numOrbits_; ++k)
        {
            if(k >= sumOrbit)
            {
                curBranchID = branchID_[++iBranch];
                sumOrbit   += numOrbitsInEachBranch_[iBranch];
            }
            orbitSize = orbits_[k].numVerticesEachPeriod;
            arrSize = (numPeriodAnimated==0) ? orbitSize : (int)(numPeriodAnimated * orbitSize);

            myVertices = new float [arrSize][3];
            myColorBase= new float [arrSize*11];
            time  = new float [arrSize];

            convertDataToInertialSystem(myVertices, time, myColorBase, arrSize, orbitSize, k, si);
            stability = clientData.labelIndex[k][3];
            type = clientData.labelIndex[k][2];

            if(whichStyle==TUBE)
            {
                solGroup->addChild(drawAnOrbitInertialSysUsingTubes(
                    iBranch,  k, myVertices,myColorBase, arrSize, lineWidthScaler,
                    stability, type));
            }
            else if(whichStyle==SURFACE)
            {
                solGroup->addChild(drawAnOrbitInertialSysUsingLines(
                    iBranch,  k, myVertices, myColorBase, arrSize, lineWidthScaler,
                    stability, type));
            }
            else if(whichStyle==NURBS)
            {
                solGroup->addChild(drawAnOrbitInertialSysUsingNurbsCurve(
                    iBranch, k, myVertices, arrSize, lineWidthScaler,
                    stability, type));
            }
            else
            {
                solGroup->addChild(drawAnOrbitInertialSysUsingLines(
                    iBranch,  k, myVertices, myColorBase, arrSize, lineWidthScaler,
                    stability, type));
            }

            si += orbits_[k].numVerticesEachPeriod;
            delete [] myVertices;
            delete [] time;
        }
    }
    else if(animationLabel != MY_NONE)
    {
        float vpp[3];
        for(std::vector<int>::size_type n=0; n<lblIndices.size(); ++n)
        {
            animationLabel=myLabels[lblIndices[n]];
            int si = 0, kno = 0;
            int iBranch = 0;
            int curBranchID = branchID_[iBranch];
            int sumOrbit    = numOrbitsInEachBranch_[iBranch];
            while(kno<numOrbits_ && myLabels[kno]<animationLabel)
            {
                si += orbits_[kno].numVerticesEachPeriod;
                if(kno >= sumOrbit)
                {
                    curBranchID = branchID_[++iBranch];
                    sumOrbit   += numOrbitsInEachBranch_[iBranch];
                }
            }

            satPeriod = orbits_[kno].period;
            long int orbitSize = orbits_[kno].numVerticesEachPeriod;
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
                    solGroup->addChild(drawAnOrbitInertialSysUsingTubes(
                        iBranch,  kno, myVertices, myColorBase, arrSize, lineWidthScaler,
                        stability, type));
                }
                else if(whichStyle==SURFACE)
                {
                    solGroup->addChild(drawAnOrbitInertialSysUsingLines(
                        iBranch,  kno, myVertices,myColorBase,  arrSize, lineWidthScaler,
                        stability, type));
                }
                else if(whichStyle==NURBS)
                {
                    solGroup->addChild(drawAnOrbitInertialSysUsingNurbsCurve(
                        iBranch,  kno, myVertices, arrSize, lineWidthScaler,
                        stability, type));
                }
                else
                {
                    solGroup->addChild(drawAnOrbitInertialSysUsingLines(
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
            asMax[0]=max_[0]; asMax[1]=max_[1];asMax[2]=max_[2];
            asMin[0]=min_[0]; asMin[1]=min_[1];asMin[2]=min_[2];
        }
        else
        {
            asMax[0] = asMax[1] = asMax[2] = 1;
            asMin[0] = asMin[1] = asMin[2] = -1;
        }

        coordSep->addChild(createCoordinates(setShow3D, whichCoord, asMax, asMin, tickers, &envColors[1]));

        aSep->addChild(coordSep);
    }

    solGroup->addChild(aSep);
    if(options[OPT_PERIOD_ANI])
    {
        int iBranch = 0;
        int curBranchID = branchID_[iBranch];
        int sumOrbit    = numOrbitsInEachBranch_[iBranch];
        long int si = 0;
        SoBlinker * solBlinker = new SoBlinker;
        solBlinker->speed = orbitSpeed;
        solBlinker->on = TRUE;
        for(int k=0; k<numOrbits_; ++k)
        {
            if(k >= sumOrbit)
            {
                curBranchID = branchID_[++iBranch];
                sumOrbit   += numOrbitsInEachBranch_[iBranch];
            }
            long int orbitSize = orbits_[k].numVerticesEachPeriod;
            long int arrSize = (numPeriodAnimated==0) ?
                orbitSize : (int)(numPeriodAnimated * orbitSize);

            float (*myVertices)[3] = new float [arrSize][3];
            float *myColorBase = new float [arrSize*11];
            float *time   = new float [arrSize];

            convertDataToInertialSystem(myVertices, time, myColorBase, arrSize, orbitSize, k, si);

            solBlinker->addChild(drawAnOrbitInertialSysUsingLines(
                iBranch,  k, myVertices, myColorBase, arrSize, aniLineScaler*lineWidthScaler,
                clientData.labelIndex[k][3],clientData.labelIndex[k][2]));
            si += orbits_[k].numVerticesEachPeriod;
            delete [] myVertices;
            delete [] myColorBase;
            delete [] time;
        }
        solGroup->addChild(solBlinker);
    }
    return solGroup;

}

#if 0 // unused R3B related functions
///////////////////////////////////////////////////////////////////////////
//
//
SoSeparator *
Solution::animateIner2(long int lblJ,long int si)
//
///////////////////////////////////////////////////////////////////////////
{
    float ptb[3];
    int center = 0;
    float mu = 0.01215;
    SoSeparator *satGroup = new SoSeparator;
    SoMaterial *satMtl = new SoMaterial;
    satMtl->diffuseColor.setValue(envColors[7]);
    satMtl->transparency = 0.0;

    SoDrawStyle *satStyle = new SoDrawStyle;
    satStyle->style = SoDrawStyle::FILLED;
    satGroup->addChild(satStyle);

    SoBlinker *satBlker = new SoBlinker;

    int upperlimit = orbits_[lblJ].numVerticesEachPeriod;
    int idx = si;
    satBlker->speed = 0.5;

// go thr the whole dataset to find the max and min in this set
// so that we can decide the size of the ball.
    float maxV[3], minV[3];
    maxV[0]=minV[0]=xyzCoords_[idx+0][0];
    maxV[1]=minV[1]=xyzCoords_[idx+0][0];
    maxV[2]=minV[2]=xyzCoords_[idx+0][0];
    for(int i=1; i<upperlimit; i++)
    {
        if(maxV[0]<xyzCoords_[idx+i][0]) maxV[0]=xyzCoords_[idx+i][0] ;
        if(minV[0]>xyzCoords_[idx+i][0]) minV[0]=xyzCoords_[idx+i][0] ;
        if(maxV[1]<xyzCoords_[idx+i][1]) maxV[1]=xyzCoords_[idx+i][1] ;
        if(minV[1]>xyzCoords_[idx+i][1]) minV[1]=xyzCoords_[idx+i][1] ;
        if(maxV[2]<xyzCoords_[idx+i][2]) maxV[2]=xyzCoords_[idx+i][2] ;
        if(minV[2]>xyzCoords_[idx+i][2]) minV[2]=xyzCoords_[idx+i][2] ;
    }
    float dis = fabs(std::max(std::max((maxV[0]-minV[0]), (maxV[1]-minV[1])),
                              (maxV[2]-minV[2])));
    float pta[3], vertices[2][3];
    float rgb1[3], rgb2[3];
    rgb1[0]=0; rgb1[2]=0; rgb1[1]=1;
    rgb2[0]=1; rgb2[1]=0; rgb2[2]=0;

// animate the orbit
    ptb[0]=xyzCoords_[idx+0][0];
    ptb[1]=xyzCoords_[idx+0][1];
    ptb[2]=xyzCoords_[idx+0][2];
    float primPos[3];

    float satPeriod ;
    satPeriod = period_[lblJ];
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
            if(time_[idx+i]>=aniTime/2.0/M_PI) break;
        }

// calculate the position of this record.
        if(i==upperlimit)
        {
            ptb[0]=xyzCoords_[idx+i-1][0];
            ptb[1]=xyzCoords_[idx+i-1][1];
            ptb[2]=xyzCoords_[idx+i-1][2];
        }
        else if(i==0)
        {
            ptb[0]=xyzCoords_[idx][0];
            ptb[1]=xyzCoords_[idx][1];
            ptb[2]=xyzCoords_[idx][2];
        }
        else
        {
            ptb[0]=(xyzCoords_[idx+i-1][0]+xyzCoords_[idx+i][0])/2.0;
            ptb[1]=(xyzCoords_[idx+i-1][1]+xyzCoords_[idx+i][1])/2.0;
            ptb[2]=(xyzCoords_[idx+i-1][2]+xyzCoords_[idx+i][2])/2.0;
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
Solution::animateOrbitMovement(long int j, long int si)
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

    int upperlimit = orbits_[j].numVerticesEachPeriod;
    int idx = si;             
    satBlker->speed = satSpeed;

    float maxV[3], minV[3];
    maxV[0]=minV[0]=xyzCoords_[idx+0][0];
    maxV[1]=minV[1]=xyzCoords_[idx+0][0];
    maxV[2]=minV[2]=xyzCoords_[idx+0][0];
    double *time = new double[upperlimit];
    float dt = 1.0/upperlimit;
    time[0] = 0.0;
    for(int i=1; i<upperlimit; i++)
    {
        if(maxV[0]<xyzCoords_[idx+i][0]) maxV[0]=xyzCoords_[idx+i][0] ;
        if(minV[0]>xyzCoords_[idx+i][0]) minV[0]=xyzCoords_[idx+i][0] ;
        if(maxV[1]<xyzCoords_[idx+i][1]) maxV[1]=xyzCoords_[idx+i][1] ;
        if(minV[1]>xyzCoords_[idx+i][1]) minV[1]=xyzCoords_[idx+i][1] ;
        if(maxV[2]<xyzCoords_[idx+i][2]) maxV[2]=xyzCoords_[idx+i][2] ;
        if(minV[2]>xyzCoords_[idx+i][2]) minV[2]=xyzCoords_[idx+i][2] ;
        time[i] = i*dt;
    }
    float dis = std::max(std::max((maxV[0]-minV[0]), (maxV[1]-minV[1])),
                                  (maxV[2]-minV[2]));

// animate the orbit
    ptb[0]=xyzCoords_[idx][0];
    ptb[1]=xyzCoords_[idx][1];
    ptb[2]=xyzCoords_[idx][2];
    satBlker->addChild(drawASphere(ptb,1.*dis/100.0));

    for(int i=1; i<upperlimit; i++)
    {
        int m = 1;
        while(time[i] > time_[idx+m] && m < upperlimit) ++m;

        if( fabs(time[i]-time_[m]) <= 1.0e-9 )
        {
            ptb[0]=xyzCoords_[idx+m][0];
            ptb[1]=xyzCoords_[idx+m][1];
            ptb[2]=xyzCoords_[idx+m][2];
            satBlker->addChild(drawASphere(ptb,1.*dis/100.0));
        }
        else
        {
            ptb[0]= (xyzCoords_[idx+m][0]+xyzCoords_[idx+m-1][0])/2.0;
            ptb[1]= (xyzCoords_[idx+m][1]+xyzCoords_[idx+m-1][1])/2.0;
            ptb[2]= (xyzCoords_[idx+m][2]+xyzCoords_[idx+m-1][2])/2.0;
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
//   This routine animate the calculation steps, namely the density of the
//   collocation points.
//
SoSeparator *
Solution::animateOrbitCalSteps(long int snOrbit, long int si)
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

    int upperlimit = numVerticesEachPeriod_[snOrbit-1];
    int idx = si;
    satBlker->speed = satSpeed;

    float maxV[3], minV[3];
    maxV[0]=minV[0]=xyzCoords_[idx+0][0];
    maxV[1]=minV[1]=xyzCoords_[idx+0][0];
    maxV[2]=minV[2]=xyzCoords_[idx+0][0];

    for(int i=1; i<upperlimit; i++)
    {
        if(maxV[0]<xyzCoords_[idx+i][0]) maxV[0]=xyzCoords_[idx+i][0] ;
        if(minV[0]>xyzCoords_[idx+i][0]) minV[0]=xyzCoords_[idx+i][0] ;
        if(maxV[1]<xyzCoords_[idx+i][1]) maxV[1]=xyzCoords_[idx+i][1] ;
        if(minV[1]>xyzCoords_[idx+i][1]) minV[1]=xyzCoords_[idx+i][1] ;
        if(maxV[2]<xyzCoords_[idx+i][2]) maxV[2]=xyzCoords_[idx+i][2] ;
        if(minV[2]>xyzCoords_[idx+i][2]) minV[2]=xyzCoords_[idx+i][2] ;
    }

    float dis = fabs(std::max(std::max((maxV[0]-minV[0]), (maxV[1]-minV[1])),
                              (maxV[2]-minV[2])));
    for(int i=0; i<upperlimit; i++)
    {
        ptb[0]=xyzCoords_[idx+i][0];
        ptb[1]=xyzCoords_[idx+i][1];
        ptb[2]=xyzCoords_[idx+i][2];
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
Solution::animateOrbitWithNurbsCurveTail(long int j, long int si)
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

    int upperlimit = orbits_[j].numVerticesEachPeriod;
    int idx = si;

    float maxV[3], minV[3];
    double *time = new double[upperlimit];
    double dt = 1.0/upperlimit;
    maxV[0]=minV[0]=xyzCoords_[idx+0][0];
    maxV[1]=minV[1]=xyzCoords_[idx+0][0];
    maxV[2]=minV[2]=xyzCoords_[idx+0][0];

    time[0] = 0.0;
    for(int i=1; i<upperlimit; i++)
    {
        if(maxV[0]<xyzCoords_[idx+i][0]) maxV[0]=xyzCoords_[idx+i][0] ;
        if(minV[0]>xyzCoords_[idx+i][0]) minV[0]=xyzCoords_[idx+i][0] ;
        if(maxV[1]<xyzCoords_[idx+i][1]) maxV[1]=xyzCoords_[idx+i][1] ;
        if(minV[1]>xyzCoords_[idx+i][1]) minV[1]=xyzCoords_[idx+i][1] ;
        if(maxV[2]<xyzCoords_[idx+i][2]) maxV[2]=xyzCoords_[idx+i][2] ;
        if(minV[2]>xyzCoords_[idx+i][2]) minV[2]=xyzCoords_[idx+i][2] ;
        time[i] = i*dt;
    }

    float dis = fabs(std::max(std::max((maxV[0]-minV[0]), (maxV[1]-minV[1])),
                              (maxV[2]-minV[2])));

    float (*myVertices)[3]= new float[upperlimit][3];

    myVertices[0][0]=xyzCoords_[idx][0];
    myVertices[0][1]=xyzCoords_[idx][1];
    myVertices[0][2]=xyzCoords_[idx][2];
    for(int i=1; i<upperlimit; i++)
    {
        int m = 1;
        while(time[i] > time_[idx+m] && m < upperlimit) ++m;

        if( fabs(time[i]-time_[m]) <= 1.0e-9 )
        {
            myVertices[i][0]=xyzCoords_[idx+m][0];
            myVertices[i][1]=xyzCoords_[idx+m][1];
            myVertices[i][2]=xyzCoords_[idx+m][2];
        }
        else
        {
            myVertices[i][0]= (xyzCoords_[idx+m][0]+xyzCoords_[idx+m-1][0])/2.0;
            myVertices[i][1]= (xyzCoords_[idx+m][1]+xyzCoords_[idx+m-1][1])/2.0;
            myVertices[i][2]= (xyzCoords_[idx+m][2]+xyzCoords_[idx+m-1][2])/2.0;
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

///////////////////////////////////////////////////////////////////////////
//
SoSeparator *
Solution::drawEarthMovement(int k)
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
    for(int i=0; i<orbits_[k].numVerticesEachPeriod; ++i)
    {
        vertices[i][0]= cos(time_[i]/2.0/M_PI*360.0);
        vertices[i][1]= sin(time_[i]*360.0/2.0/M_PI);
        vertices[i][2]= 0;
    }
    myint[0]=orbits_[k].numVerticesEachPeriod;
    SoCoordinate3 * eCoords = new SoCoordinate3;
    eCoords->point.setValues(0, orbits_[k].numVerticesEachPeriod, vertices);
    SoLineSet *eLine= new SoLineSet;
    eLine->numVertices.setValues(0, 1, myint);
    eSep->addChild(eCoords);
    eSep->addChild(eLine);
    return eSep;
}

///////////////////////////////////////////////////////////////////////////
//
// calculate the current position of the primary at time t in the inertial frame
//
void
Solution::calPrimPos(float t, float pos[])
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
Solution::calSatPos(int center, float mu, float t, float primPos[], float satPos[])
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
SoSeparator *
Solution::drawASphereWithColor(float color[], float position[], float size)
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
