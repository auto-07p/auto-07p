/* Allows reading in very large files */
#ifndef _LARGEFILE_SOURCE
#define _LARGEFILE_SOURCE
#endif
#ifndef _FILE_OFFSET_BITS
#define _FILE_OFFSET_BITS 64
#endif

#include "bifurcation.h"

#include <stdlib.h>
#include <stdio.h>

#ifdef DEBUG
using namespace std;
#include <iostream>
#endif

#include "gplaut04.h"
#include "createCoords.h"
#include "rounding.h"
#include "tube.h"

extern UserData clientData;
Bifurcation *myBifNode;

//////////////////////////////////////////////////////////////////////////
//
// Routine to create a scene graph representing an auto bifurcation
//
SoSeparator *
Bifurcation::createScene()
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
            asMax[0]=max_[0]; asMax[1]=max_[1];asMax[2]=max_[2];
            asMin[0]=min_[0]; asMin[1]=min_[1];asMin[2]=min_[2];
        }
        else
        {
            asMax[0] = asMax[1] = asMax[2] = 1;
            asMin[0] = asMin[1] = asMin[2] = -1;
        }

        coordSep->addChild(createCoordinates(setShow3D, whichCoord, asMax, asMin, tickers, &envColors[1]));

        result->addChild(coordSep);
    }

    if(useR3B)
        result->addChild(createR3BPoints(min_, max_));

// create bifurcation graph
    SoSeparator * bifBranchSep = render();
    result->addChild(bifBranchSep);

    return result;
}

///////////////////////////////////////////////////////////////////////////
//
SoSeparator *
Bifurcation::drawALabel(int row, float xoffset, long int label)
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
    normalizeData(row, xyz);

    //pick a good direction for the label
    int first = row;
    int second = row + 1;
    if (second >= totalNumPoints_)
    {
        first--;
        second--;
    }
    if (first < 0) first = 0;
    float yoffset = xoffset;
    normalizeData(first, firstxyz);
    normalizeData(second, secondxyz);
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
Bifurcation::drawLabelPtsInScene()
//
//////////////////////////////////////////////////////////////////////////
{
    float dis = !options[OPT_NORMALIZE_DATA] ? fabs(std::max(std::max(
        (max_[0]-min_[0]),
        (max_[1]-min_[1])),
        (max_[2]-min_[2]))) : 2.0;

    SoSeparator * result = new SoSeparator;
    int lbl = myLabels[lblIndices[0]], row = 0;
    float position[3];
    int lbType;

    if(lbl == MY_ALL)
    {
        for(int k = 0; k < totalLabels_; k++)
        {
            SoMaterial *lblMtl;
            row = clientData.labelIndex[k][1];
            lbType = clientData.labelIndex[k][2];
    
            lblMtl = setLabelMaterial(lbType);
            result->addChild(lblMtl);
            normalizeData(row, position);

            float size = dis*0.005*labelRadius;
            size *= lineWidthScaler;
            result->addChild( drawASphere(position, size));
            if(options[OPT_LABEL_NUMBERS])
                result->addChild( drawALabel(row, size, orbits_[k].label));
        }
    }
    else if(lbl != MY_NONE)
    {
        for(std::vector<int>::size_type i=0; i<lblIndices.size(); ++i)
        {
            SoMaterial *lblMtl;

            int k = lblIndices[i];
            row = clientData.labelIndex[k][1];
            lbType = clientData.labelIndex[k][2];

            lblMtl = setLabelMaterial(lbType);
            result->addChild(lblMtl);

            normalizeData(row, position);

	    float size = dis*0.005*labelRadius;
            size *= lineWidthScaler;
            result->addChild( drawASphere(position, size));
            if(options[OPT_LABEL_NUMBERS])
                result->addChild( drawALabel(row, size, orbits_[k].label));
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
Bifurcation::setLabelMaterial(int lblType)
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
Bifurcation::drawABranchUsingTubes(int iBranch, long int l,
long int sumX, float scaler)
//
//
/////////////////////////////////////////////////////////////////
{
    SoSeparator * tSep = new SoSeparator;
    long int upperlimit = numVerticesEachBranch_[l];

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
        normalizeData(idx, path[i]);
        if(coloringMethod>=0)
            for(int k=0; k<11; ++k)
                colorBase[i*11+k]  = data_[idx*nar_ + coloringMethod];
        else if(coloringMethod==CL_POINT_NUMBER)
            for(int k=0; k<11; ++k)
                colorBase[i*11+k]  = i;
        else if(coloringMethod == CL_STABILITY)
            for(int k=0; k<11; ++k)
                colorBase[i*11+k] = ptStability_[idx];
    }

    if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
        tSep->addChild(setLineAttributesByBranch(iBranch, 0, scaler));
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


///////////////////////////////////////////////////////////////////////////
//
//         animate the solution by using lines. This version use less memory
//         and much faster.
//////////////////////////////////////////////////////////////////////////
//
SoSeparator *
Bifurcation::drawABranchUsingLines(int iBranch, long int l, long int si, 
                                   float scaler)
//
//////////////////////////////////////////////////////////////////////////
{
    int32_t  myint[10];

    SoSeparator * aBranch = new SoSeparator;
    long int size         = numVerticesEachBranch_[l];
    float *colorBase      = new float[size];
    float (*vertices)[3]  = new float[size][3];

    long int curSize = 0;

    int lastStab = ptStability_[si];
    int curStab  = lastStab;
    long int m   = 0;
    long int idx = si+m;
    do
    {
        lastStab = curStab;
        normalizeData(idx, vertices[curSize]);
        if(coloringMethod >= 0)
            colorBase[curSize]  = data_[idx * nar_ + coloringMethod];
        else if(coloringMethod == CL_POINT_NUMBER)
            colorBase[curSize]  = m;
        else if(coloringMethod == CL_STABILITY)
            colorBase[curSize]  = ptStability_[idx];
        m++;
        idx = si+m;
        if(m < size)
            curStab = ptStability_[idx];
        curSize++;
        if( (lastStab == curStab || curSize <= 1) && m != size) continue;

        SoCoordinate3 *myCoords = new SoCoordinate3;
        myCoords->point.setValues(0, curSize, vertices);
        myint[0]= curSize;

// define the solution line set
        SoLineSet *myLine= new SoLineSet;
        myLine->numVertices.setValues(0, 1, myint);

        if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
            aBranch->addChild(setLineAttributesByBranch(iBranch, lastStab, scaler));
        else if(coloringMethod == CL_STABILITY)
            aBranch->addChild(setLineColorBlendingByStability(colorBase, curSize, lastStab, scaler));
        else /* CL_POINT_NUMBER or >= 0 */
            aBranch->addChild(setLineColorBlending(colorBase, curSize,
                lastStab, scaler));

        aBranch->addChild(myCoords);
        aBranch->addChild(myLine);

        curSize = 0;
        normalizeData(idx-1, vertices[curSize]);
        if(coloringMethod >= 0)
            colorBase[curSize]  = data_[(idx-1) * nar_ + coloringMethod];
        else if(coloringMethod == CL_POINT_NUMBER)
            colorBase[curSize]  = m-1;
        else if(coloringMethod == CL_STABILITY)
            colorBase[curSize]  = ptStability_[idx-1];
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
Bifurcation::drawABranchUsingNurbsCurve(int iBranch, long int l,
                                        long int si, float scaler)
//
//////////////////////////////////////////////////////////////////////////
{
    int32_t  myint[10];

    SoSeparator * aBranch = new SoSeparator;
    long int size = numVerticesEachBranch_[l];
    float (*vertices)[3] = new float[size][3];
    float *colorBase = new float[size];
    for(long int m=0; m<size; ++m) 
    {
        long idx = si+m;
        normalizeData(idx, vertices[m]);
        if(coloringMethod>=0)
            colorBase[m]  =data_[idx * nar_ + coloringMethod];
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

    if(coloringMethod == CL_BRANCH_NUMBER || coloringMethod == CL_CURVE_NUMBER)
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
    long int size = orbits_[l].numVerticesEachLabelInterval;
    float *colorBase = new float[size];
    float (*vertices)[3] = new float[size][3];
    for(int m=0; m<size; m++)
    {
        long int idx = si+m;
        normalizeData(idx, vertices[m]);
        if(coloringMethod>=0)
            colorBase[m]  =data_[idx * nar_ + coloringMethod];
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
    vertices = new float[orbits_[l].numVerticesEachLabelInterval][3];
    for(int m=0; m<orbits_[l].numVerticesEachLabelInterval; m++)
    {
        vertices[m][0]=xyzCoords_[si+m][0];
        vertices[m][1]=xyzCoords_[si+m][1];
        vertices[m][2]=xyzCoords_[si+m][2];
    }
    SoCoordinate3 *myCoords = new SoCoordinate3;
    myCoords->point.setValues(0, orbits_[l].numVerticesEachLabelInterval, vertices);
    myint[0]=orbits_[l].numVerticesEachLabelInterval;

    SoGroup *cvGrp = new SoGroup;

    int number = orbits_[l].numVerticesEachLabelInterval;
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


/////////////////////////////////////////////////////////////////
//
//                  create bifurcation scene
//
SoSeparator *
Bifurcation::render()
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
        for(int ka=0; ka<numBranches_; ka++)
        {
            bifGroup->addChild(drawABranchUsingTubes(ka, k, si, 1*lineWidthScaler));
            k = k+1;
            si += numVerticesEachBranch_[ka];
        }
    }
    else if (whichStyle == NURBS)
    {
        long int si = 0, k = 0;
        for(int ka=0; ka<numBranches_; ka++)
        {
            bifGroup->addChild(drawABranchUsingNurbsCurve(ka, k, si, 1*lineWidthScaler));
            k = k+1;
            si += numVerticesEachBranch_[ka];
        }

    }
    else 
    {
        long int si = 0, k = 0;
        for(int ka=0; ka<numBranches_; ka++)
        {
            bifGroup->addChild(drawABranchUsingLines(ka, k, si, 1*lineWidthScaler));
            k = k+1;
            si += numVerticesEachBranch_[ka];
        }
    }

    if(options[OPT_DRAW_LABELS])
        bifSep->addChild(drawLabelPtsInScene());

    bifSep->addChild(bifMtl);
    bifSep->addChild(bifGroup);

    return bifSep;
}

////////////////////////////////////////////////////////////////////////
//
//
//
void
Bifurcation::copyDataToWorkArray(int  varIndices[])
//
////////////////////////////////////////////////////////////////////////
{

    double mx, mi;

    for(int k=0; k<3; k++)
    {
        varIndices_[k] = varIndices[k];
        for(long int row=0; row<totalNumPoints_; ++row)
        {
            if(varIndices[k]>=0)
            {
                float dummy = data_[row*nar_ + varIndices[k]];

                if(row==0 || dummy>max_[k])
                    max_[k] = dummy;
                if(row==0 || dummy<min_[k])
                    min_[k] = dummy;
            }
            else if(varIndices[k]<0)
            {
                max_[k]= 1;
                min_[k]=-1;
            }
        }

        mx = max_[k];
        mi =min_[k];
        rounding(mx, mi);
        max_[k] = mx;
        min_[k] = mi;
    }
}

void
Bifurcation::alloc()
{
    ptStability_ = new unsigned char[totalNumPoints_];
    numVerticesEachBranch_ = new int32_t[numBranches_];
    branchID_ = new long[numBranches_];

    data_ = new float[totalNumPoints_*nar_];
    orbits_ = new struct orbit[totalLabels_];
}

void
Bifurcation::denormalizePosition(float position[])
{
    for(int k=0; k<3; k++) {
        float div = (max_[k]-min_[k])/2.0;
        if( !((max_[k]<=1.0 && max_[k]>0.5 &&
              min_[k]>=-1.0 && min_[k]<-0.5 )||
            (div<0.00000001)))
        {
            float avg = (max_[k]+min_[k])/2.0;
            position[k] = position[k]*div+avg;
        }
    }
}

/*go to the end of the line*/
static void go_to_eol(FILE *f)
{
    int i;
    do 
    {
        i = fgetc(f);
    } while (i != '\n' && i != EOF );
}

////////////////////////////////////////////////////////////////////////////////
//
bool
Bifurcation::read(const char *bFileName, int varIndices[])
//
////////////////////////////////////////////////////////////////////////////////
{
    int  branch;
    FILE * inFile;
    inFile = fopen(bFileName, "rt");
    maxndim_ = 0;
    if(!inFile)
    {
        printf(" Cannot open input file: %s\n", bFileName);
        return false;
    }

    long int numBranches   = 0;
    long int totalPoints   = 0;
    long int numPtsInThisBranch = 0;
    int maxColSize  = 0;
    int lb, lbType, lbStability;
    int totalLabels = 0;
    int numPtInCurrentInterval = 0;
    int ndim;
    long int pt = 1;

    while(fscanf(inFile, "%d", &branch) == 1)
    {
        int ic = 0;
        if(branch == 0) {
            pt = 1;
            if(fscanf(inFile, " NDIM=%d", &ndim) == 1 &&
	       ndim > maxndim_)
	        maxndim_ = ndim;
            go_to_eol(inFile);
	}
	else
        {
            long int prevpt = pt;
            fscanf(inFile, "%ld %d %d", &pt, &lbType, &lb);
            int i = 0;
	    char dummystr[25], c;
            while (fscanf(inFile,"%24s%c", dummystr, &c) == 2) {
                data_[totalPoints*nar_+(i++)] = fortranatof(dummystr);
		if(c=='\n')break;
            }

            if(abs(pt) == 1 &&
              // Sometimes the point numbers rotate, like
              // 9996, 9997, 9998, 9999, 1, 2, ...
              // -9996, -9997, 1, 0, -1, -2, ... (an AUTO bug)
              // do not define a new branch if that happens
              (abs(prevpt) != 9999 && abs(prevpt) != 9997 && abs(prevpt) != 0))
            {
                branchID_[numBranches] = branch;
                if(numBranches>0) {
                    numVerticesEachBranch_[numBranches-1] = numPtsInThisBranch;
                }
                ++numBranches;
                numPtsInThisBranch = 0;
            }
            ++numPtsInThisBranch;

            // set the stability of the point
            if(branch > 0)
            {
                lbStability = (pt>0 ? 1 : 2);
            }
            else
            {
                lbStability = (pt>0 ? 3 : 4);
            }

            ++numPtInCurrentInterval;
            if(lb != 0)
            {
                orbits_[totalLabels].numVerticesEachLabelInterval = numPtInCurrentInterval;
                numPtInCurrentInterval = 0;
                orbits_[totalLabels].label = lb;
                clientData.labelIndex[totalLabels][1] = totalPoints;
                clientData.labelIndex[totalLabels][2] = lbType;
                clientData.labelIndex[totalLabels][3] = lbStability;
                totalLabels++;
            }

            maxColSize=(maxColSize>ic) ? maxColSize : ic;
            ptStability_[totalPoints] = lbStability;
            ++totalPoints;
        }
    }

    if(numBranches>0)
        numVerticesEachBranch_[numBranches-1] = numPtsInThisBranch;

#ifdef DEBUG
    cout <<"======================================"<<endl;
    cout <<" nar_ :         "<<nar_<<endl;
    cout <<" numBranches_ : "<<numBranches_<<endl;
    cout <<" totalNumPoints_: "<<totalNumPoints_<<endl;
    cout <<" ID "<<"  numVerticesEachBranch_: "<<endl;
    for(int xi=0; xi<numBranches_; xi++)
       cout << xi<<"    "<<numVerticesEachBranch_[xi]<<endl;
    for(int xi=0; xi<totalLabels; xi++)
    {
         int xxx = clientData.labelIndex[xi][1];
#if 0
         cout <<"index"<<xi<<" | Num points in : "<<clientData.labelIndex[xi][1] 
              <<" | labelIndex: "<<labels_[xi]<< "  | data: "
             <<data_[xxx-1][0]<<" Type: "<<clientData.labelIndex[xi][2]<<endl; 
#endif
    }
    cout <<"======================================"<<endl;

#if 0
    for(int xi=0; xi<220; xi++)
        cout <<" Index: "<<xi<<" "<<data_[xi][0]<<"  "
            <<data_[xi][1]<<"  "<<data_[xi][2]<<endl;
#endif
    cout <<"======================================"<<endl;
#endif

    fclose(inFile);
    return true;
}


////////////////////////////////////////////////////////////////////
//
bool
Bifurcation::parse(const char *bFileName)
//
////////////////////////////////////////////////////////////////////
{
    FILE * inFile;
    totalLabels_ = 0;
    maxndim_ = 0;
    inFile = fopen(bFileName, "rt");
    if(!inFile)
    {
        printf(" Cannot open input file: %s\n", bFileName);
        return false;
    }

    long int totalPoints = 0;
    long int numBranches = 0;
    int maxColSize = 0;
    int branch;
    long int pt = 1;

    while(fscanf(inFile, "%d", &branch) == 1)
    {
        if(branch != 0)
        {
	    int lbType, lb;
            long int prevpt = pt;
            fscanf(inFile, "%ld %d %d", &pt, &lbType, &lb);
	    if (lb != 0) totalLabels_++;
            if(abs(pt) == 1 &&
              // Sometimes the point numbers rotate, like
              // 9996, 9997, 9998, 9999, 1, 2, ...
              // -9996, -9997, 1, 0, -1, -2, ... (an AUTO bug)
              // do not define a new branch if that happens
               (abs(prevpt) != 9999 && abs(prevpt) != 9997 && abs(prevpt) != 0))
                numBranches++;
	    char c;
            int ic = 1;
            while (fscanf(inFile,"%*s%c", &c) == 1 && c !='\n') ic++;
            if (ic > maxColSize) maxColSize = ic;
            ++totalPoints;
        }
        else
	{
            pt = 1;
            go_to_eol(inFile);
        }
    }
    nar_ = maxColSize;
    totalNumPoints_ = totalPoints;
    numBranches_ = numBranches;

    fclose(inFile);
    return true;
}

/////////////////////////////////////////////////////////////////////////////////
//
void
Bifurcation::normalizeData(long int idx, float xyzCoords[3])
//
/////////////////////////////////////////////////////////////////////////////////
{
    for(int k=0; k<3; k++)
    {
        if (varIndices_[k] == -1)
	    xyzCoords[k] = 0.0;
	else
	    xyzCoords[k] = data_[idx*nar_ + varIndices_[k]];
    }
    if(!options[OPT_NORMALIZE_DATA]) return;
    for(int k=0; k<3; k++)
    {
        float avg = (max_[k]+min_[k])/2.0;
        float div = (max_[k]-min_[k])/2.0;
        if( !((max_[k]<=1.0  && max_[k]>0.5 &&
	   min_[k]>=-1.0 && min_[k]<-0.5 )||
	  (div<0.00000001)))
        {
            xyzCoords[k]=(xyzCoords[k]-avg)/div;
        }
    }
}

void
Bifurcation::dealloc()
{
    delete [] ptStability_;
    delete [] numVerticesEachBranch_;
    delete [] branchID_;
    delete [] data_;
    delete [] orbits_;
}
