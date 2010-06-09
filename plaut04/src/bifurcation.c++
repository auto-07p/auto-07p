#include "bifurcation.h"

#include "createCoords.h"
#include "normalizeBifData.h"
#include "rounding.h"
#include "tube.h"

BifNode myBifNode;

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

    if(useR3B)
        result->addChild(createR3BPoints(myBifNode.min, myBifNode.max));

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
Bifurcation::drawLabelPtsInScene()
//
//////////////////////////////////////////////////////////////////////////
{
    float dis = !options[OPT_NORMALIZE_DATA] ? fabs(std::max(std::max(
        (myBifNode.max[0]-myBifNode.min[0]),
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
        } while( k < myBifNode.totalLabels);
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
                colorBase[i*11+k]  = myBifNode.data[idx*myBifNode.nar +
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
        if(useR3B)
            iBranch = myBifNode.branchID[iBranch];
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
            colorBase[curSize]  = myBifNode.data[idx * myBifNode.nar +
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
            if (useR3B)
                iBranch = myBifNode.branchID[iBranch];
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
            colorBase[curSize]  = myBifNode.data[(idx-1) * myBifNode.nar +
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
Bifurcation::drawABranchUsingNurbsCurve(int iBranch, long int l,
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
            colorBase[m]  =myBifNode.data[idx * myBifNode.nar + 
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
            colorBase[m]  =myBifNode.data[idx * myBifNode.nar +
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
        for(int ka=0; ka<myBifNode.numBranches; ka++)
        {
            bifGroup->addChild(drawABranchUsingTubes(ka, k, si, 1*lineWidthScaler));
            k = k+1;
            si += myBifNode.numVerticesEachBranch[ka];
        }
    }
    else if (whichStyle == NURBS)
    {
        long int si = 0, k = 0;
        for(int ka=0; ka<myBifNode.numBranches; ka++)
        {
            bifGroup->addChild(drawABranchUsingNurbsCurve(ka, k, si, 1*lineWidthScaler));
            k = k+1;
            si += myBifNode.numVerticesEachBranch[ka];
        }

    }
    else 
    {
        long int si = 0, k = 0;
        for(int ka=0; ka<myBifNode.numBranches; ka++)
        {
            bifGroup->addChild(drawABranchUsingLines(ka, k, si, 1*lineWidthScaler));
            k = k+1;
            si += myBifNode.numVerticesEachBranch[ka];
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
        myBifNode.varIndices[k] = varIndices[k];
        for(long int row=0; row<myBifNode.totalNumPoints; ++row)
        {
            if(varIndices[k]>=0)
            {
                float dummy = myBifNode.data[row*myBifNode.nar + varIndices[k]];

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
