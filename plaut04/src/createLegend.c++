#include "createLegend.h"

#include <Inventor/nodes/SoDirectionalLight.h>
#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/nodes/SoTransform.h>
#include <Inventor/nodes/SoTexture2.h>
#include <Inventor/nodes/SoMaterial.h>
#include <Inventor/nodes/SoMaterialBinding.h>
#include <Inventor/nodes/SoCoordinate3.h>
#include <Inventor/nodes/SoTriangleStripSet.h>
#include <Inventor/nodes/SoShapeHints.h>
#include <Inventor/nodes/SoText3.h>
#include <Inventor/nodes/SoText2.h>
#include <Inventor/nodes/SoFont.h>

#include <Inventor/nodes/SoPerspectiveCamera.h>

#define FONT_SIZE 12 

///////////////////////////////////////////////////////////////////////////////
//
SoSeparator * 
printScaleAt(SbVec3f position, double size, double value)
//
////////////////////////////////////////////////////////////////////////////////
{
    SoSeparator * textSep = new SoSeparator;

    SoMaterial  * txtMtl  = new SoMaterial;
    txtMtl->diffuseColor.setValue(1, 1, 1);
    textSep->addChild(txtMtl);

    SoFont * txtFont = new SoFont;
    txtFont->size = size;
    textSep->addChild(txtFont);

    SoTransform * txtXform = new SoTransform;
    txtXform->translation.setValue(position);
    textSep->addChild(txtXform);

    char strScale[10];
    sprintf(strScale, "%8.3e", value);

    SoText2 * txt = new SoText2;
    txt->string.setValue(strScale);
    textSep->addChild(txt);
    return textSep;
}

////////////////////////////////////////////////////////////////////////////////
//
SoSeparator * 
printScaleAt(SbVec3f position, double size, const char* strScale)
//
////////////////////////////////////////////////////////////////////////////////
{
    SoSeparator * textSep = new SoSeparator;

    SoMaterial  * txtMtl  = new SoMaterial;
    txtMtl->diffuseColor.setValue(1, 1, 1);
    textSep->addChild(txtMtl);

    SoFont * txtFont = new SoFont;
    txtFont->size = size;
    textSep->addChild(txtFont);

    SoTransform * txtXform = new SoTransform;
    txtXform->translation.setValue(position);
    textSep->addChild(txtXform);

//SoText3 * txt = new SoText3;
    SoText2 * txt = new SoText2;
    txt->string.setValue(strScale);
    textSep->addChild(txt);
    return textSep;
}

////////////////////////////////////////////////////////////////////////////////
//
SoSeparator * 
createLegend(SbVec3f pos, double values[5])
//
////////////////////////////////////////////////////////////////////////////////
{
    SoSeparator *legend= new SoSeparator;
    SoMaterial *legendMaterial = new SoMaterial;
    SoTransform *legendXform = new SoTransform;

//   SoOrthographicCamera *legendCamera = new SoOrthographicCamera;
//   legend->addChild(legendCamera);

    legendXform->translation.setValue(pos);
    legendXform->scaleFactor.setValue(0.2,0.5,1);
//    legendXform->rotation.setValue(-1,0,0,M_PI_2);
    legend->addChild(legendXform);

    static float vertexPositions[6][3]=
    {
        {
            0.25, -0.5, 0
        }
        ,
        {
            -0.25, -0.5, 0
        }
        ,
        {
            0.25, 0.0, 0
        },
        {-0.25, 0.0,0},
        {
            0.25,0.5,0
        }
        ,
        {
            -0.25, 0.5,0
        }
    };

    static float colors[6][3] =
    {
        {
            0,0,1
        }
        ,
        {
            0,0,1
        }
        ,
        {
            0,1,0
        }
        ,
        {
            0,1,0
        }
        ,
        {
            1,0,0
        }
        ,
        {
            1,0,0
        }
    };
    static int32_t numVertices[1]={6};

    SoShapeHints * legendHints = new SoShapeHints;
    legendHints->vertexOrdering = SoShapeHints::COUNTERCLOCKWISE;
    legend->addChild(legendHints);

    legendMaterial->diffuseColor.setValues(0, 6, colors);
//legendMaterial->transparency = 0.8;
    legend->addChild(legendMaterial);

    SoMaterialBinding *legendMtlBinding = new SoMaterialBinding;
    legendMtlBinding->value = SoMaterialBinding::PER_VERTEX;
    legend->addChild(legendMtlBinding);

    SoCoordinate3 *legendCoords = new SoCoordinate3;
    legendCoords->point.setValues(0,6,vertexPositions);
    legend->addChild(legendCoords);

    SoTriangleStripSet * legendStripSet = new SoTriangleStripSet;
    legendStripSet->numVertices.setValues(0, 1, numVertices);
    legend->addChild(legendStripSet);

    SbVec3f vec;
    for(int i=0; i<5; ++i)
    {
        vec.setValue(0.30, i*0.25-0.55, 0);
        legend->addChild(printScaleAt(vec, FONT_SIZE, values[i]));
    }
    return legend;
}


///////////////////////////////////////////////////////////////////////////////
//
SoSeparator * 
createDiscreteLegend(SbVec3f pos, SbColor lineColors[13])
///////////////////////////////////////////////////////////////////////////////
{
    const char *tyNames[13]=
    {
        "None","BP Alg", "Fold Alg", "Hopf Bif",
        "Reg UZ","UZ Par","Fold Dif", "BP Dif",
        "Pd DBL","Torus","End Pt","No Conv","OTHERS"
    };
    SoSeparator *legend= new SoSeparator;
    SoMaterial *legendMaterial = new SoMaterial;
    SoTransform *legendXform = new SoTransform;


    legendXform->translation.setValue(pos);
    legendXform->scaleFactor.setValue(0.2,0.5,1);
    legend->addChild(legendXform);

    static float vertexPositions[13*4][3];

    static SbColor colors[13]; 
    static int32_t numVertices[13]={4,4,4,4,4,4,4,4,4,4,4,4,4};
    float tcolor[3];

    int j;
    for(int i = 0; i < 13; ++i)
    {
        j = i*4;
        vertexPositions[j][0]= 0.25;
        vertexPositions[j][1]= -0.5 + i/13.0;
        vertexPositions[j][2]= 0;

        vertexPositions[j+1][0]= -0.25;
        vertexPositions[j+1][1]= vertexPositions[j][1];
        vertexPositions[j+1][2]= 0;

        vertexPositions[j+2][0]= 0.25;
        vertexPositions[j+2][1]= -0.5 + (i+1)/13.0;
        vertexPositions[j+2][2]= 0;

        vertexPositions[j+3][0]= -0.25;
        vertexPositions[j+3][1]= vertexPositions[j+2][1];
        vertexPositions[j+3][2]= 0;

        lineColors[i].getValue(tcolor[0],tcolor[1], tcolor[2]);
        colors[i].setValue(tcolor);
    }

    SoShapeHints * legendHints = new SoShapeHints;
    legendHints->vertexOrdering = SoShapeHints::COUNTERCLOCKWISE;
    legend->addChild(legendHints);

    legendMaterial->diffuseColor.setValues(0, 13, colors);
    legend->addChild(legendMaterial);

    SoMaterialBinding *legendMtlBinding = new SoMaterialBinding;
    legendMtlBinding->value = SoMaterialBinding::PER_PART;
    legend->addChild(legendMtlBinding);

    SoCoordinate3 *legendCoords = new SoCoordinate3;
    legendCoords->point.setValues(0,52,vertexPositions);
    legend->addChild(legendCoords);

    SoTriangleStripSet * legendStripSet = new SoTriangleStripSet;
    legendStripSet->numVertices.setValues(0, 13, numVertices);
    legend->addChild(legendStripSet);

    SbVec3f vec;
    for(int i=0; i<13; ++i)
    {
        vec.setValue(0.28, -0.5+i/13.0, 0);
        legend->addChild(printScaleAt(vec, FONT_SIZE, tyNames[i]));
    }
    return legend;
}


///////////////////////////////////////////////////////////////////////////////
//
SoSeparator * 
createBranchLegend(SbVec3f pos, SbColor lineColors[13])
//
///////////////////////////////////////////////////////////////////////////////
{
    char tyNames[13][10]=
    {
        "BR 1","BR 2", "BR 3", "BR 4",
        "BR 5","BR 6","BR 7", "BR 8",
        "BR 9","BR 10","BR 11","BR 12","BR 13"
    };
    SoSeparator *legend= new SoSeparator;
    SoMaterial *legendMaterial = new SoMaterial;
    SoTransform *legendXform = new SoTransform;

    legendXform->translation.setValue(pos);
    legendXform->scaleFactor.setValue(0.2,0.5,1);
    legend->addChild(legendXform);

    static float vertexPositions[13*4][3];

    static SbColor colors[13];   
    static int32_t numVertices[13]={4,4,4,4,4,4,4,4,4,4,4,4,4};
    float tcolor[3];

    int j;
    for(int i = 0; i < 13; ++i)
    {
        j = i*4;
        vertexPositions[j][0]= 0.25;
        vertexPositions[j][1]= -0.5 + i/13.0;
        vertexPositions[j][2]= 0;

        vertexPositions[j+1][0]= -0.25;
        vertexPositions[j+1][1]= vertexPositions[j][1];
        vertexPositions[j+1][2]= 0;

        vertexPositions[j+2][0]= 0.25;
        vertexPositions[j+2][1]= -0.5 + (i+1)/13.0;
        vertexPositions[j+2][2]= 0;

        vertexPositions[j+3][0]= -0.25;
        vertexPositions[j+3][1]= vertexPositions[j+2][1];
        vertexPositions[j+3][2]= 0;

        lineColors[i].getValue(tcolor[0],tcolor[1], tcolor[2]);
        colors[i].setValue(tcolor);
    }

    SoShapeHints * legendHints = new SoShapeHints;
    legendHints->vertexOrdering = SoShapeHints::COUNTERCLOCKWISE;
    legend->addChild(legendHints);

    legendMaterial->diffuseColor.setValues(0, 13, colors);
    legend->addChild(legendMaterial);

    SoMaterialBinding *legendMtlBinding = new SoMaterialBinding;
    legendMtlBinding->value = SoMaterialBinding::PER_PART;
    legend->addChild(legendMtlBinding);

    SoCoordinate3 *legendCoords = new SoCoordinate3;
    legendCoords->point.setValues(0,52,vertexPositions);
    legend->addChild(legendCoords);

    SoTriangleStripSet * legendStripSet = new SoTriangleStripSet;
    legendStripSet->numVertices.setValues(0, 13, numVertices);
    legend->addChild(legendStripSet);

    SbVec3f vec;
    for(int i=0; i<13; ++i)
    {
        vec.setValue(0.28, -0.5+i/13.0, 0);
        legend->addChild(printScaleAt(vec, FONT_SIZE, tyNames[i]));
    }
    return legend;
}

///////////////////////////////////////////////////////////////////////////////
//
SoSeparator * 
createStabilityLegend(SbVec3f pos, SbColor lineColors[2])
//
///////////////////////////////////////////////////////////////////////////////
{
    char tyNames[][10]={"UNSTB","STB"};
    SoSeparator *legend= new SoSeparator;
    SoMaterial *legendMaterial = new SoMaterial;
    SoTransform *legendXform = new SoTransform;


    legendXform->translation.setValue(pos);
    legendXform->scaleFactor.setValue(0.2,0.5,1);
    legend->addChild(legendXform);

    static float vertexPositions[2*4][3];

    static SbColor colors[2];
    static int32_t numVertices[2]={4,4};
    float tcolor[3];

    int j;
    for(int i = 0; i < 2; ++i)
    {
        j = i*4;
        vertexPositions[j][0]= 0.26;
        vertexPositions[j][1]= -0.5 + i/2.0;
        vertexPositions[j][2]= 0;

        vertexPositions[j+1][0]= -0.24;
        vertexPositions[j+1][1]= vertexPositions[j][1];
        vertexPositions[j+1][2]= 0;

        vertexPositions[j+2][0]= 0.26;
        vertexPositions[j+2][1]= -0.5 + (i+1)/2.0;
        vertexPositions[j+2][2]= 0;

        vertexPositions[j+3][0]= -0.24;
        vertexPositions[j+3][1]= vertexPositions[j+2][1];
        vertexPositions[j+3][2]= 0;

        lineColors[i].getValue(tcolor[0],tcolor[1], tcolor[2]);
        colors[i].setValue(tcolor);
    }

    SoShapeHints * legendHints = new SoShapeHints;
    legendHints->vertexOrdering = SoShapeHints::COUNTERCLOCKWISE;
    legend->addChild(legendHints);

    legendMaterial->diffuseColor.setValues(0, 2, colors);
    legend->addChild(legendMaterial);

    SoMaterialBinding *legendMtlBinding = new SoMaterialBinding;
    legendMtlBinding->value = SoMaterialBinding::PER_PART;
    legend->addChild(legendMtlBinding);

    SoCoordinate3 *legendCoords = new SoCoordinate3;
    legendCoords->point.setValues(0,52,vertexPositions);
    legend->addChild(legendCoords);

    SoTriangleStripSet * legendStripSet = new SoTriangleStripSet;
    legendStripSet->numVertices.setValues(0, 2, numVertices);
    legend->addChild(legendStripSet);

    SbVec3f vec;
    for(int i=0; i<2; ++i)
    {
        vec.setValue(0.28, -0.5+i/2.0, 0);
        legend->addChild(printScaleAt(vec, FONT_SIZE, tyNames[i]));
    }
    return legend;
}
