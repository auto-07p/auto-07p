#include "axis.h"

#define FONT_SIZE 12

//////////////////////////////////////////////////////////////
//
Axis::Axis()
//
//////////////////////////////////////////////////////////////
{
    maxValue = 1;
    minValue = -1;
    numOfTickers = 10;
    color.setValue(1,0,0);

    strcpy(name,"Y");

    double delta = (maxValue-minValue)/numOfTickers;
    for(int i = 0; i <= numOfTickers; ++i)
    {
        tickers[i] = minValue + i * delta;
    }
}


//////////////////////////////////////////////////////////////
//
Axis::~Axis()
//
//////////////////////////////////////////////////////////////
{
}


//////////////////////////////////////////////////////////////
//
Axis::Axis(bool s3D, int ty, double mxValue, double miValue, 
         int numTickers, const char* aName, int wh, SbColor clr)
//
//////////////////////////////////////////////////////////////
{
    show3D = s3D;
    set(ty, mxValue, miValue, numTickers, aName, wh, clr);
}


//////////////////////////////////////////////////////////////
//
Axis::Axis(int ty, double mxValue, double miValue, int numTickers, 
         const char* aName, int wh, SbColor clr)
//
//////////////////////////////////////////////////////////////
{
    show3D = false;
    set(ty, mxValue, miValue, numTickers, aName, wh, clr);
}


//////////////////////////////////////////////////////////////
//
void 
Axis::set(int ty, double mxValue, double mnValue, int numTickers, 
          const char* aName, int wh, SbColor clr)
//
//////////////////////////////////////////////////////////////
{
    type = ty;
    maxValue = mxValue;
    minValue = mnValue;
    numOfTickers = (numTickers<11) ? numTickers : 10;
    which = wh;
    color = clr;
    strcpy(name, aName);

    double delta = (maxValue-minValue)/numOfTickers;
    for(int i = 0; i <= numOfTickers; ++i)
    {
        tickers[i] = minValue + i * delta;
    }
}


//////////////////////////////////////////////////////////////
//
SoSeparator * 
Axis::createAxis()
//
//////////////////////////////////////////////////////////////
{
    SoSeparator * axis = new SoSeparator;
    axis->ref();

    SoMaterial *mtl = new SoMaterial;
    mtl->diffuseColor.setValue(color);
    axis->addChild(mtl);

    SoCylinder *cyl = new SoCylinder;
    cyl->radius = 2.0/500.0;
    cyl->height = 2.2;
    axis->addChild(cyl);

    if(type == 0)
        axis->addChild(drawArrow());

    float position;
    if(type != 0)
    {
#ifdef TD_FONT
        position = 0;
#else
        position = 1.1;
#endif
    }
    else
        position = 1.2;

    axis->addChild(drawAxisName(position));

// draw tickers
    if(numOfTickers>0)
        for(int i = 0; i <= numOfTickers; ++i)
            axis->addChild(drawTicker(-1.0+i*2.0/numOfTickers, 0.04));

// draw scales
    if(numOfTickers>0)
        for(int i = 0; i <= numOfTickers; ++i)
            axis->addChild(drawScale(-1.0+i*2.0/numOfTickers, tickers[i]));

    return axis;

}


//////////////////////////////////////////////////////////////
//
SoSeparator * 
Axis::drawAxisName(float pos)
//
//////////////////////////////////////////////////////////////
{
    SoSeparator *nameSep = new SoSeparator;

    SoFont *axFont = new SoFont;
    axFont->name.setValue("Helvetica");
    axFont->size.setValue(FONT_SIZE);

#ifdef TD_FONT
    SoText3 *axName = new SoText3;
#else
    SoText2 *axName = new SoText2;
#endif
    axName->string.setValue(name);

    SoTransform *tkXform = new SoTransform;
    if(type != 0)
    {
        if(which == -1)
        {
            tkXform->translation.setValue(-0.4, pos, 0.00);
            tkXform->rotation.setValue(SbVec3f(0.0,0.0,1.0), M_PI_2);
            nameSep->addChild(tkXform);

            SoTransform *tkXform2 = new SoTransform;
            tkXform2->rotation.setValue(SbVec3f(1.0,0.0,0.0), M_PI);
            nameSep->addChild(tkXform2);
        }
        else if( which == 1)
        {
            tkXform->translation.setValue(0.2, pos, 0.00);
            tkXform->rotation.setValue(SbVec3f(0.0,0.0,1.0),M_PI_2);
            nameSep->addChild(tkXform);
        }
        else
        {
            tkXform->translation.setValue(-0.4, pos, 0.00);
            tkXform->rotation.setValue(SbVec3f(0.0,0.0,1.0),M_PI_2);
            nameSep->addChild(tkXform);
        }
    }
    else
    {
        if(which == -1)
        {
            SoTransform *tkXform2 = new SoTransform;
            tkXform2->rotation.setValue(SbVec3f(0.0,0.0,1.0), M_PI);

            tkXform->translation.setValue(-0.2, -pos, 0.00);
            tkXform->rotation.setValue(SbVec3f(0.0,1.0,0.0), M_PI);
            nameSep->addChild(tkXform);

            nameSep->addChild(tkXform2);
        }
        else if( which == 1)
        {
            tkXform->translation.setValue(0.0, pos, 0.00);
            tkXform->rotation.setValue(SbVec3f(0.0,0.0,1.0),M_PI_2);
            nameSep->addChild(tkXform);
        }
        else
        {
            tkXform->translation.setValue(-0.2, pos, 0.00);
            nameSep->addChild(tkXform);
        }
    }

    nameSep->addChild(axFont);
    nameSep->addChild(axName);

    return nameSep;
}


//////////////////////////////////////////////////////////////
//
SoSeparator * 
Axis::drawTicker(float pos, float height)
//
//////////////////////////////////////////////////////////////
{
    SoSeparator *tkSep = new SoSeparator;
    SoCylinder *ticker = new SoCylinder;
    ticker->radius = height/10.0;  
    ticker->height = height;      

    SoTransform *tkXform = new SoTransform;

    if(which == -1)
    {
        tkXform->translation.setValue(0.02, pos, 0.00);
    }
    else if(which == 1)
    {
        tkXform->translation.setValue(-0.02, pos, 0.00);
    }
    else
    {
        tkXform->translation.setValue(0.02, pos, 0.00);
    }

    if(show3D)
        tkXform->rotation.setValue(SbVec3f(0.0,0.0,1.0),M_PI_2);
    else
        tkXform->rotation.setValue(SbVec3f(1.0,0.0,0.0),M_PI_2);

    tkSep->addChild(tkXform);
    tkSep->addChild(ticker);
    return  tkSep;
}


//////////////////////////////////////////////////////////////
//
SoSeparator * 
Axis::drawScale(float pos, float scale)
//
//////////////////////////////////////////////////////////////
{
    SoSeparator * scaleSep = new SoSeparator;

    SoFont *sclFont = new SoFont;
    sclFont->name.setValue("Helvetica");
    sclFont->size.setValue(FONT_SIZE);

    SoTransform *tkXform = new SoTransform;
    if(which == -1)
    {
        tkXform->translation.setValue(-0.35, -pos, 0.00);
        tkXform->rotation.setValue(SbVec3f(1.0,0.0,0.0),M_PI);
    }
    else if(which == 1)
    {
        if(show3D)
        {
            tkXform->translation.setValue(0.08, pos-0.05, 0.00);
            tkXform->rotation.setValue(SbVec3f(0.0,0.0,1.0),M_PI_2);
        }
        else
        {
            if(type == 2)
                tkXform->translation.setValue(0.00, pos-0.05, -0.20);
            else
                tkXform->translation.setValue(0.00, pos-0.05, 0.20);
        }
    }
    else
    {
        tkXform->translation.setValue(-0.25, pos, 0.00);
    }

    char strScale[10];
    sprintf(strScale, "%8.2e", scale);

#ifdef TD_FONT
    SoText3 *axScale = new SoText3;
#else
    SoText2 *axScale = new SoText2;
#endif
    axScale->string.setValue(strScale);

    scaleSep->addChild(tkXform);
    scaleSep->addChild(sclFont);
    scaleSep->addChild(axScale);
    return  scaleSep;
}


//////////////////////////////////////////////////////////////
//
SoSeparator * 
Axis::drawArrow()
//
//////////////////////////////////////////////////////////////
{
    SoSeparator * arrSep = new SoSeparator;
    SoTransform * arrXform = new SoTransform;
    if( which == -1 )
    {
        arrXform->translation.setValue(0.0, -1.1, 0.0);
        arrXform->rotation.setValue(SbVec3f(1, 0, 0), M_PI);
    }
    else
        arrXform->translation.setValue(0.0, 1.1, 0.0);

    arrSep->addChild(arrXform);

    SoCone * arr = new SoCone;
    arr->bottomRadius = 0.01;
    arr->height =0.1;
    arrSep->addChild(arr);

    return arrSep;
}
