
#include "axis.h"

struct coordScaleParameters
{
    double csclMax;
    double csclMin;
    int csclNumTicker;
} cspX, cspY, cspZ;

SoSeparator * createCoordinates(float sclMax[3], float sclMin[3], SbColor color[3])
{
    SoSeparator * aSep = new SoSeparator;
    return aSep;
}


SoSeparator * createCoordinates(bool show3D, int type, float sclMax[3], float sclMin[3], int tickers[3], int where)
{
    SbColor color[3];
    color[0].setValue(1,0,0);
    color[1].setValue(0,1,0);
    color[2].setValue(0,0,1);

    SoSeparator *axisSep = new SoSeparator;

    float transDis = -1.1;

//create Z-axsi
    if(show3D)
    {
        SoSeparator *zSep = new SoSeparator();

        SoTransform  *zXform = new SoTransform;
        if(type != 0)
        {
            zXform->translation.setValue(-1.1, 0.0, -1.1);
        }

        cspZ.csclMax = sclMax[2];
        cspZ.csclMin = sclMin[2];
        cspZ.csclNumTicker= tickers[2];
        Axis axisZ = Axis(type, cspZ.csclMax,cspZ.csclMin,cspZ.csclNumTicker,"Z-Axis/(Z)", 0, color[2]);

        zSep->addChild(zXform);
        zSep->addChild(axisZ.createAxis());
        axisSep->addChild(zSep);
        transDis = -1.1;
    }
    else
    {
        transDis = 0;
    }

//create X-axsi
    SoSeparator *xSep = new SoSeparator();

    SoTransform  *xXform = new SoTransform;
    if(type != 0)
    {
        if(where==2)
            xXform->translation.setValue(0.0, transDis, -1.1);
        else
            xXform->translation.setValue(-0.0, transDis, 1.1);

    }
    xXform->rotation.setValue(SbVec3f(0.0, 0.0, 1.0), -M_PI_2);

    cspX.csclMax = sclMax[0];
    cspX.csclMin = sclMin[0];
    cspX.csclNumTicker= tickers[0];
    Axis axisX = Axis(show3D, type, cspX.csclMax,cspX.csclMin,cspX.csclNumTicker,"X-Axis/(X)", 1, color[0]);

    xSep->addChild(xXform);
    xSep->addChild(axisX.createAxis());

//create Y-axsi
    SoSeparator *ySep = new SoSeparator();

    SoTransform  *yXform = new SoTransform;
    if(type != 0)
    {
        yXform->translation.setValue(-1.1, transDis, 0.00);
    }

    yXform->rotation.setValue(SbVec3f(1.0, 0.0, 0.0),M_PI_2);

    cspY.csclMax = sclMax[1];
    cspY.csclMin = sclMin[1];
    cspY.csclNumTicker= tickers[1];
    Axis axisY = Axis(type, cspY.csclMax,cspY.csclMin,cspY.csclNumTicker,"Y-Axis/(Y)", -1, color[1]);

    ySep->addChild(yXform);
    ySep->addChild(axisY.createAxis());

    axisSep->addChild(xSep);
    axisSep->addChild(ySep);

    return axisSep;
}
