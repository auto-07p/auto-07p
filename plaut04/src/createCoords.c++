#include "createCoords.h"

#include "gplaut04.h"
#include "axis.h"

static struct coordScaleParameters
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


SoSeparator * createCoordinates(bool show3D, int where, float sclMax[3], float sclMin[3], int tickers[3], SbColor color[3])
{
    SoSeparator *axisSep = new SoSeparator;

    float transDis = -1.1;

//create Z-axis
    if(show3D)
    {
        SoSeparator *zSep = new SoSeparator();

        SoTransform  *zXform = new SoTransform;
        if(where == COORD_AT_ORIGIN)
        {
            float x, y;
            x = -1 -sclMin[0] / (sclMax[0] - sclMin[0]) * 2;
            y = -1 -sclMin[1] / (sclMax[1] - sclMin[1]) * 2;
            zXform->translation.setValue(x, 0, -y);
        }
        else if(where == LEFTBACK || where == LEFTAHEAD)
        {
            zXform->translation.setValue(-1.1, 0.0, -1.1);
        }

        cspZ.csclMax = sclMax[2];
        cspZ.csclMin = sclMin[2];
        cspZ.csclNumTicker= tickers[2];
        Axis axisZ = Axis(show3D, where, cspZ.csclMax,cspZ.csclMin,cspZ.csclNumTicker,"Z-Axis/(Z)", 0, color[2]);

        zSep->addChild(zXform);
        zSep->addChild(axisZ.createAxis());
        axisSep->addChild(zSep);
        transDis = -1.1;
    }
    else
    {
        transDis = 0;
    }

//create X-axis
    SoSeparator *xSep = new SoSeparator();

    SoTransform  *xXform = new SoTransform;
    if(where == COORD_AT_ORIGIN)
    {
        float y, z;
        y = -1 -sclMin[1] / (sclMax[1] - sclMin[1]) * 2;
        z = -1 -sclMin[2] / (sclMax[2] - sclMin[2]) * 2;
        xXform->translation.setValue(0.0, z, -y);
    }
    else if(where == LEFTBACK || where == LEFTAHEAD)
    {
        if(where==LEFTBACK)
            xXform->translation.setValue(0.0, transDis, -1.1);
        else
            xXform->translation.setValue(-0.0, transDis, 1.1);

    }
    xXform->rotation.setValue(SbVec3f(0.0, 0.0, 1.0), -M_PI_2);

    cspX.csclMax = sclMax[0];
    cspX.csclMin = sclMin[0];
    cspX.csclNumTicker= tickers[0];
    Axis axisX = Axis(show3D, where, cspX.csclMax,cspX.csclMin,cspX.csclNumTicker,"X-Axis/(X)", 1, color[0]);

    xSep->addChild(xXform);
    xSep->addChild(axisX.createAxis());

//create Y-axis
    SoSeparator *ySep = new SoSeparator();

    SoTransform  *yXform = new SoTransform;
    if(where == COORD_AT_ORIGIN)
    {
        float x, z;
        x = -1 -sclMin[0] / (sclMax[0] - sclMin[0]) * 2;
        z = -1 -sclMin[2] / (sclMax[2] - sclMin[2]) * 2;
        yXform->translation.setValue(x, z, 0.0);
    }
    else if(where == LEFTBACK || where == LEFTAHEAD)
    {
        yXform->translation.setValue(-1.1, transDis, 0.00);
    }

    yXform->rotation.setValue(SbVec3f(1.0, 0.0, 0.0),M_PI_2);

    cspY.csclMax = sclMax[1];
    cspY.csclMin = sclMin[1];
    cspY.csclNumTicker= tickers[1];
    Axis axisY = Axis(show3D, where, cspY.csclMax,cspY.csclMin,cspY.csclNumTicker,"Y-Axis/(Y)", -1, color[1]);

    ySep->addChild(yXform);
    ySep->addChild(axisY.createAxis());

    axisSep->addChild(xSep);
    axisSep->addChild(ySep);

    return axisSep;
}
