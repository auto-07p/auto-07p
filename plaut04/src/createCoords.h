#ifndef CREATECOORDS_H
#define CREATECOORDS_H

#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/SbColor.h>

SoSeparator * createCoordinates(float sclMax[3], float sclMin[3], SbColor color[3]);
SoSeparator * createCoordinates(bool show3D, int where, float sclMax[3], float sclMin[3], int tickers[3], SbColor color[3]);

#endif

