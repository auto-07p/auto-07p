#ifndef DRAWCOORDS_H
#define DRAWCOORDS_H

#include <Inventor/nodes/SoSeparator.h>

#if 0
SoSeparator *drawCoords(int where, float pos[], SbVec3f axisColors[],
                        float height);
#endif
SoSeparator *drawTicker(float pos,float height);
SoSeparator *createAxis(float red, float green, float blue, float height,
                        const char *);

#endif
