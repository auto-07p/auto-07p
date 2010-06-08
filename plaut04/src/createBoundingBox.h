#ifndef CREATEBOUNDINGBOX_H
#define CREATEBOUNDINGBOX_H

#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/SbVec3f.h>

SoSeparator *
drawLine(SbVec3f pointS, SbVec3f pointE, SbVec3f color, float thickness, bool ticker, bool text, int dir);
SoSeparator * createBoundingBox();

#endif
