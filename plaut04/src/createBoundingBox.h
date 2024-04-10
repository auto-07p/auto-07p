#ifndef CREATEBOUNDINGBOX_H
#define CREATEBOUNDINGBOX_H

#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/SbLinear.h>

SoSeparator *
drawLine(SbVec3f pointS, SbVec3f pointE, float thickness, bool ticker, bool text, int dir);
SoSeparator * createBoundingBox();

#endif
