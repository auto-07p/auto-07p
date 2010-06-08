#ifndef CREATELEGEND_H
#define CREATELEGEND_H

#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/SbVec3f.h>
#include <Inventor/SbColor.h>

SoSeparator *printScaleAt(SbVec3f position, double size, double value);
SoSeparator *printScaleAt(SbVec3f position, double size, const char* strScale);
SoSeparator *createLegend(SbVec3f pos, double values[5]);
SoSeparator *createDiscreteLegend(SbVec3f pos, SbColor lineColors[13]);
SoSeparator *createBranchLegend(SbVec3f pos, SbColor lineColors[13]);
SoSeparator *createStabilityLegend(SbVec3f pos, SbColor lineColors[2]);

#endif
