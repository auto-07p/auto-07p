#ifndef PRINTTOPS_H
#define PRINTTOPS_H

#ifdef USE_SOQT
#include <Inventor/Qt/viewers/SoQtExaminerViewer.h>
#else
#include <Inventor/Xt/viewers/SoXtExaminerViewer.h>
#endif
#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/SbColor.h>

SbBool
printToPostScript (SoNode *root, const char *filename,
#ifdef USE_SOQT
SoQtExaminerViewer *viewer, int printerDPI);
#else
SoXtExaminerViewer *viewer, int printerDPI);
#endif

#endif

