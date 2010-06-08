#ifndef GREADSOL_H
#define GREADSOL_H

#include "gplaut04.h"

solutionp
parseSolution( const char* sFileName, bool & blOpenFile, long int &total, long int &totalNumPoints);

bool readSolution(solutionp current, const char* sFileName, int varIndices[]);

#endif
