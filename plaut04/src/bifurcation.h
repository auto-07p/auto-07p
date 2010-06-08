#ifndef BIFURCATION_H
#define BIFURCATION_H

#include "gplaut04.h"

struct BifNode {
    int varIndices[3];
    int32_t *numVerticesEachBranch;
    int32_t numVerticesEachLabelInterval[MAX_LABEL]; 
    int *xAxisItems;
    int *yAxisItems;
    int *zAxisItems;
    int nar;
    int maxndim;

    unsigned char *ptStability;       // start from 0...totalLines-1
    long int totalNumPoints; // like 10012.
    long int numBranches;    // like 4.
    long int *branchID; // branch ID of each branch.
	long int labels[MAX_LABEL];   // this real lenght should equal to totalLabels;
    int      numAxis;        // number of groups of axis. 3 is a group.
	int totalLabels;

    // max saves the maximum value of the coordinate.
    // min saves the minimum value of the coordinate.
    float max[3], min[3];
};

extern BifNode myBifNode;

class Bifurcation {
  public:
    void copyDataToWorkArray(int  varIndices[]);
    SoSeparator *createScene();
  private:
    SoSeparator *render();
    SoSeparator *drawALabel(int row, float xoffset, long int label);
    SoSeparator *drawLabelPtsInScene();
    SoMaterial *setLabelMaterial(int lblType);
    SoSeparator *drawABranchUsingTubes(int iBranch, long int l,
                                       long int sumX, float scaler);
    SoSeparator *drawABranchUsingLines(int iBranch, long int l, long int si, 
                                       float scaler);
    SoSeparator *drawABranchUsingNurbsCurve(int iBranch, long int l,
                                            long int si, float scaler);

};

#endif
