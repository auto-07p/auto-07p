#ifndef SOLUTION_H
#define SOLUTION_H

#include "gplaut04.h"

struct SolNode {
    float mass[MAX_LABEL];
    double *time; // this time is useless?
    float (*xyzCoords)[3];
    int32_t *numVerticesEachBranch;  // index start from 0 
    int32_t numVerticesEachPeriod[MAX_LABEL];  // index start from 0
    int32_t *numOrbitsInEachBranch;  // index start from 0
    int *xAxisItems;
    int *yAxisItems;
    int *zAxisItems;
    int nar;
    int npar;

    long int totalNumPoints; // like 10012.
    long int numBranches;    // like 4.
    long int *branchID; // for each label/solution
    long int numOrbits;      // like 43 == UserData.totalLabels == BifNode.totalLabels == totalLabels.
    long int labels[MAX_LABEL];   // this real lenght should equal to numOrbits; 
							// labels[0]==0, HERE, we make an assumption that no label equal to 0.
    long int ntst[MAX_LABEL];   
    long int ncol[MAX_LABEL];   

    int      numAxis;        // number of groups of axis. 3 is a group.
    int      totalLabels;
    double par[MAX_LABEL][MAX_PAR];     // keep the parameter values for each orbit.
    double (*parMax)[MAX_PAR];     // keep the max parameter values in each branch.
    double (*parMin)[MAX_PAR];     // keep the min parameter values.
    double (*parMid)[MAX_PAR];     // keep the mid parameter values.
    int      parID[MAX_PAR];    // keep the id for the par, namely the x for par(x)

    // max saves the maximum value of the coordinate.
    // min saves the minimum value of the coordinate.
    float max[3], min[3];
};

extern SolNode mySolNode;

class Solution {
  public:
    void copyDataToWorkArray(int  varIndices[], int cur, int mx, int to);
    SoSeparator *createSceneWithWidgets();
  private:
    SoSeparator *render();
    SoGroup *renderPoints(int style);
    SoGroup *renderLines();
    SoGroup *renderNurbsCurve();
    SoGroup *renderTubes();
    SoGroup *renderSurface();
    SoSeparator *drawUsingTubes();
    SoSeparator *drawABranchUsingSurface(long obStart, long obEnd,
                long numVert);
    SoSeparator *animateUsingPoints(int style, bool aniColoring);
    SoSeparator *animateUsingLines(bool aniColoring);
    SoSeparator *animateUsingTubes(bool aniColoring);
    SoSeparator *drawAnOrbitUsingPoints(int style, int iBranch,  long int l, 
                long int si, float scaler, int stability, int type,
                bool aniColoring);
    SoSeparator *drawAnOrbitUsingLines(int iBranch,  long int l, long int si, 
                float scaler, int stability, int type, bool aniColoring);
    SoSeparator *drawAnOrbitUsingTubes(int iBranch, long int l, long int si,
                                       float scaler, int stability, int type);
    SoSeparator *drawAnOrbitUsingNurbsCurve(int iBranch, long int l,
                                            long int si, float scaler,
                                            int stability, int type);
    SoSeparator *animateOrbitWithTail(int iBranch, long int j, long int si);
    SoSeparator *drawAPoint(float x, float y, float z, float size, float scale);
    SoSeparator *drawAStrip(float stripSet[][3], int size);
};

#endif
