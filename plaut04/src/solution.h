#ifndef SOLUTION_H
#define SOLUTION_H

#include <queue>

#include "gplaut04.h"

class Solution {
  public:
    Solution() {}
    ~Solution() {}
    void copyDataToWorkArray(int  varIndices[], int cur, int mx, int to);
    SoSeparator *createSceneWithWidgets();
    SoSeparator *createInertialFrameScene(float dis);
    bool parse(const char* sFileName);
    bool read(const char* sFileName, int varIndices[]);
    void normalizeData();
    void alloc();
    void dealloc();
    void searchForMaxMin(int component, int  varIndices[]);
    void denormalizePosition(float position[]);
    void set_parID(int parID[], int size);

    int nar() {return nar_;}
    int npar() {return npar_;}
    int *parID() {return parID_;}
    long int numOrbits() {return numOrbits_;}
    long int *labels() {return labels_;}
    int totalLabels() {return totalLabels_;}
    long totalNumPoints() {return totalNumPoints_;}
    int32_t *numVerticesEachPeriod() {return numVerticesEachPeriod_;}
    float **data() {return data_;}
    float *masses() {return masses_;}

    void set_npar(int npar) {npar_ = npar;}

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

    SoSeparator *animateOrbitInertialSysUsingLine(int iBranch, int iOrbit,
     float (*vertices)[3], float (*largePrimPos)[3], float (*smallPrimPos)[3],
     float * myColorBase, float period, int size,
     float scaler, int stability, int type);
    SoSeparator *drawAnOrbitInertialSysUsingLines(int iBranch, int iOrbit,
     float (*myVertices)[3], float *myColorBase,
     long int arrSize, float scaler, int stability, int type);
    void convertDataToInertialSystem(float (*myVertices)[3],
     float *timeEqualDiv, float *myColorBase,
     long int arrSize, long int orbitSize, long int kth, long int sumX);
    SoSeparator *drawAnOrbitInertialSysUsingTubes(int iBranch,  int iOrbit,
     float (*myVertices)[3], float *myColorBase, const long int arrSize,
     const float tubeRadiusScaler, const int stability, const int type);
    SoSeparator *drawAnOrbitInertialSysUsingNurbsCurve(int iBranch,
     int iOrbit, float (*myVertices)[3], const long int arrSize,
     const float scaler, const int stability, const int type);
#if 0 // unused R3B related functions
    SoSeparator * animateIner2(long int j, long int si);
    SoSeparator * animateOrbitMovement(long int n, long int si);
    SoSeparator * animateOrbitCalSteps(long int n,long int si);
    SoSeparator * animateOrbitWithNurbsCurveTail(long int j,long int si);
    SoSeparator *drawEarthMovement(int k);
    void calPrimPos(float t, float pos[]);
    void calSatPos(int center, float mu, float t, float primPos[], float satPos[]);
    SoSeparator *drawASphereWithColor(float color[], float position[], float size);
#endif

    std::queue<long> positions_;
    float masses_[MAX_LABEL];
    double *time_; // this time is useless?
    float (*xyzCoords_)[3];
    int32_t *numVerticesEachBranch_;  // index start from 0 
    int32_t numVerticesEachPeriod_[MAX_LABEL];  // index start from 0
    int32_t *numOrbitsInEachBranch_;  // index start from 0
    int nar_;
    int npar_;

    long int totalNumPoints_; // like 10012.
    long int numBranches_;    // like 4.
    long int *branchID_; // for each label/solution
    long int numOrbits_;      // like 43 == UserData.totalLabels == BifNode.totalLabels == totalLabels.
    long int labels_[MAX_LABEL];   // this real lenght should equal to numOrbits; 
							// labels[0]==0, HERE, we make an assumption that no label equal to 0.
    long int ntst_[MAX_LABEL];
    long int ncol_[MAX_LABEL];

    int      numAxis_;        // number of groups of axis. 3 is a group.
    int      totalLabels_;
    double par_[MAX_LABEL][MAX_PAR];     // keep the parameter values for each orbit.
    double (*parMax_)[MAX_PAR];     // keep the max parameter values in each branch.
    double (*parMin_)[MAX_PAR];     // keep the min parameter values.
    double (*parMid_)[MAX_PAR];     // keep the mid parameter values.
    int      parID_[MAX_PAR];    // keep the id for the par, namely the x for par(x)

    // max saves the maximum value of the coordinate.
    // min saves the minimum value of the coordinate.
    float max_[3], min_[3];

    float period_[MAX_LABEL];       // from the solution file par[10];
    float **data_;
};

extern Solution *mySolNode;

#endif
