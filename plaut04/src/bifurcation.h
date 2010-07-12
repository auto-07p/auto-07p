#ifndef BIFURCATION_H
#define BIFURCATION_H

#include <stdint.h>
#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/nodes/SoMaterial.h>

class Bifurcation {
  public:
    Bifurcation() {}
    ~Bifurcation() {}
    void copyDataToWorkArray(int  varIndices[]);
    SoSeparator *createScene();
    void alloc();
    void dealloc();
    void denormalizePosition(float position[]);
    bool read(const char *bFileName, int varIndices[]);
    bool parse(const char *bFileName);
    void normalizeData(long int idx, float xyzCoords[3]);

    int nar() const {return nar_;}
    int maxndim() const {return maxndim_;}
    long int branchID(int i) const {return branchID_[i];}
    long int labels(int i) const {return orbits_[i].label;}
    int totalLabels() const {return totalLabels_;}
    long totalNumPoints() const {return totalNumPoints_;}
    int32_t numVerticesEachBranch(int i) const {return numVerticesEachBranch_[i];}
    float *data() const {return data_;}
    
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

    int varIndices_[3];
    int32_t *numVerticesEachBranch_;
    int *xAxisItems_;
    int *yAxisItems_;
    int *zAxisItems_;
    int maxndim_;
    int nar_;

    unsigned char *ptStability_;       // start from 0...totalLines-1
    long int totalNumPoints_; // like 10012.
    long int numBranches_;    // like 4.
    long int *branchID_; // branch ID of each branch.
    struct orbit
    {
        int32_t numVerticesEachLabelInterval; 
        long int label;   // this real lenght should equal to totalLabels;
    } *orbits_;
    int      numAxis_;        // number of groups of axis. 3 is a group.
    int totalLabels_;

    // max saves the maximum value of the coordinate.
    // min saves the minimum value of the coordinate.
    float max_[3], min_[3];

    float *data_;
};

extern Bifurcation *myBifNode;

#endif
