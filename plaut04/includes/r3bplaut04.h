#ifndef R3BPLAUT04_H
#define R3BPLAUT04_H

#define OPT_DRAW_COORD 0

#define GRAPH_WIDGET_ITEMS 10
#define OPT_REF_PLAN   0
#define OPT_PRIMARY    1
#define OPT_LIB_POINTS  2
#define OPT_PERIOD_ANI  3
#define OPT_SAT_ANI    4
#define OPT_DRAW_LABELS  5
#define OPT_LABEL_NUMBERS  6
#define OPT_BACKGROUND   7
#define OPT_LEGEND   8
#define OPT_NORMALIZE_DATA  9

SoSeparator * createR3BPoints(float nodemin[], float nodemax[]);
SoSeparator * createPrimary(double mass, double pos, float scale, char *txtureFileName);
SoSeparator * createLibrationPoint(float mu, float dis, float scale,
				   int whichCoordSystem);
SoSeparator * createDisk(float where[], float scaler);
void computePrimaryPositionInInertialSystem(int coordSys, float mass, float R, float T, float t,
float bigPosition[], float smallPosition[], float velocity[]);
void smallPrimaryMovingOrbit(float R, float T, float t,
                      float position[], float veloctiy[]);
void satelliteMovingOrbit(int whichcenter, float xyzCoords[],
                     float t, float mu, float R, float Ts, float T, float g,
                     float r[], float v[]);
 
extern float libPtScaler, smallPrimRadius, largePrimRadius, diskTransparency;
extern double mass;
extern int numOfStars;
extern bool diskFromFile;

extern const char * graphWidgetItems[GRAPH_WIDGET_ITEMS];
extern bool blMassDependantOption;

#endif
