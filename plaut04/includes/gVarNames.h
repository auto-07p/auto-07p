    char *typeTokenNames[] =
    {
        "DEFAULT","BP ALG", "LP ALG",
        "HB", "UZ4", "UZ-4",
        "LP DIF", "BP DIF", "PD",
        "TR", "EP", "MX",
        "OTHERS"
    };

   char *intVariableNames[] =
    {
        "Graph Type",
        "Graph Style",
#ifdef R3B
        "Coordinate system",
#endif
        "Window Width",
        "Window Height",
//        "Labels",
        "Coloring Method",
#ifdef R3B
        "Number of Period Animated",
#endif
        "Line Width Scaler",
#ifndef R3B
        "Object Radius",
#else
        "Satellite Radius",
        "Large Primary Radius",
        "Small Primary Radius",
        "Number of Stars",
        "Libration Point Size",
#endif
        "AniLine Thickness Scaler",
#ifndef R3B
        "Object Max Animation Speed",
        "Object Min Animation Speed",
        "Orbit Max Animation Speed",
        "Orbit Min Animation Speed",
#else
        "Sat Max Animation Speed",
        "Sat Min Animation Speed",
#endif
        "Coordinate Type",
        "Background Transparency",
#ifndef R3B
        "Number of Period Animated",
#else
	"Disk Transparency",
	"Disk From File",
        "Orbit Max Animation Speed",
        "Orbit Min Animation Speed",
#endif
    };

    char * hexdecimalVarNames[] =
    {
        "UNSTABLE LINE PATTERN",
        "STABLE LINE PATTERN"
    };

   char * nDataVarNames[] =
    {
        "Background Color",
        "X Axis Color",
        "Y Axis Color",
        "Z Axis Color",
#ifndef R3B
        "Object Color",
        "Surface Color",
#else
        "satellite Color",
        "large primary Color",
        "large primary tail Color",
        "small primary Color",
        "small primary tail Color",
        "surface Color",
#endif
        "Unstable Solution Color",
#ifndef R3B
        "Stable Solution Color",
#else
        "Stable Solution Color"
#endif
    };

    char * blWidgetName[] =
    {
        "3D",
        "3DBif",
        "3DSol",
    };

char * graphWidgetItems[]=
{
#ifndef R3B
    "Highlight Orbit",
#else
    "Draw Reference Plane", 
    "Draw Primaries", 
    "Draw Libration Points",
#endif
    "Orbit Animation", 
#ifdef R3B
    "Satellite Animation",
#endif
    "Draw Background", 
    "Draw Legend", 
    "Normalize Data"
};

char * axesNames[] =
{
        "X Axis Solution",
        "Y Axis Solution",
        "Z Axis Solution",
        "X Axis Bifurcation",
        "Y Axis Bifurcation",
        "Z Axis Bifurcation",
};
