    const char *typeTokenNames[] =
    {
        "DEFAULT","BP ALG", "LP ALG",
        "HB", "UZ4", "UZ-4",
        "LP DIF", "BP DIF", "PD",
        "TR", "EP", "MX",
        "OTHERS"
    };

   const char *intVariableNames[] =
    {
        "Graph Type",
        "Graph Style",
        "Window Width",
        "Window Height",
//        "Labels",
        "Coloring Method",
        "Coloring Method Solution",
        "Coloring Method Bifurcation",
        "Line Width Scaler",
        "AniLine Thickness Scaler",
#ifndef R3B
        "Object Animation Speed",
        "Object Max Animation Speed",
        "Object Min Animation Speed",
#else
        "Sat Animation Speed",
        "Sat Max Animation Speed",
        "Sat Min Animation Speed",
#endif
        "Orbit Animation Speed",
        "Orbit Max Animation Speed",
        "Orbit Min Animation Speed",
        "Coordinate Type",
        "Background Transparency",
        "Number of Period Animated",
        "Label Sphere Radius",
        "Disk Rotation",
        "Disk Position",
        "Disk Radius",
        "Disk Height",
        "Disk Transparency",
        "Disk From File",
        "Sphere Position",
        "Sphere Radius",
        "Sphere Transparency",
        "Sphere From File",
#ifndef R3B
        "Object Radius",
#else
        "Satellite Radius",
        "Large Primary Radius",
        "Small Primary Radius",
        "Libration Point Size",
        "Coordinate system",
        "Number of Stars",
#endif
    };

    const char * hexdecimalVarNames[] =
    {
        "UNSTABLE LINE PATTERN",
        "STABLE LINE PATTERN"
    };

   const char * nDataVarNames[] =
    {
        "Background Color",
        "X Axis Color",
        "Y Axis Color",
        "Z Axis Color",
        "Surface Color",
        "Unstable Solution Color",
        "Stable Solution Color",
#ifndef R3B
        "Object Color",
#else
        "satellite Color",
        "large primary Color",
        "large primary tail Color",
        "small primary Color",
        "small primary tail Color",
#endif
    };

    const char * blWidgetName[] =
    {
        "3D",
        "3DBif",
        "3DSol",
    };

const char * graphWidgetItems[]=
{
    "Draw Reference Plane", 
    "Draw Reference Sphere", 
#ifdef R3B
    "Draw Primaries", 
    "Draw Libration Points",
#else
    "Highlight Orbit",
#endif
    "Orbit Animation", 
#ifdef R3B
    "Satellite Animation",
#endif
    "Draw Labels",
    "Show Label Numbers",
    "Draw Background", 
    "Draw Legend", 
    "Normalize Data",
};

const char * axesNames[] =
{
        "X Axis Solution",
        "Y Axis Solution",
        "Z Axis Solution",
        "X Axis Bifurcation",
        "Y Axis Bifurcation",
        "Z Axis Bifurcation",
};
