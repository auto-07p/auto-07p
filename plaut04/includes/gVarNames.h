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
        "Object Animation Speed",    // R3B: "Sat Animation Speed"
        "Object Max Animation Speed",// R3B: "Sat Max Animation Speed"
        "Object Min Animation Speed",// R3B: "Sat Min Animation Speed"
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
        "Object Radius",             // R3B: "Satellite Radius"
        "Large Primary Radius",
        "Small Primary Radius",
        "Libration Point Size",
        "Coordinate system",
        "Number of Stars",
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
        "Object Color", // "satellite Color" for R3B
        "large primary Color",
        "large primary tail Color",
        "small primary Color",
        "small primary tail Color",
    };

    const char * blWidgetName[] =
    {
        "3D",
        "3DBif",
        "3DSol",
        "Draw Scale",
        "R3B",
    };

const char * graphWidgetItems[]=
{
    "Draw Reference Plane", 
    "Draw Reference Sphere", 
    "Draw Primaries", // R3B only
    "Draw Libration Points", // R3B only
    "Highlight Orbit", // "Orbit Animation" for R3B
    "Orbit Animation", // "Satellite Animation" for R3B
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
