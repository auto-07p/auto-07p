#include "printToPS.h"

#include <Inventor/SbViewportRegion.h>
#include <Inventor/SoOffscreenRenderer.h>

#if defined(__COIN__) && COIN_MAJOR_VERSION >= 2 && COIN_MINOR_VERSION >= 1
#include <Inventor/annex/HardCopy/SoVectorizePSAction.h>
#endif

SbBool
printToPostScript (SoNode *root, const char *filename,
#ifdef USE_SOQT
SoQtExaminerViewer *viewer, int printerDPI)
#else
SoXtExaminerViewer *viewer, int printerDPI)
#endif
{
#if defined(__COIN__) && COIN_MAJOR_VERSION >= 2 && COIN_MINOR_VERSION >= 1
    SoVectorizePSAction * ps = new SoVectorizePSAction;
    SoVectorOutput * out = ps->getOutput();

    if (!out->openFile(filename)) {
      return FALSE; // unable to open output file
    }

    // to enable gouraud shading. 0.1 is a nice epsilon value
    //ps->setGouraudThreshold(0.1f);

    // clear to white background. Not really necessary if you
    // want a white background
    ps->setBackgroundColor(TRUE, SbColor(1.0f, 1.0f, 1.0f));

    // select LANDSCAPE or PORTRAIT orientation
    //ps->setOrientation(SoVectorizeAction::LANDSCAPE);
    ps->setOrientation(SoVectorizeAction::PORTRAIT);

    // start creating a new page (A4 page, with 10mm border).
    //ps->beginPage(SbVec2f(10.0f, 10.0f), SbVec2f(190.0f, 277.0f));
    // start creating a new page (15x15 cm, no border).
    ps->beginPage(SbVec2f(0.0f, 0.0f), SbVec2f(150.0f, 150.0f));

    // There are also enums for A0-A10. Example:
    // ps->beginStandardPage(SoVectorizeAction::A4, 10.0f);

    // calibrate so that text, lines, points and images will have the
    // same size in the postscript file as on the monitor.
    ps->calibrate(viewer->getViewportRegion());

    // apply action on the viewer scenegraph. Remember to use
    // SoSceneManager's scene graph so that the camera is included.
    ps->apply(viewer->getSceneManager()->getSceneGraph());

    // this will create the postscript file
    ps->endPage();

    // close file
    out->closeFile();

    delete ps;
#else
    FILE *file = fopen(filename,"w");
    if (file == NULL)
        return FALSE;
    const SbViewportRegion &vp  = viewer->getViewportRegion();
    const SbVec2s &imagePixSize = vp.getViewportSizePixels();
    SbVec2f imageInches;
    float pixPerInch;

    pixPerInch = SoOffscreenRenderer::getScreenPixelsPerInch();
    imageInches.setValue((float)imagePixSize[0] / pixPerInch,
        (float)imagePixSize[1] / pixPerInch);

    SbVec2s postScriptRes;
    postScriptRes.setValue((short)(imageInches[0]*printerDPI),
        (short)(imageInches[1]*printerDPI));

// Create a viewport to render the scene into.
    SbViewportRegion myViewport;
    myViewport.setWindowSize(postScriptRes);
    myViewport.setPixelsPerInch((float)printerDPI);

// Render the scene
    SoOffscreenRenderer *myRenderer =
        new SoOffscreenRenderer(myViewport);
    if (!myRenderer->render(root))
    {
        delete myRenderer;
        return FALSE;
    }

// Generate PostScript and write it to the given file
    myRenderer->writeToPostScript(file);
    fclose(file);

    delete myRenderer;
#endif
    return TRUE;
}
