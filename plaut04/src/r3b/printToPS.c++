#include <Inventor/SbViewportRegion.h>
#include <Inventor/SoOffscreenRenderer.h>
#ifdef USE_SOQT
#include <Inventor/Qt/viewers/SoQtExaminerViewer.h>
#else
#include <Inventor/Xt/viewers/SoXtExaminerViewer.h>
#endif

SbBool
printToPostScript (SoNode *root, FILE *file,
#ifdef USE_SOQT
SoQtExaminerViewer *viewer, int printerDPI)
#else
SoXtExaminerViewer *viewer, int printerDPI)
#endif
{
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

    delete myRenderer;
    return TRUE;
}
