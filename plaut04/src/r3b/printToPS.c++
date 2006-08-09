#include <Inventor/SbViewportRegion.h>
#include <Inventor/SoOffscreenRenderer.h>
#include <Inventor/Xt/viewers/SoXtExaminerViewer.h>

SbBool
printToPostScript (SoNode *root, FILE *file,
SoXtExaminerViewer *viewer, int printerDPI)
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
