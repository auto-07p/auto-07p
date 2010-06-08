#include "createSphere.h"

#include <Inventor/So.h>
#include <Inventor/SbLinear.h>
#include "gplaut04.h"

extern const char *autoDir;
extern float sphereTransparency;
extern bool sphereFromFile;

SoSeparator *
createSphere(float where[], float scaler)
{
// create SPHERE
    SoSeparator *sphereSep = new SoSeparator;
    static bool obj = sphereFromFile;
    SoInput mySceneInput;

    char *diskFileName = new char[strlen(autoDir) + 32];
    strcpy(diskFileName, autoDir);
    strcat(diskFileName, "/plaut04/widgets/sphere.iv");
    if (obj && mySceneInput.openFile(diskFileName))
    {
        SoSeparator *sphere = SoDB::readAll(&mySceneInput);
        if (sphere == NULL)
        {
            obj=FALSE;
        }
        else
        {
            mySceneInput.closeFile();
            SoTransform *sphereXform = new SoTransform;
            sphereXform->translation.setValue(where[0],where[1],where[2]);
            sphereXform->scaleFactor.setValue(scaler, scaler, scaler);

            sphereSep->addChild(sphereXform);
            sphereSep->addChild(sphere);
            obj = TRUE;
        }
    }
    else
    {
        obj = FALSE;
    }
    delete [] diskFileName;

    if(!obj)
    {
        SoMaterial *sphereMtl = new SoMaterial;
        sphereMtl->diffuseColor.setValue(1.0,1.0,1.0);
        sphereMtl->transparency = sphereTransparency;

        SoComplexity *sphereComplx = new SoComplexity;
        sphereComplx->type = SoComplexity::OBJECT_SPACE;
        sphereComplx->value = 1.0;
        sphereComplx->textureQuality = 0.5;

        SoTransform *sphereXform = new SoTransform;
        sphereXform->translation.setValue(where[0],where[1],where[2]);

        SoSphere *sphere = new SoSphere;
        sphere->radius = 1.0 * scaler;

        sphereSep->addChild(sphereXform);
        sphereSep->addChild(sphereComplx);
        sphereSep->addChild(sphereMtl);
        sphereSep->addChild(sphere);
    }

#ifdef ENABLE_MTL_EDITOR
#ifdef USE_SOQT
    SoQtMaterialEditor *mtlEditor = new SoQtMaterialEditor;
#else
    SoXtMaterialEditor *mtlEditor = new SoXtMaterialEditor;
#endif
    mtlEditor->attach(sphereMtl);
    mtlEditor->show();
#endif

    return sphereSep;
}
