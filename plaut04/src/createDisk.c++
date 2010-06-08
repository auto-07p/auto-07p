#include "createDisk.h"

#include <Inventor/So.h>
#include <Inventor/SbLinear.h>

#ifdef USE_SOQT
#include <Inventor/Qt/editors/SoQtMaterialEditor.h>
#else
#include <Inventor/Xt/SoXtMaterialEditor.h>
#endif

#include "gplaut04.h"

extern const char *autoDir;
extern float diskTransparency;
extern float diskRotation[4];
extern float diskHeight;
extern bool diskFromFile;

SoSeparator *
createDisk(float where[], float scaler)
{
// create DISK
    SoSeparator *diskSep = new SoSeparator;
    static bool obj = diskFromFile;
    SoInput mySceneInput;
    char *diskFileName = new char[strlen(autoDir) + 30];
    strcpy(diskFileName, autoDir);
    strcat(diskFileName, "/plaut04/widgets/disk.iv");
    if (obj && mySceneInput.openFile(diskFileName))
    {
        SoSeparator *disk = SoDB::readAll(&mySceneInput);
        if (disk == NULL)
        {
            obj=FALSE;
        }
        else
        {
            mySceneInput.closeFile();
            SoTransform *diskXform = new SoTransform;
            diskXform->translation.setValue(where[0],where[1],where[2]);
            diskXform->scaleFactor.setValue(scaler, scaler, scaler);

            diskSep->addChild(diskXform);
            diskSep->addChild(disk);
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
        SoMaterial *diskMtl = new SoMaterial;
        diskMtl->diffuseColor.setValue(1.0,1.0,1.0);
        diskMtl->transparency = diskTransparency;

        SoComplexity *diskComplx = new SoComplexity;
        diskComplx->type = SoComplexity::OBJECT_SPACE;
        diskComplx->value = 1.0;
        diskComplx->textureQuality = 0.5;

        SoTransform *diskXform = new SoTransform;
        diskXform->rotation.setValue(SbVec3f(diskRotation[0], diskRotation[1],
					     diskRotation[2]), diskRotation[3]);
        diskXform->translation.setValue(where[0],where[1],where[2]);

        SoCylinder  *disk = new SoCylinder;
        disk->radius = 1.0 * scaler;
        disk->height = diskHeight;

        diskSep->addChild(diskXform);
        diskSep->addChild(diskComplx);
        diskSep->addChild(diskMtl);
        diskSep->addChild(disk);
    }

#ifdef ENABLE_MTL_EDITOR
#ifdef USE_SOQT
    SoQtMaterialEditor *mtlEditor = new SoQtMaterialEditor;
#else
    SoXtMaterialEditor *mtlEditor = new SoXtMaterialEditor;
#endif
    mtlEditor->attach(diskMtl);
    mtlEditor->show();
#endif

    return diskSep;
}
