import StringIO
import UserList
import types
from PySoSF import *
from PySoMF import *

class PySoBase:
    def __init__(self):
        pass
    
    def internalStr(self):
        return ""

    def __setattr__(self,name,value):
        if name in self.__dict__.keys():
            # If the value is of the same class as the attribute just
            # copy it over.
            try:
                if self.__dict__[name].__class__ == value.__class__:
                    self.__dict__[name] = value
                    return
            except AttributeError:
                pass
            # Otherwise try to call the setValue routine to check the value
            self.__dict__[name].setValue(value)
        else:
            self.__dict__[name] = value

class PySoFieldContainer(PySoBase):
    def __init__(self):
        PySoBase.__init__(self)

    def internalStr(self):
        rep = StringIO.StringIO()
        rep.write(PySoBase.internalStr(self))
        return rep.getvalue()

class PySoNode(PySoFieldContainer):
    def __init__(self):
        PySoFieldContainer.__init__(self)

    def __str__(self):
        classname = self.__class__.__name__[4:]
        return classname+" {\n"+self.internalStr()+"}\n"

    def internalStr(self):
        rep = StringIO.StringIO()
        rep.write(PySoFieldContainer.internalStr(self))
        return rep.getvalue()
        

class PySoShape(PySoNode):
    def __init__(self):
        PySoNode.__init__(self)
    
    def internalStr(self):
        rep = StringIO.StringIO()
        rep.write(PySoNode.internalStr(self))
        return rep.getvalue()

class PySoShapeHints(PySoNode):
    def __init__(self):
        self.vertexOrdering = PySoSFEnum(["UNKNOWN_ORDERING",
                                          "CLOCKWISE",
                                          "COUNTERCLOCKWISE"])
        self.shapeType      = PySoSFEnum(["UNKNOWN_SHAPE_TYPE",
                                          "SOLID"])
        self.faceType       = PySoSFEnum(["UNKNOWN_FACE_TYPE",
                                          "CONVEX"])
        self.creaseAngle    = PySoSFFloat()
        PySoNode.__init__(self)
    
    def internalStr(self):
        rep = StringIO.StringIO()
        rep.write(PySoNode.internalStr(self))
        rep.write("vertexOrdering ")
        rep.write(str(self.vertexOrdering)+"\n")
        rep.write("shapeType ")
        rep.write(str(self.shapeType)+"\n")
        rep.write("faceType ")
        rep.write(str(self.faceType)+"\n")
        rep.write("creaseAngle ")
        rep.write(str(self.creaseAngle)+"\n")

        return rep.getvalue()


class PySoSphere(PySoShape):
    def __init__(self):
        self.radius = PySoSFFloat()
        PySoShape.__init__(self)
    
    def internalStr(self):
        rep = StringIO.StringIO()
        rep.write(PySoNode.internalStr(self))
        rep.write("radius ")
        rep.write(str(self.radius)+"\n")
        return rep.getvalue()

class PySoTransformation(PySoNode):
    def __init__(self):
        PySoNode.__init__(self)

    def internalStr(self):
        rep = StringIO.StringIO()
        rep.write(PySoNode.internalStr(self))
        return rep.getvalue()

class PySoTransform(PySoTransformation):
    def __init__(self):
        self.translation     =PySoSFVec3f([0.0,0.0,0.0])
        self.rotation        =PySoSFRotation([0.0,0.0,1.0,0.0])
        self.scaleFactor     =PySoSFVec3f([1.0,1.0,1.0])
        self.scaleOrientation=PySoSFRotation([0.0,0.0,1.0,0.0])
        self.center          =PySoSFVec3f([0.0,0.0,0.0])
        PySoTransformation.__init__(self)

    def internalStr(self):
        rep = StringIO.StringIO()
        # Do the stuff we inherit first
        rep.write(PySoTransformation.internalStr(self))

        rep.write("translation ")
        rep.write(str(self.translation)+"\n")
        rep.write("rotation ")
        rep.write(str(self.rotation)+"\n")
        rep.write("scaleFactor ")
        rep.write(str(self.scaleFactor)+"\n")
        rep.write("scaleOrientation ")
        rep.write(str(self.scaleOrientation)+"\n")
        rep.write("center ")
        rep.write(str(self.center)+"\n")

        return rep.getvalue()

class PySoMaterial(PySoNode):
    def __init__(self):
        self.ambientColor = PySoMFColor()
        self.diffuseColor = PySoMFColor()
        self.specularColor = PySoMFColor()
        self.emissiveColor = PySoMFColor()
        self.shininess = PySoMFFloat()
        self.transparency = PySoMFFloat()
        PySoNode.__init__(self)

    def internalStr(self):
        rep = StringIO.StringIO()
        # Do the stuff we inherit first
        rep.write(PySoNode.internalStr(self))

        rep.write("ambientColor ")
        rep.write(str(self.ambientColor)+"\n")
        rep.write("diffuseColor ")
        rep.write(str(self.diffuseColor)+"\n")
        rep.write("specularColor ")
        rep.write(str(self.specularColor)+"\n")
        rep.write("emissiveColor ")
        rep.write(str(self.emissiveColor)+"\n")

        rep.write("shininess ")
        rep.write(str(self.shininess)+"\n")
        rep.write("transparency ")
        rep.write(str(self.transparency)+"\n")

        return rep.getvalue()
    
class PySoVertexProperty(PySoNode):
    _binding = ["OVERALL",            #Whole object has same material/normal
                "PER_PART",           #One material/normal for each part of object
                "PER_PART_INDEXED",   #One material/normal for each part, indexed
                "PER_FACE",           #One material/normal for each face of object
                "PER_FACE_INDEXED",   #One material/normal for each face, indexed
                "PER_VERTEX",         #One material/normal for each vertex of object
                "PER_VERTEX_INDEXED"] #One material/normal for each vertex, indexed
    def __init__(self):
        self.vertex=PySoMFVec3f()
        self.normal=PySoMFVec3f()
        self.orderedRGBA=PySoMFUInt32()
        self.texCoord=PySoMFVec2f()
        self.normalBinding=PySoSFEnum(self._binding)
        self.materialBinding=PySoSFEnum(self._binding)
        PySoNode.__init__(self)

    def internalStr(self):
        rep = StringIO.StringIO()
        # Do the stuff we inherit first
        rep.write(PySoNode.internalStr(self))

        rep.write("vertex ")
        rep.write(str(self.vertex)+"\n")

        rep.write("normal ")
        rep.write(str(self.normal)+"\n")

        rep.write("orderedRGBA ")
        rep.write(str(self.orderedRGBA)+"\n")

        rep.write("texCoord ")
        rep.write(str(self.texCoord)+"\n")
        
        rep.write("normalBinding ")
        rep.write(str(self.normalBinding)+"\n")

        rep.write("materialBinding ")
        rep.write(str(self.materialBinding)+"\n")
        
        return rep.getvalue()

class PySoShapeHints(PySoNode):
    _vertexOrdering = ["UNKNOWN_ORDERING", 
                       "CLOCKWISE",        
                       "COUNTERCLOCKWISE"]
    _shapeType = ["UNKNOWN_SHAPE_TYPE",
                  "SOLID"]
    _faceType = ["UNKNOWN_FACE_TYPE",
                 "CONVEX"]
    def __init__(self):
        self.vertexOrdering=PySoSFEnum(self._vertexOrdering)
        self.shapeType=PySoSFEnum(self._shapeType)
        self.faceType=PySoSFEnum(self._faceType)
        self.creaseAngle=PySoSFFloat() 
        PySoNode.__init__(self)

    def internalStr(self):
        rep = StringIO.StringIO()
        # Do the stuff we inherit first
        rep.write(PySoNode.internalStr(self))

        rep.write("vertexOrdering ")
        rep.write(str(self.vertexOrdering)+"\n")
        rep.write("shapeType ")
        rep.write(str(self.shapeType)+"\n")
        rep.write("faceType ")
        rep.write(str(self.faceType)+"\n")
        rep.write("creaseAngle ")
        rep.write(str(self.creaseAngle)+"\n")
        
        return rep.getvalue()
        
class PySoMaterialBinding(PySoNode):
    _binding = ["OVERALL",            #Whole object has same material/normal
                "PER_PART",           #One material/normal for each part of object
                "PER_PART_INDEXED",   #One material/normal for each part, indexed
                "PER_FACE",           #One material/normal for each face of object
                "PER_FACE_INDEXED",   #One material/normal for each face, indexed
                "PER_VERTEX",         #One material/normal for each vertex of object
                "PER_VERTEX_INDEXED"] #One material/normal for each vertex, indexed
    def __init__(self):
        self.value=PySoSFEnum(self._binding)
        PySoNode.__init__(self)

    def internalStr(self):
        rep = StringIO.StringIO()
        # Do the stuff we inherit first
        rep.write(PySoNode.internalStr(self))

        rep.write("value ")
        rep.write(str(self.value)+"\n")
        
        return rep.getvalue()
        
class PySoVertexShape(PySoShape):
    def __init__(self):
        self.vertexProperty=PySoVertexProperty()
        PySoShape.__init__(self)
    def internalStr(self):
        rep = StringIO.StringIO()
        rep.write(PySoNode.internalStr(self))
        rep.write("vertexProperty ")
        rep.write(str(self.vertexProperty)+"\n")
        return rep.getvalue()

class PySoIndexedShape(PySoVertexShape):
    def __init__(self):
        self.coordIndex=PySoMFInt32()
        self.materialIndex=PySoMFInt32()
        self.normalIndex=PySoMFInt32()
        self.textureCoordIndex=PySoMFInt32()
        PySoVertexShape.__init__(self)
    def internalStr(self):
        rep = StringIO.StringIO()
        rep.write(PySoVertexShape.internalStr(self))
        rep.write("coordIndex ")
        rep.write(str(self.coordIndex)+"\n")
        rep.write("materialIndex ")
        rep.write(str(self.materialIndex)+"\n")
        rep.write("normalIndex ")
        rep.write(str(self.normalIndex)+"\n")
        rep.write("textureCoordIndex ")
        rep.write(str(self.textureCoordIndex)+"\n")
        return rep.getvalue()

class PySoIndexedLineSet(PySoIndexedShape):
    def __init__(self):
        PySoIndexedShape.__init__(self)
    def internalStr(self):
        rep = StringIO.StringIO()

        # Do the stuff we inherit first
        rep.write(PySoIndexedShape.internalStr(self))
        return rep.getvalue()
    
class PySoIndexedFaceSet(PySoIndexedShape):
    def __init__(self):
        PySoIndexedShape.__init__(self)
    def internalStr(self):
        rep = StringIO.StringIO()

        # Do the stuff we inherit first
        rep.write(PySoIndexedShape.internalStr(self))
        return rep.getvalue()
    
class PySoIndexedTriangleStripSet(PySoIndexedShape):
    def __init__(self):
        PySoIndexedShape.__init__(self)
    def internalStr(self):
        rep = StringIO.StringIO()

        # Do the stuff we inherit first
        rep.write(PySoIndexedShape.internalStr(self))
        return rep.getvalue()

class PySoNonIndexedShape(PySoVertexShape):
    def __init__(self):
        self.startIndex=PySoSFInt32()
        PySoVertexShape.__init__(self)
    def internalStr(self):
        rep = StringIO.StringIO()
        rep.write(PySoVertexShape.internalStr(self))
        rep.write("startIndex ")
        rep.write(str(self.startIndex)+"\n")
        return rep.getvalue()

class PySoPointSet(PySoNonIndexedShape):
    def __init__(self):
        self.numPoints=PySoMFInt32()
        PySoNonIndexedShape.__init__(self)
    def internalStr(self):
        rep = StringIO.StringIO()

        # Do the stuff we inherit first
        rep.write(PySoNonIndexedShape.internalStr(self))
        rep.write("numPoints ")
        rep.write(str(self.numPoints)+"\n")
        return rep.getvalue()

class PySoLineSet(PySoNonIndexedShape):
    def __init__(self):
        self.numVertices=PySoMFInt32()
        PySoNonIndexedShape.__init__(self)
    def internalStr(self):
        rep = StringIO.StringIO()

        # Do the stuff we inherit first
        rep.write(PySoNonIndexedShape.internalStr(self))
        rep.write("numVertices ")
        rep.write(str(self.numVertices)+"\n")
        return rep.getvalue()


class PySoFaceSet(PySoNonIndexedShape):
    def __init__(self):
        self.numVertices=PySoMFInt32()
        PySoNonIndexedShape.__init__(self)
    def internalStr(self):
        rep = StringIO.StringIO()

        # Do the stuff we inherit first
        rep.write(PySoNonIndexedShape.internalStr(self))
        rep.write("numVertices ")
        rep.write(str(self.numVertices)+"\n")
        return rep.getvalue()

class PySoTriangleStripSet(PySoNonIndexedShape):
    def __init__(self):
        self.numVertices=PySoMFInt32()
        PySoNonIndexedShape.__init__(self)
    def internalStr(self):
        rep = StringIO.StringIO()

        # Do the stuff we inherit first
        rep.write(PySoNonIndexedShape.internalStr(self))
        rep.write("numVertices ")
        rep.write(str(self.numVertices)+"\n")
        return rep.getvalue()


if __name__ == "__main__":
    print '#Inventor V2.1 ascii'

    vp = PySoVertexProperty()
    vp.vertex.append(PySoSFVec3f([1.0,0.0,0.0]))
    vp.vertex.append(PySoSFVec3f([0.0,1.0,0.0]))
    vp.vertex.append(PySoSFVec3f([0.0,0.0,0.0]))
    print vp

    data = PySoLineSet()
    data.vertexProperty = vp
    data.numVertices.setValue(3)
    print data

    colors = PySoMaterial()
    colors.emissiveColor.append(PySoSFColor([1.0,0.0,0.0]))
    print colors

    data = PySoFaceSet()
    data.vertexProperty = vp
    data.numVertices.setValue(3)
    print data










