import types

#Here are all of the single value types
class _PySoSFVecf:
    def __init__(self,length,list=None):
        self.length = length
        self.data=[]
        for i in range(self.length):
            self.data.append(0.0)
        if not list is None:
            self.setValue(list)
        
    def setValue(self,value):
        if len(value) != self.length:
            raise ValueError

        for i in range(self.length):
            if type(value[i]) != types.FloatType:
                raise ValueError

        self.data=value

    def __str__(self):
        str = ""
        for i in range(self.length):
            str = str + "%f "%self.data[i]
        return str

class PySoSFVec2f(_PySoSFVecf):
    def __init__(self,list=None):
        _PySoSFVecf.__init__(self,2,list)

class PySoSFVec3f(_PySoSFVecf):
    def __init__(self,list=None):
        _PySoSFVecf.__init__(self,3,list)

class PySoSFVec4f(_PySoSFVecf):
    def __init__(self,list=None):
        _PySoSFVecf.__init__(self,4,list)

class PySoSFEnum:
    def __init__(self,values):
        self.possible_values = values
        self.value = values[0]
    def setValue(self,value):
        if value in self.possible_values:
            self.value = value
        else:
            raise AttributeError

    def __str__(self):
        return "%s"%self.value

class PySoSFLong:
    def __init__(self,value=None):
        self.value = 0
        if not value is None:
            self.setValue(value)
    def setValue(self,value):
        if type(value) == types.IntType:
            self.value = value
        else:
            raise TypeError
    def __str__(self):
        return "%ld"%self.value
    
PySoSFInt32 = PySoSFLong
PySoSFShort = PySoSFLong

class PySoSFULong:
    def __init__(self,value=None):
        self.value = 0
        if not value is None:
            self.setValue(value)
    def setValue(self,value):
        if type(value) == types.IntType and value > 0:
            self.value = value
        else:
            raise ValueError

    def __str__(self):
        return "%d"%self.value
PySoSFUInt32 = PySoSFULong
PySoSFUShort = PySoSFULong

class PySoSFFloat:
    def __init__(self,value=None):
        self.value = 0
        if not value is None:
            self.setValue(value)
    def setValue(self,value):
        if type(value) == types.FloatType:
            self.value = value
        else:
            raise TypeError
    def __str__(self):
        return "%f"%self.value
    
class PySoSFColor:
    def __init__(self,list=None):
        # This is for an RGB triple
        self.length = 3
        self.data=[]
        for i in range(self.length):
            self.data.append(0.0)
        if not list is None:
            self.setValue(list)
        
    def setValue(self,value):
        if len(value) != self.length:
            raise ValueError

        for i in range(self.length):
            if type(value[i]) != types.FloatType or value[i] > 1.0 or value[i] < 0.0:
                raise ValueError

        self.data=value

    def __str__(self):
        str = ""
        for i in range(self.length):
            str = str + "%f "%self.data[i]
        return str

class PySoSFRotation:
    def __init__(self,list=None):
        # This is for a rotation.  The first 3 entries are a vector and the last
        # is an angle
        # NOTE:  Does the vector need to be normalized!?
        self.length = 4
        self.data=[]
        for i in range(self.length):
            self.data.append(0.0)
        if not list is None:
            self.setValue(list)
        
    def setValue(self,value):
        if len(value) != self.length:
            raise ValueError

        for i in range(self.length):
            if type(value[i]) != types.FloatType:
                raise ValueError

        self.data=value

    def __str__(self):
        str = ""
        for i in range(self.length):
            str = str + "%f "%self.data[i]
        return str


if __name__ == "__main__":
    # Things that should work
    foo = PySoSFVec2f()
    foo.setValue([0.1,0.2])
    print foo
    foo = PySoSFVec3f()
    foo.setValue([0.1,0.2,0.3])
    print foo
    foo = PySoSFVec4f()
    foo.setValue([0.1,0.2,0.3,0.4])
    print foo

    values = ["FIRST_VALUE","SECOND_VALUE","THIRD_VALUE"]
    foo = PySoSFEnum(values)
    foo.setValue("SECOND_VALUE")
    print foo

    foo = PySoSFLong()
    foo.setValue(-57)
    print foo
    foo = PySoSFInt32()
    foo.setValue(-57)
    print foo
    foo = PySoSFShort()
    foo.setValue(-57)
    print foo

    foo = PySoSFULong()
    foo.setValue(57)
    print foo
    foo = PySoSFUInt32()
    foo.setValue(57)
    print foo
    foo = PySoSFUShort()
    foo.setValue(57)
    print foo

