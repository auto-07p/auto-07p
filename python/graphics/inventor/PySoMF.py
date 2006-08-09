import UserList
import types
import PySoSF
import StringIO
import copy

class_def = """class PySoMF%s(_PySoMFGenerator):
    def __init__(self,list=None):
        _PySoMFGenerator.__init__(self,PySoSF.%s,list)
"""
#The multivalue types are just generated from the single value versions.
class _PySoMFGenerator(UserList.UserList):
    def __init__(self,container_class,list=None):
        self.container_class=container_class
        UserList.UserList.__init__(self,list)

    def __setitem__(self,i,item):
        self[i] = self.container_class(item)

    def append(self,item):
        self.data.append(self.container_class(item))
        
    def setValue(self,value):
        self.data=[]
        for x in value:
            self.append(x)

    def __str__(self):
        rep=StringIO.StringIO()
        rep.write("[\n")
        for i in range(len(self.data)):
            rep.write("  "+str(self.data[i]))
            if i == len(self.data)-1:
                rep.write("\n")
            else:
                rep.write(",\n")
        rep.write("]")
        return rep.getvalue()
            
for single_value_class in PySoSF.__dict__.keys():
    if single_value_class[:6] == "PySoSF":
        exec(class_def%(single_value_class[6:],single_value_class))

if __name__ == "__main__":
    foo = PySoSF.PySoSFVec3f()
    foo.setValue([0.1,0.2,0.3])

    bar = PySoMFVec3f()
    bar.append(foo)
    bar.append(foo)
    bar.append(foo)

    print bar

    values = ["FIRST_VALUE","SECOND_VALUE","THIRD_VALUE"]
    foo = PySoSF.PySoSFEnum(values)
    foo.setValue("SECOND_VALUE")

    bar = PySoMFEnum()
    bar.append(foo)
    bar.append(foo)
    bar.append(foo)

    print bar

    
