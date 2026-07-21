#!/usr/bin/env python

from auto import Points
from auto.AUTOutil import format19_10E3

class AUTOParameters(Points.Point):
    def __init__(self, kwd=None, **kw):
        if isinstance(kwd, self.__class__):
            for k,v in list(kwd.__dict__.items()):
                self.__dict__[k] = v
            return
        if kwd is None and kw == {}:
            self.coordnames = []
            self.dimension = 0
            return
        coordnames = kw.get("coordnames",[])[:]
        if "coordarray" in kw:
            coordarray = kw["coordarray"]
            if len(coordarray) < len(coordnames):
                kw["coordarray"] = (list(coordarray) +
                                    (len(coordnames)-len(coordarray)) * [0.0])
            for i in range(len(coordnames),len(coordarray)):
                coordnames.append("PAR(%d)"%(i+1))
            kw["coordtype"] = Points.float64
            kw["coordnames"] = coordnames
        Points.Point.__init__(self,kwd,**kw)

    def __call__(self,index):
        return self.coordarray[index-1]

    def __str__(self):
        rep = []
        for i in range(1,len(self)+1,5):
            j = min(i+4,len(self))
            line = "PAR(%-7s "%("%d:%d):"%(i,j))
            for k in range(i,j+1):
                if self.coordnames[k-1] != "PAR(%d)"%k:
                    for k in range(i,j+1):
                        line += "    %-15s"%self.coordnames[k-1]
                    rep.append(line)
                    line = 12*" "
                    break
            line += "".join([format19_10E3(self(k)) for k in range(i,j+1)])
            rep.append(line)
        return "\n".join(rep)

def AUTOatof(input_string):
    #Sometimes AUTO messes up the output.  I.e. it gives an
    #invalid floating point number of the form x.xxxxxxxE
    #instead of x.xxxxxxxE+xx.  Here we assume the exponent
    #is 0 and make it into a real real number :-)
    try:
        return float(input_string)
    except ValueError:
        try:
            if not isinstance(input_string, str):
                input_string = input_string.decode('ascii')
            if input_string[-1] == "E":
                #  This is the case where you have 0.0000000E
                return float(input_string.strip()[0:-1])
            if len(input_string) >= 5:
                if input_string[-4] in ["-","+"]:
                    #  This is the case where you have x.xxxxxxxxx-yyy
                    #  or x.xxxxxxxxx+yyy (standard Fortran but not C)
                    return float(input_string[:-4]+'E'+input_string[-4:])
                if input_string[-4] == "D":
                    #  This is the case where you have x.xxxxxxxxxD+yy
                    #  or x.xxxxxxxxxD-yy (standard Fortran but not C)
                    return float(input_string[:-4]+'E'+input_string[-3:])
            input_string = input_string.replace("D","E")
            input_string = input_string.replace("d","e")
            try:
                return float(input_string)
            except ValueError:
                i = input_string.find("-", 1)
                if i == -1:
                    i = input_string.find("+", 1)
                if i != -1 and input_string[i-1] not in ["e", "E"]:
                    # form x.xxx+yy or x.xxx-yy (standard Fortran but not C)
                    return float(input_string[:i]+'E'+input_string[i:])
            print("Encountered value I don't understand")
            print(input_string)
            print("Setting to 0")
            return 0.0
        except ValueError:
            print("Encountered value which raises an exception while processing!!!")
            print(input_string)
            print("Setting to 0")
            return 0.0

