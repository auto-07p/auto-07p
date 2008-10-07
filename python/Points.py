#! /usr/bin/env python
# Point class used by parseB and parseS
# derived from PyDSTool, by:
#Author:    Robert Clewley
#Date:      14 January 2007
#$Revision: 2.0.4 $
"""Point and Pointset enhanced array classes.

(Objects of both classes are mutable.)

    Robert Clewley, February 2006
"""

import string
from types import *
import AUTOutil
numpyimported = False

def importnumpy():
    global N, numpyimported, fromstring, ndarray, array, rank, take
    global float64, int32
    fromstring = None    
    try:
        import matplotlib.numerix
        N = matplotlib.numerix
        if N.which[0] == 'numpy':
            from numpy import fromstring,ndarray,float64,int32,rank,array,take
    except ImportError:
        try:
            import numpy
            N = numpy
            N.nonzero = N.flatnonzero
            from numpy import fromstring,ndarray,float64,int32,rank,array,take
        except ImportError:
            try:
                import Numeric
                N = Numeric
            except ImportError:
                import array
                N = array
                def array(l,code='d'):
                    return N.origarray(code,l)
                def rank(a):
                    return 1
                def take(a,idx):
                    b=[]
                    for i in idx:
                        b.append(a[i])
                    return array(b)
                N.origarray = N.array
                float64 = 'd'
                int32 = 'i'
    if not fromstring:
        if hasattr(N,'transpose'):
            # Numeric or matplotlib+Numeric (not numpy)
            float64 = N.Float64
            int32 = N.Int32
            rank = N.rank
            array = N.array
            take = N.take
        # array or Numeric
        ndarray = N.ArrayType
    numpyimported = True

from copy import copy

# for internal use
point_keys = ['coorddict', 'coordarray', 'coordtype', 'norm', 'labels']
_float_types = FloatType
_int_types = IntType

def uniqueList(objlist):
    """Check that list contains items only once"""
    for obj in objlist:
        if objlist.count(obj) != 1:
            return False
    return True

def compareNumTypes(t1, t2):
    _num_type2name = {float64: 'float', int32: 'int',
                      FloatType: 'float', IntType: 'int'}
    try:
        for t in t2:
            if _num_type2name[t1] == _num_type2name[t]:
                return True
        return False
    except TypeError:
        # t2 not iterable, assume singleton
        try:
            return _num_type2name[t1] == _num_type2name[t2]
        except KeyError:
            return False
    except KeyError:
        return False

# find intersection of two lists, sequences, etc.
def intersect(a, b):
    return filter(lambda e,b=b : e in b, a)

class Point:
    """N-dimensional point class."""

    # Contains an ordered list of names for the coordinates (to
    # suggest how the points belong to a particular space specified
    # using a particular basis)
    def __init__(self, kwd=None, **kw):
        if not numpyimported:
            importnumpy()
        _num_equivtype = {FloatType: float64, IntType: int32,
                          float64: float64, int32: int32,
                          float: float64, int: int32}
        if kwd is not None:
            if kw != {}:
                raise ValueError("Cannot mix keyword dictionary and keywords")
            kw = kwd
        self._parameterized = False
        self.labels = {}
        if intersect(kw.keys(), point_keys) == []:
            # creating Point from dictionary
            temp_kw = {}
            temp_kw['coorddict'] = copy(kw)
            kw = copy(temp_kw)
        if 'coorddict' in kw.keys():
            coorddict = {}
            try:
                ct = kw['coordtype']
            except KeyError:
                self.coordtype = float64
            else:
                try:
                    self.coordtype = _num_equivtype[ct]
                except KeyError:
                    raise TypeError('Coordinate type %s not valid for Point'%str(ct))
            for c, v in kw['coorddict'].items():
                if not isinstance(c, StringType):
                    c_key = repr(c)
                else:
                    c_key = c
                if isinstance(v, ListType):
                    coorddict[c_key] = array(v, self.coordtype)
                elif isinstance(v, ndarray):
                    if len(v) == 1:
                        coorddict[c_key] = v[0]
                    else:
                        coorddict[c_key] = array(v)
                    assert self.coordtype == coorddict[c_key].dtype.type, 'type mismatch'
                elif isinstance(v, _float_types):
                    assert compareNumTypes(self.coordtype, float64), 'type mismatch'
                    coorddict[c_key] = array([v], self.coordtype)
                elif isinstance(v, _int_types):
                    assert compareNumTypes(self.coordtype, (int32, float64)), 'type mismatch'
                    coorddict[c_key] = array([v], self.coordtype)
##                elif isinstance(v, _complex_types):
##                    assert compareNumTypes(self.coordtype, complex64), 'type mismatch'
##                    coorddict[c_key] = array([v], self.coordtype)
                else:
                    raise TypeError("Must pass numeric type or sequence of numeric types")
            self.coordnames = coorddict.keys()
            # only way to order dictionary keys for array is to sort
            self.coordnames.sort()
            self.dimension = len(self.coordnames)
            datalist = []
            for c in self.coordnames:
                assert not type(coorddict[c]) in [ListType,TupleType], 'type mismatch'
                datalist.append(coorddict[c][0])
            self.coordarray = array(datalist, self.coordtype)
            r = rank(self.coordarray)
            if r == 1:
                pass
            elif r == 0:
                self.coordarray = self.coordarray.ravel()
            else:
                raise ValueError("Invalid rank for coordinate array: %i"%r)
            assert self.dimension == len(self.coordarray), "Invalid coord array"
        elif 'coordarray' in kw.keys():
            # 'coordtype' key is optional unless 'array' is actually a list,
            # when this key specifies the internal Python to use
            if isinstance(kw['coordarray'], ndarray):
                # use 'array' constructor to ensure that copy is made of array
                # in case either original or new array is independently changed.
                try:
                    array_temp = array(kw['coordarray'])
                except TypeError:
                    array_temp = array(kw['coordarray'].tolist(),
                                       kw['coordarray'].typecode)
                try:
                    self.coordtype = _num_equivtype[type(array_temp[0])]
                except KeyError:
                    raise TypeError('Coordinate type %s not valid for Point'%str(ct))
            elif isinstance(kw['coordarray'], ListType):
                try:
                    self.coordtype = _num_equivtype[kw['coordtype']]
                except KeyError:
                    raise TypeError('Coordinate type %s not valid for Point'%str(ct))
                array_temp = array(kw['coordarray'], self.coordtype)
            else:
                raise TypeError('Coordinate type %s not valid for Point'%str(type(kw['coordarray'])))
            r = rank(array_temp)
            if r == 1:
                self.coordarray = array_temp
            elif r == 0:
                self.coordarray = array_temp.ravel()
            else:
                raise ValueError("Invalid rank for coordinate array: %i"%r)
            self.dimension = len(self.coordarray)
            if 'coordnames' in kw.keys():
                if isinstance(kw['coordnames'], StringType):
                    coordnames = [kw['coordnames']]
                else:
                    coordnames = kw['coordnames']
            else:
                coordnames = map(str, range(self.dimension))
            if len(coordnames) != self.dimension:
                print "Point initialization error:"
                print "Found coord names: ", coordnames, \
                             "(dimension = %s)"%len(coordnames)
                print "vs. data dimension =", self.dimension
                raise ValueError("Mismatch between number of coordnames and "
                                 "dimension of data")
            self.coordnames = kw['coordnames']
        else:
            raise ValueError("Missing coord info in keywords")
        assert uniqueList(self.coordnames), 'Coordinate names must be unique'
        self.makeIxMaps()
        if 'norm' in kw.keys():
            if kw['norm'] == 0:
                raise ValueError("Norm order for point cannot be zero")
            self._normord = kw['norm']
        else:
            self._normord = 2
        # extra information (for special bifurcation point data)
        if 'labels' in kw.keys():
            self.addlabel(kw['labels'])


    def addlabel(self, label):
        if label is None:
            pass
        elif isinstance(label, StringType):
            self.labels = {label: {}}
        elif isinstance(label, TupleType) and len(label)==2:
            if (isinstance(label[0], StringType) and
                isinstance(label[1], DictType)):
                self.labels[label[0]] = label[1]
        elif isinstance(label, DictType):
            self.labels = label
        else:
            raise TypeError("Point label must be a string, a pair, or a dict")


    def removelabel(self):
        self.labels = {}


    def makeIxMaps(self):
        self._name_ix_map = dict(zip(self.coordnames, range(self.dimension)))
        self._ix_name_map = copy(self.coordnames)


    def todict(self, aslist=False):
        """Convert Point to a dictionary of array values (or of list with aslist=True)."""
        if aslist:
            return dict(zip(self._ix_name_map, self.coordarray.tolist()))
        else:
            return dict(zip(self._ix_name_map, self.coordarray))

    def __delitem__(self, k):
        raise NotImplementedError

    def get(self, coord, d=None):
        if coord in self._ix_name_map:
            return self.__call__(coord)
        else:
            return d

    def update(self, d):
        for k, v in d.iteritems():
            self.coordarray[self._map_names_to_ixs(k)] = v

    def items(self):
        return zip(self._ix_name_map, self.coordarray)

    def iteritems(self):
        return iter(zip(self._ix_name_map, self.coordarray))

    def values(self):
        return self.coordarray.tolist()

    def itervalues(self):
        return iter(self.coordarray.tolist())

    def keys(self):
        return self._ix_name_map

    def iterkeys(self):
        return iter(self._ix_name_map)

    def has_key(self, k):
        return k in self._ix_name_map


    def _map_names_to_ixs(self, namelist):
        try:
            try:
                # single string
                return self._name_ix_map[namelist]
            except TypeError:
                # list of strings
                r = []
                for n in namelist:
                    r.append(self._name_ix_map[n])
                return r
        except KeyError, e:
            import AUTOExceptions
            raise AUTOExceptions.AUTORuntimeError("Name not found: "+str(e))


    def __len__(self):
        return self.dimension


    def _force_coords_to_ixlist(self, x):
        if x is None:
            return range(self.dimension)
        elif x in range(self.dimension):
            # only used for recursive calls
            return [x]
        elif x in self._ix_name_map:
            # only used for recursive calls
            return [self._name_ix_map[x]]
        elif type(x) in [TupleType, ListType, ndarray]:
            if len(x) == 0:
                return range(self.dimension)
            else:
                r = []
                for el in x:
                    r.append(self._force_coords_to_ixlist(el)[0])
                return r
        elif isinstance(x, SliceType):
            stop = x.stop or self.dimension
            s1, s2, s3 = x.start, x.stop, x.step
            if s1 < 0 or s2 > self.dimension or s1 >= self.dimension:
                raise ValueError("Slice index out of range")
            return range(s1, s2, s3)
        else:
            raise ValueError("Invalid coordinate / index: %s"%str(x) + \
                             " -- coord names are: %s"%str(self._ix_name_map))


    def __call__(self, coords):
        if coords in range(self.dimension):
            return self.coordarray[coords]
        elif coords in self._ix_name_map:
            ix = self._name_ix_map[coords]
            return self.coordarray[ix]
        else:
            ixlist = self._force_coords_to_ixlist(coords)
            coordnames = []
            for i in ixlist:
                coordnames.append(self.coordnames[i])
            return Point({'coordarray': take(self.coordarray,ixlist),
                      'coordnames': coordnames,
                      'coordtype': self.coordtype,
                      'norm': self._normord,
                      'labels': self.labels})

    __getitem__ = __call__


    def __setitem__(self, ixarg, val):
        """Change coordinate array values."""
        ixs = self._force_coords_to_ixlist(ixarg)
        if len(ixs) == 1:
            val = [val]
        try:
            for i, v in zip(ixs,val):
                self.coordarray[i] = v
        except TypeError:
            raise TypeError("Bad value type for Point")


    def toarray(self):
        if self.dimension == 1:
            return self.coordarray[0]
        else:
            return self.coordarray


    def __add__(self, other):
        res = self.copy()
        try:
            res.coordarray = res.coordarray + other.coordarray
        except AttributeError:
            res.coordarray = res.coordarray + other
        return res

    __radd__ = __add__

    def __sub__(self, other):
        res = self.copy()
        try:
            res.coordarray = res.coordarray - other.coordarray
        except AttributeError:
            res.coordarray = res.coordarray - other
        return res

    def __rsub__(self, other):
        res = self.copy()
        try:
            res.coordarray = other.coordarray - res.coordarray
        except AttributeError:
            res.coordarray = other - res.coordarray
        return res

    def __mul__(self, other):
        res = self.copy()
        try:
            res.coordarray = res.coordarray * other.coordarray
        except AttributeError:
            res.coordarray = res.coordarray * other
        return res

    __rmul__ = __mul__

    def __div__(self, other):
        res = self.copy()
        try:
            res.coordarray = res.coordarray / other.coordarray
        except AttributeError:
            res.coordarray = res.coordarray / other
        return res

    __truediv__ = __div__

    def __rdiv__(self, other):
        res = self.copy()
        try:
            res.coordarray = other.coordarray / res.coordarray
        except AttributeError:
            res.coordarray = other / res.coordarray
        return res

    __rtruediv__ = __rdiv__

    def __pow__(self, other):
        res = self.copy()
        res.coordarray = res.coordarray ** other
        return res

    def __neg__(self):
        res = self.copy()
        res.coordarray = - res.coordarray
        return res

    def __pos__(self):
        return self.copy()

    def __lt__(self, other):
        try:
            assert shape(self) == shape(other)
            if hasattr(other, 'coordnames'):
                if self.coordnames != other.coordnames:
                    raise ValueError("Coordinate mismatch")
            return abs(self) < abs(other)
        except (AttributeError, TypeError, AssertionError):
            return self.coordarray < other
        except ZeroDivisionError:
            raise ValueError("Norm order for point cannot be zero")

    def __gt__(self, other):
        try:
            assert shape(self) == shape(other)
            if hasattr(other, 'coordnames'):
                if self.coordnames != other.coordnames:
                    raise ValueError("Coordinate mismatch")
            return abs(self) > abs(other)
        except (AttributeError, TypeError, AssertionError):
            return self.coordarray > other
        except ZeroDivisionError:
            raise ValueError("Norm order for point cannot be zero")

    def __le__(self, other):
        try:
            assert shape(self) == shape(other)
            if hasattr(other, 'coordnames'):
                if self.coordnames != other.coordnames:
                    raise ValueError("Coordinate mismatch")
            return abs(self) <= abs(other)
        except (AttributeError, TypeError, AssertionError):
            return self.coordarray <= other
        except ZeroDivisionError:
            raise ValueError("Norm order for point cannot be zero")

    def __ge__(self, other):
        try:
            assert shape(self) == shape(other)
            if hasattr(other, 'coordnames'):
                if self.coordnames != other.coordnames:
                    raise ValueError("Coordinate mismatch")
            return abs(self) >= abs(other)
        except (AttributeError, TypeError, AssertionError):
            return self.coordarray >= other
        except ZeroDivisionError:
            raise ValueError("Norm order for point cannot be zero")

    def __eq__(self, other):
        try:
            assert shape(self) == shape(other)
            if hasattr(other, 'coordnames'):
                if self.coordnames != other.coordnames:
                    raise ValueError("Coordinate mismatch")
            return abs(self) == abs(other)
        except (AttributeError, TypeError, AssertionError):
            return self.coordarray == other
        except ZeroDivisionError:
            raise ValueError("Norm order for point cannot be zero")

    def __ne__(self, other):
        try:
            assert shape(self) == shape(other)
            if hasattr(other, 'coordnames'):
                if self.coordnames != other.coordnames:
                    raise ValueError("Coordinate mismatch")
            return abs(self) == abs(other)
        except (AttributeError, TypeError, AssertionError):
            return self.coordarray != other
        except ZeroDivisionError:
            raise ValueError("Norm order for point cannot be zero")


    def _infostr(self, verbose=0):
        precision = 8
        if verbose == 0:
            outputList = ["Point with coords:\n"]
            for c in self._ix_name_map:
                outputList.append(c)
                if c != self._ix_name_map[-1]:
                    outputList.append("\n")
        elif verbose > 0:
            outputList = []
            for c in self._ix_name_map:
                v = self.coordarray[self._map_names_to_ixs(c)]
                if isinstance(v, ndarray):
                    dvstr = str(v[0])
                else:
                    # only alternative is a singleton numeric value (not list)
                    dvstr = str(v)
                outputList.append(string.strip(c)+':  '+dvstr)
                if c != self._ix_name_map[-1]:
                    outputList.append("\n")
            for label, infodict in self.labels.items():
                outputList.append("\nLabels: %s (%s)"%(label, str(infodict)))
        return string.join(outputList,"")


    def __repr__(self):
        return self._infostr(verbose=1)


    __str__ = __repr__


    def info(self, verboselevel=1):
        print self._infostr(verboselevel)


    def __abs__(self):
        norm = 0.0
        for x in self.coordarray:
            norm = norm + x ** self._normord
        return norm ** (1.0/self._normord)


    def __copy__(self):
        return Point({'coordarray': copy(self.coordarray),
                      'coordnames': copy(self.coordnames),
                      'coordtype': self.coordtype,
                      'norm': self._normord,
                      'labels': self.labels})

    copy = __copy__


    def __getstate__(self):
        d = copy(self.__dict__)
        # remove reference to Cfunc type
        d['coordtype'] = _num_type2name[self.coordtype]
        return d


    def __setstate__(self, state):
        self.__dict__.update(state)
        # reinstate Cfunc type
        self.coordtype = _num_name2type[self.coordtype]


def comparePointCoords(p1, p2, fussy=False):
    """Compare two Points, Pointsets, or dictionary of point data, coordinate-wise.
    If p1 or p2 are Pointsets, their independent variable values, if present, are
    *not* compared.

    fussy option causes point norm order and coordinate types to be
    checked too (requires both arguments to be Points or Pointsets)."""
    try:
        p1d = dict(p1)
        p1dk = p1d.keys()
        p2d = dict(p2)
        p2dk = p2d.keys()
    except:
        raise TypeError("Invalid Points, Pointsets, or dictionaries passed "
                        "to comparePointCoords")
    test1 = True
    for ks in zip(p1dk, p2dk):
        if ks[0]!=ks[1]:
            test1 = False
    test2 = True
    for i in range(len(p1dk)):
        if p1d[p1dk[i]] != p2d[p2dk[i]]:
            test2 = False
    if fussy:
        try:
            test3 = p1._normord == p2._normord
            test4 = compareNumTypes(p1.coordtype, p2.coordtype)
            return test1 and test2 and test3 and test4
        except AttributeError:
            raise TypeError("Invalid Points, Pointsets, or dictionaries passed "
                            "to comparePointCoords with fussy option")
    else:
        return test1 and test2

def test_point():
    print "\n****** Point class test ******\n"
    print "x uses Python float type:"
    xstr = """x = Point({'coorddict': {'x0': [1.123456789], 'x1': [-0.4],
                   'x2': [4000]},
               'coordtype': float})"""
    print xstr
    exec(xstr)
    # float is equivalent to float64
    print "x => ", repr(x)
    print "x.toarray() = ", x.toarray()
    print "\nprint x => ", x
    print "x.dimension => ", x.dimension, ", x.coordnames => ", x.coordnames
    print "x.coordtype => ", x.coordtype
    print "x.coordtype => ", x.coordtype
    print "x('x1') = ", x('x1')
    print "x(['x1','x0']) = ", x(['x1','x0'])
    print "x([0,1]) = ", x([0,1])
    print "\nChanging x entries is done by x[index] = value:"
    print "x[1] = -0.45"
    x[1] = -0.45
    print "\nThe index can also be a name, a list of names, or even a dictionary:"
    print "x[['x0', 'x1']] = [4.11103, -0.56])"
    x[['x0', 'x1']] = [4.11103, -0.56]
    print "\ny is a 1D point (with integer type):"
    # can also specify as array([4])
    ystr = """y = Point({'y': 4})"""
    print ystr
    exec(ystr)
    print "print y => ", y
    print "y(0) = ", y(0)
    print "type(y(0)) => ", type(y(0))
    print "y([0]) = ", y([0])
    print "y.toarray() = ", y.toarray()
    if hasattr(N,'transpose'):
        assert comparePointCoords(x,(x+0)*1,fussy=True)
    # pass x back
    return x

if __name__ == '__main__':
    x = test_point()
    #wp, wnp, wpt, wp_part = test_pointset()
    print "\n"
    print "x (point) and wp, wnp (param'd and non-param'd pointsets) are available in the global namespace,", \
          "to play with interactively now that this script has run."
