#! /usr/bin/env python
try:
    from ConfigParser import ConfigParser
except ImportError: # Python 3
    from configparser import ConfigParser
import os
import array
import gzip
import sys
import AUTOExceptions
N = array

# This file contains code from the Python distribution.  As
# per the Python License we include the following:

##  -----------------------------------------------------------------------
##  Copyright 1991-1995 by Stichting Mathematisch Centrum, Amsterdam,
##  The Netherlands.

##                          All Rights Reserved

##  Permission to use, copy, modify, and distribute this software and its
##  documentation for any purpose and without fee is hereby granted,
##  provided that the above copyright notice appear in all copies and that
##  both that copyright notice and this permission notice appear in
##  supporting documentation, and that the names of Stichting Mathematisch
##  Centrum or CWI or Corporation for National Research Initiatives or
##  CNRI not be used in advertising or publicity pertaining to
##  distribution of the software without specific, written prior
##  permission.

##  While CWI is the initial source for this software, a modified version
##  is made available by the Corporation for National Research Initiatives
##  (CNRI) at the Internet address ftp://ftp.python.org.

##  STICHTING MATHEMATISCH CENTRUM AND CNRI DISCLAIM ALL WARRANTIES WITH
##  REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
##  MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL STICHTING MATHEMATISCH
##  CENTRUM OR CNRI BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
##  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
##  PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
##  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
##  PERFORMANCE OF THIS SOFTWARE.
##  -----------------------------------------------------------------------

# Portions taken from SCons and AIMA:
# 
# http://code.google.com/p/aima-python/source/browse/trunk/utils.py
#
# __COPYRIGHT__
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
# KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
# WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#

# Portions of the following are derived from the compat.py file in
# Twisted, under the following copyright:
#
# Copyright (c) 2001-2004 Twisted Matrix Laboratories

try:
    unicode
except NameError:  # 'unicode' is undefined, must be Python 3
    basestring = (str,bytes) 

def isiterable(value):
    # no strings!
    if isinstance(value, basestring): return False
    try:
        iter(value)
        return True
    except TypeError:
        return False

def findBaseClass(inputClass,baseClass):
    try:
        for base in inputClass.__bases__:
            if base == baseClass:
                return 1
            elif findBaseClass(base,baseClass) == 1:
                return 1
        return 0
        # Sometimes inputClass isn't really a class, if so we just return false
    except AttributeError:
        return 0

def getAUTORC(section=None):
    parser = ConfigParser()
    if section is not None:
        parser.add_section(section)
    path = os.path.expandvars("$AUTO_DIR/.autorc")
    if os.path.exists(path):
        parser.read(path)
    path = os.path.expandvars("$HOME/.autorc")
    if os.path.exists(path):
        parser.read(path)
    if os.path.exists("./autorc"):
        parser.read("./autorc")
    elif os.path.exists("./.autorc"):
        parser.read("./.autorc")
    return parser

def openFilename(filename,mode):
    try:
        inputfile = open(filename,mode)
    except IOError:
        s = sys.exc_info()[1]
        try:
            import gzip
            inputfile = gzip.open(filename+".gz",mode)
            inputfile.name = filename
        except IOError:
            raise IOError(s)
    return inputfile

def getstatusoutput(cmd, shell=False):
    try:
        import subprocess, os
        if not shell:
            for i in range(len(cmd)):
                cmd[i] = cmd[i].replace("'","")
        p = subprocess.Popen(cmd, shell=shell, stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT)
        output = p.communicate()
        s = output[0].decode('ascii')
        if len(s) > 0 and s[-1] == '\n':
            s = s[:-1]	
        return p.returncode, s
    except ImportError:
        import commands
        if isiterable(cmd):
            cmd = " ".join(cmd)
        return commands.getstatusoutput(cmd)

try:
    import __builtin__
except ImportError:
    import builtins as __builtin__ # Python 3

try:
    raw_input
except NameError: # Python 3
    __builtin__.raw_input = input

try:
    all
except NameError:
    # Pre-2.5 Python has no all() function.
    def all(iterable):
        """
        Returns True if all elements of the iterable are true.
        """
        for element in iterable:
             if not element:
                return False
        return True
    __builtin__.all = all
    all = all
        
try:
    any
except NameError:
    # Pre-2.5 Python has no any() function.
    def any(iterable):
        """
        Returns True if any element of the iterable is true.
        """
        for element in iterable:
            if element:
                return True
        return False
    __builtin__.any = any
    any = any
        
try: reversed ## Introduced in 2.4
except NameError:
    def reversed(seq):
        """Iterate over x in reverse order.
        >>> list(reversed([1,2,3]))
        [3, 2, 1]
        """
        if hasattr(seq, 'keys'):
            raise ValueError("mappings do not support reverse iteration")
        i = len(seq)
        while i > 0:
            i -= 1
            yield seq[i]
    __builtin__.reversed = reversed


try: sorted ## Introduced in 2.4
except NameError:
    import copy
    def sorted(seq, cmp=None, key=None, reverse=False):
        """Copy seq and sort and return it.
        >>> sorted([3, 1, 2])
        [1, 2, 3]
        """    
        seq2 = copy.copy(seq)
        if key:
            if cmp == None:
                cmp = __builtins__.cmp
            seq2.sort(lambda x,y: cmp(key(x), key(y)))
        else:
            if cmp == None:
                seq2.sort()
            else:
                seq2.sort(cmp)
        if reverse:
            seq2.reverse()
        return seq2
    __builtin__.sorted = sorted

try:
    set, frozenset ## set builtin introduced in 2.4
except NameError:
    import sets ## sets module introduced in 2.3
    set, frozenset = sets.Set, sets.ImmutableSet
    __builtin__.set = set
    __builtin__.frozenset = frozenset
       
# very basic 1D/2D numpy emulation:
class array(object):
    def __init__(self, l, code=None, copy=True):
        self.base = 0
        if code is None:
            if isinstance(l, array):
                code = l.typecode
            else:
                code = 'd'
        if isinstance(l, array):
            if not copy:
                self.base = l.base
                self.typecode = l.typecode
                self.data = l.data
                self.__shape = l.shape
                self.strides = l.strides
                return
            l = l.tolist()
        if (isinstance(l, list) and len(l) > 0 and
            isinstance(l[0], (array, list))):
            a2 = []
            for a1 in l:
                if isinstance(a1, array):
                    a1 = a1.tolist()
                a2.extend(a1)
            self.data = N.array(code, a2)
            self.__shape = len(l), len(l[0])
            self.strides = len(l[0]), 1
        else:
            if not isiterable(l):
                l=[l]
            self.data = N.array(code, l) 
            self.__shape = len(self.data),
            self.strides = 1,
        self.typecode = code

    def __copy__(self):
        return array(self)

    copy = __copy__

    def __eq__(self, other):
        return array([a == b for a, b in zip(self, other)], 'B')

    def __ne__(self, other):
        return array([a != b for a, b in zip(self, other)], 'B')

    def __gt__(self, other):
        return array([a > b for a, b in zip(self, other)], 'B')

    def __lt__(self, other):
        return array([a < b for a, b in zip(self, other)], 'B')

    def __ge__(self, other):
        return array([a >= b for a, b in zip(self, other)], 'B')

    def __le__(self, other):
        return array([a <= b for a, b in zip(self, other)], 'B')

    def __add__(self, other):
        return array([a + other for a in self], self.typecode)

    def __mul__(self, other):
        return array([a * other for a in self], self.typecode)

    def __setattr__(self, attr, value):
        if attr == 'shape':
            length = 1
            for idx in self.__shape:
                length *= idx
            j = None
            for i, idx in enumerate(value):
                if idx == -1:
                    j = i
                elif idx != 0:
                    length //= idx
            if j is not None:
                value = value[:j] + (length,) + value[j+1:]
            self.__shape = value
            if len(value) == 2:
                self.strides = value[1], 1
            else:
                self.strides = 1,
        else:
            object.__setattr__(self, attr, value)

    def __getattr__(self, attr):
        if attr == 'shape':
            return self.__shape
        return object.__getattribute__(self, attr)

    def __subarray(self, i):
        if not isinstance(i, tuple):
            i = i,
        if len(i) < len(self.__shape):
            i = i + (slice(None),)
        base = self.base
        strides = []
        shape = []
        for j, idx in enumerate(i):
            if isinstance(idx, slice):
                idx, stop, step = idx.indices(self.__shape[j])
                strides.append(step*self.strides[j])
                shape.append(abs(stop-idx+step-1)//abs(step))
            else:
                if idx < 0:
                    idx += self.__shape[j]
                if idx < 0 or idx >= self.__shape[j]:
                    raise IndexError
            base += idx*self.strides[j]
        return base, strides, shape

    def __setitem__(self, i, v):
        base, strides, shape = self.__subarray(i)
        if len(strides) == 0:
            self.data[base] = v
        elif len(strides) == 1:
            stop = base+strides[0]*shape[0]
            if stop < 0:
                stop = None
            if isinstance(v,array):
                self.data[base:stop:strides[0]] = v._arrayslice()
            else:
                self.data[base:stop:strides[0]] = N.array(self.typecode, v)
        else:
            raise AUTOExceptions.AUTORuntimeError("2D updates only "
                                                  "supported with numpy.")

    def __getitem__(self, i):
        base, strides, shape = self.__subarray(i)
        if len(strides) == 0:
            return self.data[base]
        new = array(self, copy=False)
        new.base = base
        new.shape = tuple(shape)
        new.strides = tuple(strides)
        return new

    def __len__(self):
        return self.shape[0]

    def _arrayslice(self):
        stop = self.base+self.shape[0]*self.strides[0]
        if stop < 0:
            stop = None
        return self.data[self.base:stop:self.strides[0]]

    def __str__(self):
        return array2string(self)

    def tolist(self):
        if len(self.shape) == 1:
            return self._arrayslice().tolist()
        return [e.tolist() for e in self]

def rank(a):
    return len(a.shape)

def reshape(a,shape):
    new = array(a, copy=False)
    if isinstance(shape,int):
        shape = shape,
    new.shape = shape
    return new

def transpose(a):
    new = array(a, copy=False)
    new.shape = a.shape[::-1]
    new.strides = a.strides[::-1]
    return new

def take(a, idx, axis=0):
    try:
        if axis == 1:
            return array([[j[i] for i in idx] for j in a])
        return array([a[i] for i in idx])
    except TypeError:
        raise IndexError

def array2string(a,precision=0,format=None):
    indent = 1
    if a.typecode == 'd' and format is None:
        indent = 0
        absa = [abs(e) for e in ravel(a) if e != 0]
        if absa != []:
            minval = min(absa)
            maxval = max(absa)
        else:
            maxval = minval = 1
        if minval < 0.0001 or maxval > minval * 1000:
            maxlen = 16 + (0 < minval < 1e-99 or maxval >= 1e100)
            format = "%%%s.8e"%maxlen
        else:
            precision = 0
            for e in absa:
                s = "%.8f"%e
                precision = max(precision, 8 - (len(s) - len(s.rstrip("0"))))
            maxlen = len(str(int(maxval))) + precision + 2
            format = "%%#%s.%sf"%(maxlen, precision)
    if len(a.shape) == 2:
        return "["+"\n ".join([array2string(e,format=format)
                               for e in a])+"]"
    if a.typecode == 'B':
        return '[ '+"  ".join([str(e == 1) for e in a])+']'
    if a.typecode == 'd':
        ss = [format%e for e in a]
        if format[-1] == 'f':
            for i, s in enumerate(ss):
                t = s.rstrip("0")
                ss[i] = t + (len(s)-len(t))*" "
        if len(ss) > 0:
            itemsperline = 77//(1+len(ss[0]))
            for i in range(itemsperline-1, len(ss)-1, itemsperline):
                ss[i] = ss[i]+"\n"+" "*indent
        return '['+" ".join(ss)+']'
    return '[ '+"  ".join(map(str, a))+']'

def shape(a):
    try:
        return a.shape
    except AttributeError:
        return array(a).shape
        
def zeros(dim,code):
    if len(dim) == 1:
        return array(dim[0]*[0.0],code)
    return array([array(dim[1]*[0.0],code) for i in range(dim[0])],code)

def less(a, val):
    return array([v < val for v in a], a.typecode)

def ravel(a):
    if len(a.shape) == 1:
        return a
    new = array([])
    new.data = a.data
    new.shape = a.shape[0]*a.shape[1],
    new.strides = a.strides[1],
    new.base = a.base
    return new

ArrayType = array

def test():
    a=array([1,2,3,4])
    print("%s"%a[::-1])
    a=array([[1,2],[3,4]])
    print("%s %s"%(a[:,1],a[1,:]))
    print("%s"%a[:,::-1])
    print("%s"%a[::-1,:])
    print("%s"%a[::-1])
    print("%s"%a[-1,-1])
    print("%s"%a[::-1,::-1])
    b=array(a)
    b[0][0] = 4
    print("%s,%s"%(a,b))
    b[0,:]=[7,8]
    print("%s"%(b))

if __name__ == "__main__":
    test()
