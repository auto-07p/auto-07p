#! /usr/bin/env python
try:
    from ConfigParser import ConfigParser
except ImportError: # Python 3
    from configparser import ConfigParser
import os
import array
import gzip
import sys
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
        return p.returncode, output[0]
    except ImportError:
        import commands
        return commands.getstatusoutput(" ".join(cmd))

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
       
# very basic numpy emulation:
class array(object):
    def __init__(self, l, code=None):
        if code is None:
            if isinstance(l, array):
                code = l.typecode
            else:
                code = 'd'
        if isinstance(l, array):
            l = l.tolist()
        if (isinstance(l, list) and len(l) > 0 and
            isinstance(l[0], (array, list))):
            self.data = [array(a1, code) for a1 in l]
            self.shape = len(l), len(l[0])
        else:
            if not isinstance(l, list):
                try:
                    l=list(l)
                except TypeError:
                    l=[l]
            self.data = N.array(code, l) 
            self.shape = len(l),
        self.typecode = code

    def __eq__(self, other):
        for a, b in zip(self, other):
            if a != b:
                return False
        return True

    def __setitem__(self, i, v):
        if isinstance(i, slice) and isinstance(v, array):
            self.data[i] = v.data
        else:
            self.data[i] = v

    def __getitem__(self, i):
        return self.data[i]

    def __len__(self):
        return self.shape[0]

    def __str__(self):
        if len(self.shape) == 1:
            return str(self.data)
        return str([e.data for e in self.data])

    def tolist(self):
        if len(self.shape) == 1:
            return self.data.tolist()
        return [e.tolist() for e in self.data]

def rank(a):
    return len(a.shape)

def take(a, idx, axis=0):
    if axis == 1:
        return [take(j, idx) for j in a]
    b=[]
    try:
        b = [a[i] for i in idx]
    except TypeError:
        raise IndexError
    return array(b)

def array2string(a,precision=0):
    return '[ '+"  ".join(map(str, a))+']'

def shape(a):
    return a.shape
        
def zeros(dim,code):
    if len(dim) == 1:
        return array(dim[0]*[0.0],code)
    return array([array(dim[1]*[0.0],code) for i in range(dim[0])],code)

def less(a, val):
    return [v < val for v in a]

def ravel(a):
    return N.array('d',a)

ArrayType = array

def test():
    a=array([1,2,3])
    b=array(a)
    b[0] = 4
    print("%s,%s"%(a,b))

if __name__ == "__main__":
    test()
