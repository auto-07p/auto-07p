#! /usr/bin/env python
from __future__ import generators
try:
    from ConfigParser import ConfigParser
except ImportError: # Python 3
    from configparser import ConfigParser
import os
import array
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
        
try: bool, True, False ## Introduced in 2.3
except NameError:
    class bool(int):
        "Simple implementation of Booleans, as in PEP 285"
        def __init__(self, val): self.val = val
        def __int__(self): return self.val
        def __repr__(self): return ('False', 'True')[self.val]

    True, False = bool(1), bool(0)
    __builtin__.True = True
    __builtin__.False = False
    __builtin__.bool = bool

# Pre-2.3 Python has no sum() function.
try: 
    sum
except NameError:
    import operator
    def sum(seq, start=0):
        """Sum the elements of seq.
        >>> sum([1, 2, 3])
        6
        """
        return reduce(operator.add, seq, start)
    __builtin__.sum = sum


# Pre-2.3 Python has no enumerate() function.
try:
    enumerate
except NameError:
    def enumerate(sequence):
        index = 0
        for item in sequence:
            yield index, item
            index += 1
    __builtin__.enumerate = enumerate


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
    try:
        import sets ## sets module introduced in 2.3
        set, frozenset = sets.Set, sets.ImmutableSet
    except (NameError, ImportError):
        class BaseSet:
            "set type (see http://docs.python.org/lib/types-set.html)"

           
            def __init__(self, elements=[]):
                self.dict = {}
                for e in elements:
                    self.dict[e] = 1
       
            def __len__(self):
                return len(self.dict)
       
            def __iter__(self):
                for e in self.dict:
                    yield e
       
            def __contains__(self, element):
                return element in self.dict
       
            def issubset(self, other):
                for e in self.dict.keys():
                    if e not in other:
                        return False
                return True

            def issuperset(self, other):
                for e in other:
                    if e not in self:
                        return False
                return True
       

            def union(self, other):
                return type(self)(list(self) + list(other))
       
            def intersection(self, other):
                return type(self)([e for e in self.dict if e in other])

            def difference(self, other):
                return type(self)([e for e in self.dict if e not in other])

            def symmetric_difference(self, other):
                return type(self)([e for e in self.dict if e not in other] +
                                  [e for e in other if e not in self.dict])

            def copy(self):
                return type(self)(self.dict)

            def __repr__(self):
                elements = ", ".join(map(str, self.dict))
                return "%s([%s])" % (type(self).__name__, elements)

            __le__ = issubset
            __ge__ = issuperset
            __or__ = union
            __and__ = intersection
            __sub__ = difference
            __xor__ = symmetric_difference

        class frozenset(BaseSet):
            "A frozenset is a BaseSet that has a hash value and is immutable."

            def __init__(self, elements=[]):
                BaseSet.__init__(elements)
                self.hash = 0
                for e in self:
                    self.hash |= hash(e)

            def __hash__(self):
                return self.hash

        class set(BaseSet):  
            "A set is a BaseSet that does not have a hash, but is mutable."
       
            def update(self, other):
                for e in other:
                    self.add(e)
                return self

            def intersection_update(self, other):
                for e in self.dict.keys():
                    if e not in other:
                        self.remove(e)
                return self

            def difference_update(self, other):
                for e in self.dict.keys():
                    if e in other:
                        self.remove(e)
                return self

            def symmetric_difference_update(self, other):
                to_remove1 = [e for e in self.dict if e in other]
                to_remove2 = [e for e in other if e in self.dict]
                self.difference_update(to_remove1)
                self.difference_update(to_remove2)
                return self

            def add(self, element):
                self.dict[element] = 1
               
            def remove(self, element):
                del self.dict[element]
       
            def discard(self, element):
                if element in self.dict:
                    del self.dict[element]
                   
            def pop(self):
                key, val = self.dict.popitem()
                return key
       
            def clear(self):
                self.dict.clear()
       
            __ior__ = update
            __iand__ = intersection_update
            __isub__ = difference_update
            __ixor__ = symmetric_difference_update
       
    __builtin__.set = set
    __builtin__.frozenset = frozenset
       
def Dict(**entries):  
    """Create a dict out of the argument=value arguments.
    >>> Dict(a=1, b=2, c=3)
    {'a': 1, 'c': 3, 'b': 2}
    """
    return entries

# very basic numpy emulation:
def array(l, code=None):
    if isinstance(l, N.ArrayType):
        if code is None:
            code = l.typecode
        return N.array(code, l.tolist())
    if code is None:
        code = 'd'
    if isinstance(l, list):
        if isinstance(l[0], N.ArrayType):
            return l
        elif isinstance(l[0], list):
            a2 = []
            for a1 in l:
                a2.append(N.array(code, a1))
            return a2
        return N.array(code, l)
    try:
        l=list(l)
        return N.array(code, l) 
    except TypeError:
        return N.array(code, [l])

def rank(a):
    if isinstance(a, list):
        return 2
    return 1

def take(a, idx, axis=0):
    b=[]
    if axis == 1:
        for j in a:
            b.append(take(j, idx))
        return b
    try:
        for i in idx:
            b.append(a[i])
    except TypeError:
        raise IndexError
    return array(b)

def array2string(a,precision=0):
    return '[ '+"  ".join(map(str, a))+']'

def shape(a):
    if isinstance(a, list):
        return (len(a), len(a[0]))
    else:
        return (len(a),)
        
def zeros(dim,code):
    if len(dim) == 1:
        return N.array(code,dim[0]*[0.0])
    a = []
    for i in range(dim[0]):
        a.append(N.array(code,dim[1]*[0.0]))
    return a

def less(a, val):
    cond = []
    for v in a:
        cond.append(v < val)
    return cond

def ravel(a):
    return N.array('d',a)

ArrayType = N.ArrayType

def test():
    a=array([1,2,3])
    b=array(a)
    b[0] = 4
    print(a,b)

if __name__ == "__main__":
    test()
