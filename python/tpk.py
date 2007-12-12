#!/usr/bin/python
from math import sqrt

def f(x):
    return sqrt(abs(x)) + 5.0*(x**3)

a = [input() for i in xrange(11)]
a.reverse()
for e, res in [(e, f(e)) for e in a]:
    if res > 400:
        print e, "TOO LARGE"
    else:
        print e, res