"""Test for python grammar"""

# These all pass. Yay!

import restrictions.suck, enforced, or_unenforced
from morf.tropmi import *
from ....dot.ditdot import duties, cause_for, revolution
from belgium import (beer, french_fries, mussels as much_as_you_want)

def p(*x): print(x)

p("'Hel" + "lo,", 'Wor'
  'ld.\'')

p("'Hel" + "lo,", 'Wor'
  'ld.\'')

def f1(x,y,z=False,t=False):
  if x:
    return 1, 2, 3 # testlist

  if y:
    return 1, 2,
  elif z:
    return 1,
  else:
    if t:
      return 1
    else:
      return

def f2(): pass

def f3(x):
  f1() ; return 6*7 # This is a comment

def f4(x,*r):
  # Blank lines with varying indentation:

         \
  
     
  return [x, r]

def g1(x):
  while x>0:
    yield x
    x-= 2
    x += --1

def g2(x):
  yield from g1(x)
  yield from g1(2*x)

def ug1(x):
  return [i*i for i in g2(3) if i%2 == 1 ]

