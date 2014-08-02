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

@frills
@more(frills)
def ff():
  with mucho(gusto), efficiency*10 as (not usual):
    try:
      not be(stupid)
    finally: start()
  try: ~again()
  except Error: run()
  except Other as o: print(o)
  else: yay()
  finally: never(mind)
  del x[k]
  assert True
  assert True, Error
  raise Hell
  raise Prices from where(they,are)

glob = 0

def glab():
  global glob
  glob += 1
  return glob

def make_incer(x):
  def inc():
    nonlocal x
    x += 1
    return x
  return inc

for i in range(10,100):
  if i > 50: break
  if i < 20: continue
  print(i)
else: print("nope")

*x, *y

class Foo:
  a = 2
  class Inner(object):
    b = 3
  def foo(self):
    super(Foo, self).foo(self)
    a *= 3
    a /= 2
    a **= 4
    a |= 7
    a &= 255
    a ^= 33
    a <<= 3
    a >>= 2
    a %= 91
  b = c = a+1-2*4**3/5 | 15%7 & 255 ^ -1 << 2 >> 3
  a = b + c
  f = lambda x: x+1
