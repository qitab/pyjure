# This file should run without any error or warning with python, python3, pyjure.
import warnings

# for assertions: <Yhg1s> Fare: exception objects usually have an 'args' attribute that is their associated data. It differs per exception type.

def ee(x,e): ## eval a form, assert that it returns value
  r = eval(x)
  if not r == e:
    raise Exception("%s evaluated to %r, not %r" % (x,r,e))

def ex(x,e): ## exec a form, assert that it emits error
  try:
    exec(x)
  except Exception as ex:
    s = "%s: %s" % (ex.__class__.__name__,ex)
    if s.startswith(e):
      return
    else:
      raise Exception("%s failed with %r, not %r" % (x,s,e))
  raise Exception("%s failed to fail with %r" % (x,e))

def ew(x,w): ## exec a form, assert that it emits warning
  with warnings.catch_warnings(record=True) as wl:
    # Cause all warnings to always be triggered.
    warnings.simplefilter("always")
    # Trigger a warning.
    exec(x)
    # Verify some things
    assert len(wl) == 1
    assert ("%s: %s" % (wl[0].category.__name__, wl[0].message)) == w

def local_late_def():
  return local_late()
  def local_late(): # Defined too late!
    return "inner h"

ex("local_late_def()", "UnboundLocalError: local variable 'local_late' referenced before assignment")
ex("global_late()", "NameError: name 'global_late' is not defined")

def global_late():
  return "found"

x = global_late();
assert x=="found"

def f(x):
  def g(x):
    def f(x):
      if x:
        return g(x-1)
      else:
        return "inner f"
      def g(x):
        return "inner inner g"
    if x:
      return f(x-1)
    else:
      return "inner g"
  if x:
    return g(x-1)
  else:
    return "outer f"

assert f(0)=="outer f"
assert f(1)=="inner g"
assert f(2)=="inner f"
ex("f(3)", "UnboundLocalError: local variable 'g' referenced before assignment")

XX = "global"

ew("""
def gw1():
  XX = 'local'
  def w1():
    XX = 'g2'
    a = XX
    global XX
    b = XX
    return (a, b)
  return (XX, w1())
assert gw1() == ('local', ('g2', 'g2'))
assert XX == 'g2'
""", "SyntaxWarning: name 'XX' is assigned to before global declaration")



XX = "global"
def g():
  XX = "local"
  def h():
    return XX
  def i():
    global XX
    return XX
  def l():
    nonlocal XX
    return XX
  def m():
    XX = "inner"
    return XX
  def n():
    return XX
  return [h(), i(), l(), m(), n()]

assert g() == ["local", "global", "local", "inner", "local"]

ex("""
def m():
  global XX
  nonlocal XX
""", "SyntaxError: name 'XX' is nonlocal and global")

i=0
assert i==0
a=[]
for i in range(2,5): a=a+[i]
assert a==[2,3,4]
assert i==4

i=0 ; count=0 ; assert i==0
a=[]
for i in range(2,5):
  i0 = i
  for i in range(6,9):
    a = a + [(i0, i)]
    count += 1
assert i==8 ; assert count==9 ; assert a == [(2,6), (2,7), (2,8), (3,6), (3,7), (3,8), (4,6), (4,7), (4,8)]

i=0 ; assert i==0
assert [i*10 for i in range(5,9)] == [50,60,70,80]
assert i==0

assert [i*10 for i in range(2,5) for i in range(5,9)] == [50, 60, 70, 80, 50, 60, 70, 80, 50, 60, 70, 80]
assert i==0

ex("""
def foo(i):
  global i
  i=1
""", "SyntaxError: name 'i' is parameter and global")

ew("""
def foo(x):
  i = 2
  if x:
    global i
  return i
assert foo(True) == 2
assert foo(False) == 2
""", "SyntaxWarning: name 'i' is assigned to before global declaration")

def foo2(x):
  if x==1:
    y="1"
  elif x==2:
    y="2"
  return y

ex("foo2(0)", "UnboundLocalError: local variable 'y' referenced before assignment")
assert foo2(1) == "1"
assert foo2(2) == "2"

ex("""
def is_global_nonlocal():
  nonlocal i
  return i""", "SyntaxError: no binding for nonlocal 'i' found")

def which_nonlocal():
  def f(): return i
  i = "defined_later"
  return f()
assert which_nonlocal() == "defined_later"

def f9(a,b=1,*d,e=2,**g): pass

r = range(0,100)[95:]
assert r == range(95, 100)

i="toplevel"
def f():
  i="f-level"
  def g():
    i="g-level"
    del i # Makes it unbound, but happily doesn't change who is bound where
    return i
  return g()
ex("f()", "UnboundLocalError: local variable 'i' referenced before assignment")

def f():
  try:
    i=1
    a=[1,2,3,4,5]
    del i, a[i]
    return a
  except UnboundLocalError as x: assert str(x) == "local variable 'i' referenced before assignment"
f()

def f():
  ex = UnboundLocalError
  try:
    ex = Exception
    assert False
  except ex as x:
    # in python, the handler sees the side effect!
    assert type(x) == AssertionError
    assert ex == Exception
f()

# left to right evaluation including function first.
def foo(): (yield (((yield 0) or (lambda x: x)) ((yield 1) or (yield 2) or 3)))
assert [i for i in foo()] == [0, 1, 2, 3]

## Not on python3
#import sys, StringIO
#class Capturing(list):
#    def __enter__(self):
#        self._stdout = sys.stdout
#        sys.stdout = self._stringio = StringIO.StringIO()
#        return self
#    def __exit__(self, *args):
#        self.extend(self._stringio.getvalue().splitlines())
#        sys.stdout = self._stdout

#def p(x): print(x)
#with Capturing() as output:
#  def pp(x: p(1) = p(2)) -> p(3): p(x)
#  p(0) ; pp(4)
#assert output == ["2", "1", "3", "0", "4"]


xxx = "g"
def xg(): return xxx
assert xg() == "g"

def xl(): xxx = "l" ; return xxx
assert xl() == "l"
assert xg() == "g"

#ex("""
#def ef1():
#  def x10(): return xxx
#  try: x10(); assert False
#  except UnboundLocalError as x: pass
#  xxx = "f1"
#""", "NameError: free variable 'xxx' referenced before assignment in enclosing scope")

def f1():
  assert xg() == "g"

  ## future implicit local shadows global
  def x10(): return xxx
  try: x10(); assert False
  except NameError as x: assert str(x) == "free variable 'xxx' referenced before assignment in enclosing scope"

  ## local shadows global
  xxx = "f1"
  def x1(): return xxx
  assert x1() == "f1"
  assert xg() == "g"

  ## local shadows local
  def f10(): xxx = "f10" ; return xxx
  assert f10() == "f10"
  assert x1() == "f1"
  assert xg() == "g"

  ## nonlocal allows modification of local
  def n1(): nonlocal xxx ; xxx = "n1" ; return xxx
  assert n1() == "n1"
  assert x1() == "n1"
  assert xg() == "g"

  ## global allows modification of global
  def g1(): global xxx ; xxx = "g1" ; return xxx
  assert g1() == "g1"
  assert x1() == "n1"
  assert xg() == "g1"

  ## local is inherited
  def g2():
    assert xxx == "n1"
    def n2():
      nonlocal xxx
      assert xxx == "n1"
      xxx = "n2" ; return xxx
    assert n2() == "n2"
    assert x1() == "n2"
    assert xg() == "g1"
    return xxx
  assert g2() == "n2"
  assert x1() == "n2"
  assert xg() == "g1"
  return xxx
assert f1() == "n2"
assert xg() == "g1"

## global blocks nonlocal
ex("""def l2():
  xxx = 1
  def g2():
    global xxx
    def n2():
      nonlocal xxx
""", "SyntaxError: no binding for nonlocal 'xxx' found")
