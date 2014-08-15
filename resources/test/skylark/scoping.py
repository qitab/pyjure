# This file should run without any error with python, python3, skylark.

# for assertions: <Yhg1s> Fare: exception objects usually have an 'args' attribute that is their associated data. It differs per exception type.

def local_late_def():
  return local_late()
  def local_late(): # Defined too late!
    return "inner h"

try: local_late_def(); assert False
except UnboundLocalError as x: print("local_late_def():", x)

try: print("global_late():", global_late()) ; assert False
except NameError as x: print("global_late():", x)

def global_late():
  return "found"

x = global_late();
print("after definition, global_late():", x)
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

print("f(0):", f(0)) ; assert f(0)=="outer f"
print("f(1):", f(1)) ; assert f(1)=="inner g"
print("f(2):", f(2)) ; assert f(2)=="inner f"
try:
  print("f(3):", f(3)) ; assert False
except UnboundLocalError as x: print("f(3):", x)

XX = "global"

def g():
  XX = "local"
  def h():
    return XX
  def i():
    global XX
    return XX
  def j():
    return XX
    global XX
  def k():
    a = XX
    global XX
    b = XX
    return (a,b)
  def l():
    nonlocal XX
    return XX
  def m():
    XX = "inner"
    return XX
  def n():
    return XX
  return [h(), i(), j(), k(), l(), m(), n()]

print(g())
assert g() == ["local", "global", "global", ("global", "global"), "local", "inner", "local"]

try:
  exec("""
def m():
  global XX
  nonlocal XX
""") ; assert False
except SyntaxError as x: print(x)

i=0
print("initial i:", i) ; assert i==0
for i in range(2,5):
  print(i)
print("final i after for loop in range(2,5):", i) ; assert i==4

i=0 ; count=0
print("initial i:", i) ; assert i==0
for i in range(2,5):
  for i in range(6,9):
    print(i)
    count += 1
print("final i after for loop in range(6,9) nested in loop in range(2,5):", i)
assert i==8 ; assert count==9

i=0
print("initial i:", i) ; assert i==0
print([i*10 for i in range(5,9)])
print("final i after list comprehension in range(5,9):", i) ; assert i==0

print([i*10 for i in range(2,5) for i in range(5,9)])
print("final i after list comprehension in range(5,9) nested in range(2,5):", i) ; assert i==0


try: exec("""
def foo():
  with bar() as x:
    global x
  nonlocal x
""") ; assert False
except SyntaxError as x: print(x)

try: exec("""
def foo(i):
  global i
  i=1
""") ; assert False
except SyntaxError as x: print(x)


def foo(x):
  i = 2
  if x:
    global i
  return i

print(foo(True))  # 2
print(foo(False)) # 2

def foo2(x):
  if x==1:
    y="1"
  elif x==2:
    y="2"
  return y

try:
  print(foo2(0)) ; assert False
except UnboundLocalError as x: print(x) # local variable 'y' referenced before assignment
print(foo2(1)) ; assert foo2(1) == "1"
print(foo2(2)) ; assert foo2(2) == "2"

try: exec("""
def is_global_nonlocal():
  nonlocal i
  return i""") ; assert False
except SyntaxError as x: print(x)

def which_nonlocal():
  def f(): return i
  i = "defined_later"
  return f()

print(which_nonlocal())

def f9(a,b=1,*d,e=2,**g): pass

print(range(0,100)[95:])

i="toplevel"
def f():
  i="f-level"
  def g():
    i="g-level"
    del i # Makes it unbound, but happily doesn't change who is bound where
    return i
  return g()
try: f() ; assert False
except UnboundLocalError as x: print(x)

def f():
  try:
    i=1
    a=[1,2,3,4,5]
    del i,a[i]
    return a
  except UnboundLocalError as x: print(x)
f()

def f():
  ex = UnboundLocalError
  try:
    ex = Exception
    assert False
  except ex as x: print(type(x)) # in python, the handler sees the side effect!
print(41)
f()
print(42)

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
def pp(x: p(1) = p(2)) -> p(3): p(x)
p(0) ; pp(4)
assert output == ["2", "1", "3", "0", "4"]

