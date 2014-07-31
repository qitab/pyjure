# This file should run without any error with python, python3, skylark.

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
except SyntaxError: assert True

