"""This is a test of Python. L'as-tu lu?
"""

class Env(Object):
  def __init_arg__(**k): return dict(**k)

class FExpr(Object):
  def __init_arg__(**k): return dict(**k)

class Env(Object):
  def __init_arg__(**k): return dict(**k)

def eval(x, E):
  # integer variables are self-evaluating
  if isinstance(x, int):
    return x

  # lists are FEXPR + arguments
  elif isinstance(x, list):
    if x:
      return x
    else:
      (fun, E)=e(x[0], E)
      eval_apply(fun, x[1:], E)

  # strings are identifiers refering to the environment
  elif isinstance(x, string):
    if x in E:
      return E[x]
    else:
      raise 'Variable %s not found' % x

  else:
    raise 'Invalid form %r' % (x,)

def eval_apply(fun, args, E):
  

def test_augassign():
  x = (y,z,t) = 
