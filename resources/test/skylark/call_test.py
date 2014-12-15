

def f(a,b,c=10,d=11,*e,f=12,g=13,**k):
  result = [a, b, c, d, e, f, g, k]
  print(result)
  return result

def g(x,e):
  r = eval(x)
  if not r == e:
    raise Exception("%s evaluated to %r, not %r" % (x,r,e))

def h(x,e):
  try:
    r = eval(x)
  except Exception as ex:
    s = "%s: %s" % (ex.__class__.__name__,ex)
    if s.startswith(e):
      return
    else:
      raise Exception("%s failed with %r, not %r" % (x,s,e))
  raise Exception("%s failed to fail with %r, instead returned %r" % (x,e,r))


g("f(1,2)", [1, 2, 10, 11, (), 12, 13, {}])
h("f(b=1,2,3)", "SyntaxError: non-keyword arg after keyword arg")
g("f(*[1,2,3,4,5,6])", [1, 2, 3, 4, (5, 6), 12, 13, {}])
g("f(*[1,2,3,4,5,6,7,8])", [1, 2, 3, 4, (5, 6, 7, 8), 12, 13, {}])
g("f(*[1,2,3,4,5,6,7,8], g=-1)", [1, 2, 3, 4, (5, 6, 7, 8), 12, -1, {}])
g("f(*[1,2,3,4,5,6,7,8], g=-1, h=-2, i=-3)", [1, 2, 3, 4, (5, 6, 7, 8), 12, -1, {'i': -3, 'h': -2}])
h("f(*[1,2,3,4,5,6,7,8], d=0, g=-1, h=-2, i=-3)", "TypeError: f() got multiple values for argument 'd'")
g("f(1,*[2, 3])", [1, 2, 3, 11, (), 12, 13, {}])
g("f(**{'a': 1, 'b': 2})", [1, 2, 10, 11, (), 12, 13, {}])
h("f(*[3],**{'a': 1, 'b': 2})", "TypeError: f() got multiple values for argument 'a'")
h("f(1,2,*[-3],**{'d': -4})", "TypeError: f() got multiple values for argument 'a'")
g("f(1,2,*[-3],**{'d': -4})", [1, 2, -3, -4, (), 12, 13, {}])
h("f(*[-3],a=-1, b=-2, **{'d': -4})", "TypeError: f() got multiple values for argument 'a'")
h("f(a=-1, *[-3], b=-2, **{'d': -4})", "TypeError: f() got multiple values for argument 'a'")
h("f(*[-3], b=-2, **{'d': -4})", "TypeError: f() got multiple values for argument 'a'")
g("f(*[-3], b=-2, **{'d': -4})", [-3, -2, 10, -4, (), 12, 13, {}])
