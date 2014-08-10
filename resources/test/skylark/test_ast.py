import ast

def f(x): print(x); print(ast.dump(ast.parse(x))); print()

#f("def foo(a0, a1, b1=11, a2, b2=12, *c, d1=21, d2=22, **e): pass")

f("None")
f("a[1]")
f("a[1,2]")
f("a[1:2]")
f("a[1:2:3]")
f("a[1:2:3,4:5:6]")
f("a[1:2:3,4]")
