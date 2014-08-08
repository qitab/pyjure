import ast

print(ast.dump(ast.parse("def foo(a0, a1, b1=11, a2, b2=12, *c, d1=21, d2=22, **e): pass")))
