def syracusan_sequence(n):
  def f(x):
    print(x)
    if x == 1:
      return
    else:
      return g(x)

  def g(x):
    if (x % 2) == 0:
      return f(x >> 1)
    else:
      return f(3*x+1)

  f(n)

syracusan_sequence(39571320957)
