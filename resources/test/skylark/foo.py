class Foo:
  a, b, *c, d, e, f = 1, 2, 3, 4, 5, 6, 7
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
