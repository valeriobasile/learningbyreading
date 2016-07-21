
import re
CAT_RE = re.compile(r'([\\/()])(?:\{([A-Z_]\*?)\})?|(?:([A-Z]+|[.,;:]|conj)(?:\[([Xa-z]+)\])?(?:\{([A-Z_]\*?)\})?(?:<([0-9])>)?)')
del re

class Cat(object):
  __slots__ = ('slash', 'colour')

  @property
  def bwd(self):
    return self.slash == '\\'

  @property
  def fwd(self):
    return self.slash == '/'

  @classmethod
  def parse(cls, str):
    return cls._parse(CAT_RE.findall(str))

  @classmethod
  def _parse(cls, tokens):
    nest, var1, atom, feat, var2, slot = tokens.pop(0)
    if atom:
      res = Atom(atom, feat or None, var2 or None, slot and int(slot) or None)
    elif nest == '(':
      res = cls._parse(tokens)
      nest, var1, atom, feat, var2, slot = tokens.pop(0)
      assert nest == ')'
      res.var = var1 or None
    if not tokens or tokens[0][0] in '()':
      return res
    nest, var1, atom, feat, var2, slot = tokens.pop(0)
    return Complex(res, nest, Cat._parse(tokens), var1 or None)

  def __str__(self):
    return self._str(False)

  def __repr__(self):
    return self._repr()

class Atom(Cat):
  __slots__ = ('atom', 'feat', 'var', 'slot')

  def __init__(self, atom, feat=None, var=None, slot=None):
    self.slash = None
    self.atom, self.feat, self.var, self.slot = atom, feat, var, slot

  def _str(self, bracket):
    return '%s%s' % (self.atom, self.feat and ('[%s]' % self.feat) or '')

  def _repr(self):
    return ''.join((self.atom, self.feat and ('[%s]' % self.feat) or '',
                    self.var and ('{%s}' % self.var) or '',
                    self.slot and ('<%d>' % self.slot) or ''))

  def unify(self, other):
    return self.atom == other.atom and \
        (self.feat in (None, 'X') or other.feat in (None, 'X') or \
           self.feat == other.feat)

class Complex(Cat):
  __slots__ = ('res', 'arg', 'var')

  def __init__(self, res, slash, arg, var=None):
    self.res, self.slash, self.arg, self.var = res, slash, arg, var

  def _str(self, bracket):
    res = '%s%s%s' % (self.res._str(True), self.slash, self.arg._str(True))
    if bracket:
      res = '(%s)' % res
    return res

  def _repr(self):
    return '(%s%s%s)%s' % (self.res._repr(), self.slash, self.arg._repr(),
                           self.var and ('{%s}' % self.var) or '')

  def unify(self, other):
    return self.slash == other.slash and \
        self.res.unify(other.res) and self.arg.unify(other.arg)

EG1 = r'((S[X]{Y}\NP{Z}){Y}/(S[X]{Y}<1>\NP{Z}){Y}){_}'
EG2 = r'((S[dcl]{Y}\NP{Z}){Y}/(S[b]{Y}<1>\NP{Z}){Y}){_}'

