""" Abstart Syntax Tree construction """

def iter_fields(node):
    """
    Yield a tuple of ``(fieldname, value)`` for each field in ``node._fields``
    that is present on *node*.
    """
    """ Slightly modified version of the ast.iter_fields method
    found in the Python standard library.
    """
    for field in vars(node):
        try:
            yield field, getattr(node, field)
        except AttributeError:
            pass

def dump(node):
    """
    Return a formatted dump of the tree in *node*.  This is mainly useful for
    debugging purposes.  The returned string will show the names and the values
    for fields."""
    """A slightly modified version of the ast.dump function
    found in the Python standard library."""
    def _format(node):
        if isinstance(node, AST):
            fields = [(a, _format(b)) for a, b in iter_fields(node)]
            rv = '%s(%s' % (node.__class__.__name__, ', '.join(
                ('%s=%s' % field for field in fields)
            ))
            return rv + ')'
        elif isinstance(node, list):
            return '[%s]' % ', '.join(_format(x) for x in node)
        return repr(node)

    if not isinstance(node, AST):
        raise TypeError('expected AST, got %r' % node.__class__.__name__)

    return _format(node)

class AST(object):
    pass

# class Node(object):
#     def __init__(self, type, children=None, leaf=None):
#         self.type = type
# 
#         if isinstance(children, list):
#             self.children = children
#         else:
#             if children:
#                 self.children = [children]
#             else:
#                 self.children = []
#         self.leaf = leaf
# 
#     def show(self, right=0):
#         print (' ' * right) + '(%s' % self.type
# 
#         if self.leaf:
#             print (' ' * (right+4)) + self.leaf
# 
#         for child in self.children:
#             if isinstance(child, Node):
#                 child.show(right + 4)
#             else:
#                 print (' ' * (right + 4)) + child
# 
#         print (' ' * (right)) + ')'

class Module(AST):
    def __init__(self, body):
        self.body = body

class FunctionDef(AST):
    def __init__(self, name, args, body, decorator_list, returns):
        self.name = name
        self.decorator_list = decorator_list
        self.args = args
        self.returns = returns

class ClassDef(AST):
    def __init__(self,
                 name,
                 bases,
                 keywords,
                 starargs,
                 kwargs,
                 body,
                 decorator_list):
        pass

class Return(AST):
    def __init__(self, expr):
        pass

class Delete(AST):
    def __init__(self, targets):
        pass

class Assign(AST):
    def __init__(self, targets, value):
        pass

class AugAssign(AST):
    def __init__(self, target, op, value):
        pass

class For(AST):
    def __init__(self, target, iter_, body, else_):
        pass

class While(AST):
    def __init__(self, test, body, else_):
        pass

class If(AST):
    def __init__(self, test, body, else_):
        pass

class With(AST):
    def __init__(self, items, body):
        pass

class Raise(AST):
    def __init__(self, exc, cause):
        pass

class Try(AST):
    def __init__(self, body, handlers, else_, finalbody):
        pass

class Assert(AST):
    def __init__(self, test, msg):
        pass

class Import(AST):
    def __init__(self, names):
        pass

class ImportFrom(AST):
    def __init__(self, module, name, level):
        pass

class Global(AST):
    def __init__(self, names):
        pass

class Nonlocal(AST):
    def __init__(self, names):
        pass

class Expr(AST):
    def __init__(self, value):
        pass

class Pass(AST):
    def __init__(self):
        pass

class Break(AST):
    def __init__(self):
        pass

class Continue(AST):
    def __init__(self):
        pass

class BoolOp(AST):
    def __init__(self, op, values):
        pass

class BinOp(AST):
    def __init__(self, left, op, right):
        pass

class UnaryOp(AST):
    def __init__(self, op, operand):
        pass

class Lambda(AST):
    def __init__(self, args, body):
        pass

class IfExp(AST):
    def __init__(self, test, body, else_):
        pass

class Dict(AST):
    def __init__(self, keys, values):
        pass

class Set(AST):
    def __init__(self, elt):
        pass

class ListComp(AST):
    def __init__(self, elt, generators):
        pass

class SetComp(AST):
    def __init__(self, elt, generators):
        pass

class DictComp(AST):
    def __init__(self, key, value, generators):
        pass

class GeneratorExp(AST):
    def __init__(self, elt, generators):
        pass

class Yield(AST):
    def __init__(self, value):
        pass

class YieldFrom(AST):
    def __init__(self, value):
        pass

class Compare(AST):
    def __init__(self, left, ops, comparators):
        pass

class Call(AST):
    def __init__(self, func, args, keywords, starargs, kwargs):
        pass

class Num(AST):
    def __init__(self, n):
        pass

class Str(AST):
    def __init__(self, s):
        self.s = s

class Bytes(AST):
    def __init__(self, s):
        pass

class Ellipsis(AST):
    def __init__(self):
        pass

class Attribute(AST):
    def __init__(self, value, attr, ctx):
        pass

class Subscript(AST):
    def __init__(self, value, slice, ctx):
        pass

class Starred(AST):
    def __init__(self, value, ctx):
        pass

class Name(AST):
    def __init__(self, id, ctx):
        self.id = id

class List(AST):
    def __init__(self, elts, ctx):
        pass

class Tuple(AST):
    def __init__(self, elts, ctx):
        pass


def _create_node(name):
   """Creates an __init__ method with attibute 'name' properly set 
   without explicitly passing it."""
   def _make_initer(name):
       def __init__(self):
           self.name = name
       return __init__

   return type(name,
           (AST,),
           {'type': name,
               '__init__': _make_initer(name)})
 
def _node_factory(names):
    return map(_create_node, names)


expr_ctxs = ['Load', 'Store', 'Del', 'AugLoad',
            'AugStore', 'Param']
(Load, Store, Del, 
AugLoad, AugStore, Param) = _node_factory(expr_ctxs)

class Slice(AST):
    def __init__(self, lower, upper, step):
        pass

class ExtSlice(AST):
    def __init__(self, dims):
        pass

class Index(AST):
    def __init__(self, value):
        pass

boolops = ['And', 'Or']
(And, Or) = _node_factory(boolops) 

operators = ['Add', 'Sub', 'Mult', 'Div',
             'Mod', 'Pow', 'LShift', 'RShift',
             'BitOr', 'BitXor', 'BitAnd', 'FloorDiv']
(Add, Sub, Mult, Div,
Mod, Pow, LShift, RShift,
BitOr, BitXor, BitAnd, FloorDiv) = _node_factory(operators)

unaryops = ['Invert', 'Not', 'UAdd', 'USub']
(Invert, Not, UAdd, USub) = _node_factory(unaryops)

cmpops = ['Eq', 'NotEq', 'Lt', 'LtE', 'Gt',
          'GtE', 'Is', 'IsNot', 'In', 'NotIn']
(Eq, NotEq, Lt, LtE, Gt,
GtE, Is, IsNot, In, NotIn) = _node_factory(cmpops)

class Comprehension(AST):
    def __init__(self, type, name, body):
        pass

class ExceptHandler(AST):
    def __init__(self, type, name, body):
        pass

class Arguments(AST):
    def __init__(self,
                 args,
                 vararg,
                 varargannotation,
                 kwonlyargs,
                 kwarg,
                 kwargannotation,
                 defaults,
                 kw_defaults):
        self.args = args
        self.vararg = vararg
        self.varargannotation = varargannotation
        self.kwonlyargs = kwonlyargs
        self.kwarg = kwarg
        self.kwargannotation = kwargannotation
        self.defaults = defaults
        self.kw_defaults = kw_defaults

class Arg(AST):
    def __init__(self, arg, annotation=None):
        self.arg = arg
        self.annotation = annotation
        self._default_val = None

class Keyword(AST):
    def __init__(self, arg, value):
        pass

class Alias(AST):
    def __init__(self, name, asname):
        pass

class WithItem(AST):
    def __init__(self, ctx_expr, optional_vars):
        self.name = 'WithItem'
        self.ctx_expr = ctx_expr
