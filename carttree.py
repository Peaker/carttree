#
# From: http://tomerfiliba.com/blog/Cartesian-Tree-Product/
# Run with: python carttree.py
import itertools

def cartesian_tree_product(node):
    if not isinstance(node, tuple):
        yield node
        return
    
    lhs, op, rhs = node
    for l in set(cartesian_tree_product(lhs)):
        for r in set(cartesian_tree_product(rhs)):
            if op == "|":
                yield l
                yield r
            else:
                yield (l, op, r)

exp = ((("x=5", "|", "y=6"), "&", (("z=7", "|", "w=8"), "|", "q=9")),
       "&", "r=10")

for v in set(cartesian_tree_product(exp)):
    print v

# (('x=5', '&', 'z=7'), '&', 'r=10')
# (('y=6', '&', 'w=8'), '&', 'r=10')
# (('y=6', '&', 'z=7'), '&', 'r=10')
# (('x=5', '&', 'w=8'), '&', 'r=10')
# (('y=6', '&', 'q=9'), '&', 'r=10')
# (('x=5', '&', 'q=9'), '&', 'r=10')

counter = itertools.count()
def mkexp(n):
    if n == 0:
        return "x%d" % (counter.next(),)
    return (mkexp(n-1), "&" if n % 2 == 0 else "|", mkexp(n-1))

mkexp(3)
# ((('x0', '|', 'x1'), '&', ('x2', '|', 'x3')), '|', (('x4', '|', 'x5'), 
#    '&', ('x6', '|', 'x7')))
for i in range(1, 7):
    variants = set(cartesian_tree_product(mkexp(i)))
    print i, len(variants)
# 1 2
# 2 4
# 3 8
# 4 64
# 5 128
# 6 16384
