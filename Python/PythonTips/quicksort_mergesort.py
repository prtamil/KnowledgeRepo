In [66]: def mrg(l1,l2):
    ...:     i = j = 0
    ...:     s = []
    ...:     while i < len(l1) and j < len(l2):
    ...:         if l1[i] <= l2[j]:
    ...:             s.append(l1[i])
    ...:             i += 1
    ...:         else:
    ...:             s.append(l2[j])
    ...:             j += 1
    ...:     s.extend(l1[i:])
    ...:     s.extend(l2[j:])
    ...:     return s

In [67]: def ms(l)
  File "<ipython-input-67-ef7fb7331f10>", line 1
    def ms(l)
             ^
SyntaxError: invalid syntax


In [68]: def ms(l):
    ...:     if len(l) < 2:
    ...:         return l
    ...:     m = len(l) / 2
    ...:     f = ms(l[m:])
    ...:     l = ms(l[:m])
    ...:     return mrg(f,l)

In [69]: ms(ll1)
Out[69]: [1, 2, 3, 4, 5, 6, 7, 8, 9]

In [70]: def qs(l):
    ...:     if not l:
    ...:         return []
    ...:     else:
    ...:         piv = l[0]
    ...:         less = [x for x in l     if x < piv]
    ...:         more = [x for x in l[1:]    if x >= piv]
    ...:         return qs(less) + [piv] + qs(more)
    ...:     

In [71]: qs(ll1)
Out[71]: [1, 2, 3, 4, 5, 6, 7, 8, 9]

In [72]: 