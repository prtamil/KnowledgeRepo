""" Matrix Transpose Logic
"""
res = []
for (i,x) in enumerate(mat):
    v = []
    for (j,y) in enumerate(x):
        v.append(mat[j][i])
    res.append(v)


""" Matrix Transpose Pythonic way
"""
  list(zip(*mat)) 
" * is essenatial (* changes list as arguments, ** changes dict to args)"