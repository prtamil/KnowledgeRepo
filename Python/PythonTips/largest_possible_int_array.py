""" Find largest possible integer number in array
 for ex [9,50,6] => 9650"""
 "initial false
 arr = [9,50,6]
 ''.join(sorted([str(x) for x in arr],reverse=True))

 " correct one
 def biggnum(arr):
    sar = [str(x) for x in arr]
    sep = [y for x in sar for y in x]
    return ''.join(sorted(sep,reverse=True))

