class Tam:
    def __init__(self,lst):
        self.lst = lst
    def __getitem__(self, val):
        print(self.lst)
        if isinstance(val,slice):
             print (slice.start,slice.stop,slice.step)
             res = [self.lst[ii] for ii in range(*val.indices(len(self.lst)))] #should be val.indices(len(self.datastruct))
             print(res)
             return res
        return self.lst[key]

#u can use it like
t = Tam([1,2,3,4,5,6])
t[1:5]


