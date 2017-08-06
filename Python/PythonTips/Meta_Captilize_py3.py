class MetaUpperAttribute(type):
    
    def __new__(cls,name,bases,dct):
        ua = {}
        for name,val in dct.items():
            if not name.startswith('_'):
                ua[name.upper()] = val
            else:
                ua[name]=val
        return super().__new__(cls,name,bases,ua)


#use it as

class Tam(metaclass=MetaUpperAttribute):
    x = 20

t = Tam()
print(t.X)

