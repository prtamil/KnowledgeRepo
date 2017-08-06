class MetaSingleton(type):
    obj = None

    def __call__(cls,*args,**kwargs):
        if obj is None:
            obj = super().__call__(*args,**kwargs)
        return obj

#you can use it as

class Tam(metaclass=MetaSingleton):
    X = 10

t = Tam()
g = Tam()

#id(t)==id(g)

