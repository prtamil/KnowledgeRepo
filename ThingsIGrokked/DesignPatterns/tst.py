def decor(fn):
    def wrapper(*args, **kwargs):
        print('Decorator')
        res = fn(*args, **kwargs)
        print(res)
    return wrapper


@decor
def sq(x):
    return x*x

[x*x for x in range(10)]
l = (x * x for x in range(10))

if __name__ == '__main__':
    sq(10)
