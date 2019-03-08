from functools import wraps

class DemoException(Exception):
    ...

def demo_exc_handling():
    print(' -> CoRoutine Started')
    while True:
        try:
            x = yield
        except DemoException:
            print('---Demo Exception')
        else:
            print('Corotine received: {!r}'.format(x))
        finally:
            print('Co Routine Ending ->')

def coroutine(func):
    @wraps(func)
    def primer(*args, **kwargs):
        "Prime the generator (starting coroutine by priming it by next)"
        gen = func(*args, **kwargs)
        next(gen)
        return gen
    return primer

from collections import namedtuple

Result = namedtuple('Result', 'count average')

@coroutine
def avger():
    total = 0.0
    count = 0
    avg = None
    while True:
        term = yield avg
        if term is None:
            break
        total += term
        count += 1
        avg = total / count
    return Result(count, avg)

