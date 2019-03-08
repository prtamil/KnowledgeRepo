def myzip(*iters):
    sentinel = object()
    iterators = [iter(it) for it in iters]
    while iterators:
        res = []
        for it in iterators:
            elem = next(it, sentinel)
            if elem is sentinel:
                return
            res.append(elem)
        yield tuple(result)
