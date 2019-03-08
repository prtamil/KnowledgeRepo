def checkprime(n):
    res = []
    for x in range(1,n+1):
       if n % x == 0:
           res.append(x)
    if len(res) > 2 :
        return False
    else:
        return True




if __name__ == '__main__':
    print([checkprime(x) for x in [197,719,971]])


