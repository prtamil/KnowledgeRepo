""" Dynamic Programming Fibonacci
"""

def fibo(n):
    fib = {}
    for k in range(1,n+1):
        if k <= 2:
            f = 1
        else:
            f = fib[k-1] + fib[k-2]
        fib[k] = f
    return fib[n]

""" Generator Fibonacci
"""
 def fib():
            a, b = 0, 1
            while 1:
                yield b
                a, b = b, a+b

""" use this
b = fib()
[next(b) for x in range(10)]
"""

"""  Using looping technique
"""
def fib(n):
 a,b = 1,1
 for i in range(n-1):
  a,b = b,a+b
 return a

print fib(5)
 
""" Using recursion
"""
def fibR(n):
 if n==1 or n==2:
  return 1
 return fib(n-1)+fib(n-2)

print fibR(5)


""" Using Memonization
"""

def memoize(fn, arg):
 memo = {}
 if arg not in memo:
  memo[arg] = fn(arg)
 return memo[arg]
 
## fib() as written in example 1.
fibm = memoize(fib,5)
print fibm
 
"""Using memoization as decorator
"""
class Memoize:
 def __init__(self, fn):
  self.fn = fn
  self.memo = {}
 def __call__(self, arg):
  if arg not in self.memo:
   self.memo[arg] = self.fn(arg)
  return self.memo[arg]
 
@Memoize
def fib(n):
 a,b = 1,1
 for i in range(n-1):
  a,b = b,a+b
 return a
print fib(5)