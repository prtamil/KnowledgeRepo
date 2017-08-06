"Using closure for Counter in python
def  counter():
	cnt = 0;
	def inner():
		nonlocal cnt  "nonlocal is for python3 for python2 its not needed"
		cnt += 1
		return cnt
	return inner

s = counter()
s()
s()

