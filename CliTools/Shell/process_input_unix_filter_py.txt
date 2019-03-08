How to write Unix filter in python ? How to get stdin from pipe ?
-------------------------------------------------------------------

1. using sys
  
    import sys
    
    for line in sys.stdin:
        process(line.rstrip('\r\n'))

2. using fileinput

   import fileinput
   for line in fileinput.input():
       process(line)
