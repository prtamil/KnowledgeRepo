 " 2d list comprehension reverse the list order
 """ for ex
   for x in sar:
   	for y in x:
   		"do something

  should be converted to

  [y for y in x for x in sar]
  """