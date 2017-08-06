# Set tips
# if you have list of sets 
ls = [{1,2,3,4},{1,2,3},{1,4,5,6,2}]
# Common of all sets in list
#----------------------------
c = ls[0]
for x in ls[1:]:
	c = c.intersection(x)
# c has the common element
#---------------------------
# Another Tric
res = set.intersection(*ls)
# cool


