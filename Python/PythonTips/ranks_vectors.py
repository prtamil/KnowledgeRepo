"""Rank Vectors

Given an array (or list) of scores, return the array of ranks for each value in the array. The largest value has rank 1, the second largest value has rank 2, and so on. Ties should be handled by assigning the same rank to all tied values. For example:

ranks([9,3,6,10]) = [2,4,3,1] and ranks([3,3,3,3,3,5,1]) = [2,2,2,2,2,1,3]
"""
def rank(data):
	scores = {} """dict to store scores"""
	for i,val in enumerate(sorted(data,reverse=True)):
		if val not in scores:
			scores[val] = i + 1

	return [scores[n] for n in data]


