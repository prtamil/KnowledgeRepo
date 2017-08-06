""" Finding max value in a dictornary """
d = {}

max(d.keys(),key=(lambda key: d[key]))

""" Max function takes the key array and return



How `key` works?
By default in Python 2 key compares items based on a set of rules based on the type of the objects(for example a string is always greater than an integer).

To modify the object before comparison or to compare based on a particular attribute/index you've to use the key argument.

Example 1:

A simple example, suppose you've a list of numbers in string form, but you want to compare those items by their integer value.

>>> lis = ['1','100','111','2']
Here max compares the items using their original values(strings are compared lexicographically so you'd get '2' as output) :

>>> max(lis)
'2'
To compare the items by their integer value use key with a simple lambda:

>>> max(lis, key=lambda x:int(x))  #compare `int` version of each item
'111'
Example 2: Applying max to a list of lists.

>>> lis = [(1,'a'),(3,'c'), (4,'e'), (-1,'z')]
By default max will will compare the items by the first index, if the first index is same then it'd compare the second index. As in my example all items have unique first index so, you'd get this as the answer:

>>> max(lis)
(4, 'e')
But, what if you wanted to compare each item by the value at index 1? Simple, use lambda:

>>> max(lis, key = lambda x: x[1])
(-1, 'z')
Comparing items in an iterable that contains objects of different type:

List with mixed items:

>>> lis = ['1','100','111','2', 2, 2.57]
In Python 2 it is possible to compare items of two different types:

>>> max(lis) # works in Python 2
'2'
>>> max(lis, key=lambda x: int(x)) #compare integer version of each item
'111'
But in Python 3 you can't do that any more:

>>> lis = ['1','100','111','2', 2, 2.57]
>>> max(lis)
Traceback (most recent call last):
  File "<ipython-input-2-0ce0a02693e4>", line 1, in <module>
    max(lis)
TypeError: unorderable types: int() > str()
But this works, as we are comparing integer version of each object:

>>> max(lis, key=lambda x: int(x)) # or simply `max(lis, key=int)`
'111'
"""