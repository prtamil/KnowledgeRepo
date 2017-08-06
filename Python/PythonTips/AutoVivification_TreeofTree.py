AutoVivification
================
 autovivification is the automatic creation of new arrays and hashes as required every time an undefined value is dereferenced

How to do
=========
# yo dawg, i heard you liked dicts                                                                      
def yodict():
    return defaultdict(yodict)

Vdict = lambda *args, **kwargs: defaultdict(Vdict, *args, **kwargs)


Another method
===============
I recently discovered a more elegant approach to the top answer here that has been available (and documented) since Python 2.5, and I love how it pretty prints just like a normal dict, instead of the ugly printing of an autovivified defaultdict:

class Vividict(dict):
    def __missing__(self, key):
        value = self[key] = type(self)()
        return value
The explanation: we're just providing another nested instance of our class Vividict whenever a key is accessed but missing. (Returning the value assignment is useful because it avoids us additionally calling the getter on the dict, and unfortunately, we can't return it as it is being set.)

Demonstration of Usage

Below is just an example of how this dict could be easily used to create a nested dict structure on the fly. This can quickly create a hierarchical tree structure as deeply as you might want to go.

import pprint

class Vividict(dict):
    def __missing__(self, key):
        value = self[key] = type(self)()
        return value

d = Vividict()

d['foo']['bar']
d['foo']['baz']
d['fizz']['buzz']
d['primary']['secondary']['tertiary']['quaternary']
pprint.pprint(d)
Which outputs:

{'fizz': {'buzz': {}},
 'foo': {'bar': {}, 'baz': {}},
 'primary': {'secondary': {'tertiary': {'quaternary': {}}}}}
And as the last line shows, it pretty prints beautifully and in order for manual inspection. But if you want to visually inspect your data, implementing __missing__ to set a new instance of its class to the key and return it is a far better solution.

Another Method
===============

This is a function that returns a nested dictionary of arbitrary depth:

from collections import defaultdict
def make_dict():
    return defaultdict(make_dict)
Use it like this:

d=defaultdict(make_dict)
d["food"]["meat"]="beef"
d["food"]["veggie"]="corn"
d["food"]["sweets"]="ice cream"
d["animal"]["pet"]["dog"]="collie"
d["animal"]["pet"]["cat"]="tabby"
d["animal"]["farm animal"]="chicken"
Iterate through everything with something like this:

def iter_all(d,depth=1):
    for k,v in d.iteritems():
        print "-"*depth,k
        if type(v) is defaultdict:
            iter_all(v,depth+1)
        else:
            print "-"*(depth+1),v

iter_all(d)
This prints out:

- food
-- sweets
--- ice cream
-- meat
--- beef
-- veggie
--- corn
- animal
-- pet
--- dog
---- labrador
--- cat
---- tabby
-- farm animal
--- chicken
You might eventually want to make it so that new items can not be added to the dict. Try this

def fix(d):
    d.default_factory = lambda: None
    for v in d.values():
        if type(v) is defaultdict:
            fix(v)
