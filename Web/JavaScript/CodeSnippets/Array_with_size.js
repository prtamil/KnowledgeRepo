Creating Array with size
=========================

Array(5) //gives you an array with length 5 but no values, hence you can't interate over it.

Array.apply(null, Array(5)).map(function () {}) //gives you an array with length 5 and undefined as values, now it can be iterated over.

Array.apply(null, Array(5)).map(function (x, i) { return i; }) //gives you an array with length 5 and values 0,1,2,3,4.

Array(5).forEach(alert) does nothing, 
Array.apply(null, Array(5)).map(function () {}).forEach(alert) //gives you 5 alerts