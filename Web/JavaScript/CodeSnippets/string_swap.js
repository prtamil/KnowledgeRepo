//Swapping string
//Javascript arrays are immutable you cannot change it.

//wht we can do is we can copy string -> convert to array -> modify -> Join array

var str = "Tamil";
 var s = str.slice(0);
 var sa = [].slice.call(s);

 //swap sa
 var t = sa[0];
 sa[0] = sa[2];
 sa[2]  = t;

 //Join array
 var res = sa.join(''); 
 //res = 'maTil'