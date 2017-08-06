//to use some array methods such as filter to string 
//we can use like 

var s = "Sparta";

var t = [].filter.call(s,function(val,idx){
  return val ? true : false;
});

console.log(t);
