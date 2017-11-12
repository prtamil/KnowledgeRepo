var s = 'sparta';

var t = [].map.call(s,function(val,idx){
  var x = val.charCodeAt(0);
  x = x + 1;
  var y= String.fromCharCode(x);
  return y;
});

var xy = [].reduce.call(t, function(acc,it){
  acc = acc.concat(it);
  return acc;
},'');
console.log(xy);
