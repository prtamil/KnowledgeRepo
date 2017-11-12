//convert function argument as array
//if your obj has length property you can use slice
function name(){
  console.log(arguments);//pseudo array
  var args = Array.prototype.slice.call(arguments);
  console.log(args); //proper array
}

name('this','is','sparta');

function st(){
  var args = [].slice.call(arguments);
  console.log(args);
}

st('1',2,3,4,5,56,6);
