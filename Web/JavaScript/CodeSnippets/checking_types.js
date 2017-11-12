function chktypes(arg){
  return Object.prototype.toString.call(arg);
}

function tst(){
console.log(
  chktypes(arguments)
);
}

tst(1,2,3,4,5)
//prints [object Type] for ex [object Number]
