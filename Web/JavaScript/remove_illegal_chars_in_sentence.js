var sentence = 'this is sparta&*&*& and ia%$#@m sa****(())m';

console.log(
  sentence.replace(/[^A-Za-z0-9 ]/g,'')
);


//[] -> group
//[^ ]-> negate group

