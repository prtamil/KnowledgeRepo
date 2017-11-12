/*  
   Call and Apply
   ---------------
       Both are same except passing parameters.
       Call -> Comma, -> Passing arguments in comma seperated way.
       Apply -> Array, -> Passing arguments in array.

	Both calls the function immediatly
 
   Bind
   ----
       Bind takes copy of the function. It doesn't call immediatly
       Bind takes copy of function with user defined this and assign to vars
       We can call the modified function later.
     */


//They all attach this into function (or object) and the difference is in the function invocation (see below).

//call attaches this into function and executes the function immediately:

var person = {  
  name: "James Smith",
  hello: function(thing) {
    console.log(this.name + " says hello " + thing);
  }
}

person.hello.call(person, "world"); // output: James Smith says hello world
//bind attaches this into function and it needs to be invoked separately like this:

var person = {  
  name: "James Smith",
  hello: function(thing) {
    console.log(this.name + " says hello " + thing);
  }
}

var helloFunc = person.hello.bind(person);
helloFunc("world");  // output: James Smith says hello world
//or like this:

//...    
var helloFunc = person.hello.bind(person, "world");
helloFunc();  // output: James Smith says hello world
//apply is similar to call except that it takes an array-like object instead of listing the arguments out one at a time:

function personContainer() {
  var person = {  
     name: "James Smith",
     hello: function() {
       console.log(this.name + " says hello " + arguments[1]);
     }
  }
  person.hello.apply(person, arguments);
}
personContainer("world", "mars"); // output: James Smith says hello mars


