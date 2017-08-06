//DuckTyping
//----------
//  If it walks like duck, if it swims like duck, if its quack like duck 
//  Then it is duck.

//So if Different Object behaves like another Object both are same

// Javascript provieds facility for ducktyping using Callmethod.

//for ex
function Tom(firstname,lastname) {
	this.firstname = firstname;
	this.lastname = lastname;
	this.AboutMe = function() {
		console.log(`Hello My name is ${this.firstname},${this.lastname}`);
	}
}
var t = new Tom('Tom','Granger');
t.AboutMe();

function Dick(firstname,lastname,job){
	this.firstname = firstname;
	this.lastname = lastname;
	this.job = job;
}

//As per duck typing i can use toms AboutMe method for dick
//Since both have firstname,secondname
t.AboutMe.call(new Dick('Dick','Cock'));
//This is call duck typing.
//Javascript call function has facility for this
