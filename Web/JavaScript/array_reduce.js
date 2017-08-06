//How reduce Works
var ar = [1,2,3,4,5,6,6];
ar.reduce(function(prev,curr,index,array){
	prev = prev + curr;
	//prev acts as accumulator.
	//prev accumulates the result
	return prev;
});

//We can change the return type of reduce
//ususaly map,filter fn's return type is array
//but in reduce we can modify to any type
ar.reduce(function(acc,curr){
	//counts how many times each element in array present.
	//the return type is object
	//its specified in {} in reduce after specifing function.
	return acc[curr] = ++acc[curr] || 1;
},{});
