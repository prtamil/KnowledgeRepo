//Check Object has own property instead of derived from prototype
Object.prototype.hasOwnProperty.call(obj,'property') == true 
//then its it own property
//we cal directly call
obj.hasOwnProperty('property');
//but if obj is null or primitives then obj. gives error
//to avoid this we use call 
Object.prototype.hasOwnProperty.call(theobject,'property');
//Since we use call and give objects manually instead of calling obj.hasOWnProperty
//we reduce the amount of error it gives.
