// DataBinary name space object

let DataBinary = {};







//
// Serializers
//



// `DataBinary.Serializer` is a constructor function that creates a serializer,
// which is just a stateful way of concatenating to a string state.

DataBinary.Serializer = function () {
  this.byteString = "";
};



// The `put` method on serializers is just a way to concatenate to the
// byte string property. Given an integer, it converts that to the
// corresponding string/character value and appends it to the end of the
// byte string.

DataBinary.Serializer.prototype.put = function (x) {
  this.byteString += String.fromCharCode(x);
};



// The `putByteString` method on serializers is a convenient way to add an
// entire byte string to the serializer's internal state.

DataBinary.Serializer.prototype.putByteString = function (b) {
  this.byteString += b;
};



// The `runSerializer` function takes a put function for the appropriate type,
// and some data of that type, and runs the put function on a new serializer
// derived from that data. For example, if `putInt` is the put function for
// Int, then by calling `DataBinary.runSerializer(putInt, 42)`, we would get
// back the byte string "*".

DataBinary.runSerializer = function (f,x) {
  let s = new DataBinary.Serializer;
  f(x,s);
  return s.byteString;
};







//
// Deserializers
//



// `DataBinary.Deserializer` is a constructor function that takes a byte
// string, splits it into characters, converts them to their char codes, and
// stores the array in the returned object's `bytes` field. This is then used
// as a kind of stream of bytes for deserialization.

DataBinary.Deserializer = function (b) {
  this.bytes = b.split("").map(c => c.charCodeAt(0));
};



// The `get` method of deserializers simply a way to get the next byte from
// the deserializer's byte array.

DataBinary.Deserializer.prototype.get = function () {
  return this.bytes.shift();
};



// The `getByteString` method of deserializers simply reads off the first `n`
// characters from the bytes into a byte string.

DataBinary.Deserializer.prototype.getByteString = function (n) {
  return this.bytes.splice(0, n).map(b => String.fromCharCode(b)).join("");
};



// The `runDeserializer` function takes a get function for the appropriate
// type and a byte string, and runs the get function on a new deserializer
// derived from the byte string. For example, if `getInt` is the function for
// Int off a deserializer, then we might run
// `DataBinary.runDeserializer(getInt, "*")` and we'd get back the Int `42`.

DataBinary.runDeserializer = function (f, b) {
  return f(new DataBinary.Deserializer(b));
};







//
// Standard Put functions
//



// Putting an Int involves just storing it.

DataBinary.putInt = function (n, serializer) {
  serializer.put(n);
};



// Putting a String involves storing its length, and then writing the
// string to the serializer as a byte string.

DataBinary.putString = function (s, serializer) {
  DataBinary.putInt(s.length, serializer);
  serializer.putByteString(s);
};



// Serializing an Array of A's involves storing the array's length, then
// serializing each element according to the provided serializer function.
// Because of this, we make the function for array's curried, taking as its
// first argument the serializer of its data.

DataBinary.putArray = function (f) {
  return function (xs, serializer) {
    DataBinary.putInt(xs.length, serializer);
    for (let i = 0; i < xs.length; i++) {
      f(xs[i], serializer);
    }
  };
}







//
// Standard Get functions
//



// Getting an Int involves just getting the next byte.

DataBinary.getInt = function (deserializer) {
  return deserializer.get();
};



// Getting a String involves getting the length, then reading that many bytes
// as a byte string.

DataBinary.getString = function (deserializer) {
  let l = DataBinary.getInt(deserializer);
  return deserializer.getByteString(l);
};



// Getting an Array of A's involves getting the length, then reading that many
// A's using the provided A get function.

DataBinary.getArray = function (f) {
  return function (deserializer) {
    let l = DataBinary.getInt(deserializer);
    let xs = [];
    
    for (let i = 0; i < l; i++) {
      xs.push(f(deserializer));
    }
    
    return xs;
  };
};