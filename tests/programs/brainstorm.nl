// this is a comment

// List of Types

// ==================== Number
42;
-333;
123.456;

// ==================== Bool
true;
false;

// ==================== Null
null;

// ==================== String
"this is a string";
""; // empty string

// ==================== List
a = [];
a = [1 + 2 + 3];
a = [1, 2, 3];
a = [[[[]]]];

a[0] = 1;
a[1] = a[0 + 0];

// ==================== Error
err = Error("error message");
err.throw();
err?; 

// ==================== Type Type
Error, List, Map, String, Num, Fn, AsyncFn, Bool, Null, Future

// ==================== Map
a = {};
a = {a : "a", b: 123, c: []};
a = {1 + 1: null};
a = {map: {}};
a = {foo: 1, "bar": 2};

// use 'map.key' to access symbolic keys
a.foo == 1;

// use 'map[key]' to access value keys
a["bar"] == 2;


// ==================== Function
fn() {};

// Putting an exclamation in front of a fn means it will catch a thrown error
// return it as its return value.
err = fn!() {
  throw Error("this will be caught");
};

fn(arg1, arg2, arg3) {
  print("Hello from function!");
  return arg1 + arg2 + arg3;
};

// functions can be assigned to values
my_fn = fn() { 1 + 1; };


// functions are values and can be assigned to vars
adder = fn(a, b) { 
  return a + b;
}

// functions support binding arguements
get_two = adder.bind(1, 1);
assert(2 == get_two() == add_one(1));

// calling a function
two = add_one(1);

// functions can create closures
adder_maker = fn(b) {
  return fn(a) { 
    return a + b;
  }
}

add_one = adder_maker(1);

get_counter = fn() {
  i = 0;

  return fn() {
    i += 1;

    return i;
  }
}


// ====================== Globals
// denoted by a leading @
@my_global_value = "data_accessible_everywhere";

data = @my_global_value;


// ====================== Global Intrinsic Functions
// denoted by a leading $
$print("message");
$error("message");
$assert(true != false);

bool_string = $type_string(true)

input_string = $read();
input_args = $args();

async_fn = async fn() {};

async_fn(); // launched asynchronously, future is ignored
my_future = async_fn(); // launched asynchronously and future is held onto
value = await my_future;
value = await async_fn();


// ====================== Control Flow
if false {
  print("this won't print");
}

if 1 == 2 {
  print("this won't print");
} else {
  print("testing");
}


a = [0];
// create a loop
while true {
  if a[0] > 10 {
    // breaking out of a while loop
    break;

  } else {
    a[0] = add_one(a[0]);

    // continuing in a while loop
    continue;
  }
  print("this won't print");
}

// return value from function, or if top level sets 
return;


// ========================= Truthiness Rules
// These rules apply when a value is used as a control flow condition
// [..], {..}, "..", fn(..) {..}, lists, maps, strings and functions always evaluate as true
// 0 & 0.0 eval as false, all other numbers are true
// true is true false is false
// null evals to false
// functions eval as true
