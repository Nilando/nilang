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

// ==================== Map
a = {};
a = {a : "a", b: 123, c: []};
a = {1 + 1: null};
a = {map: {}};
a = {foo: 1, "bar": 2};

// use 'map.key' to access symbolic keys
print(a.foo == 1); // true

// use 'map[key]' to access value keys
a["bar"] == 2;

// ==================== Function
fn () {};

fn test (arg1, arg2, arg3) {
  print("Hello from function!");
  return arg1 + arg2 + arg3;
}


// functions can be assigned to values
my_fn = fn() { 1 + 1; };


// functions are values and can be assigned to vars
adder = fn(a, b) { 
  return a + b;
};

// functions support binding arguements
get_two = adder.bind(1, 1);
assert(2 == get_two() == add_one(1));

// calling a function
two = add_one(1);

// functions can create closures
adder_maker = fn(b) {
  return fn(a) { 
    return a + b;
  };
};

add_one = adder_maker(1);

get_counter = fn() {
  i = 0;

  return fn() {
    i = i + 1;

    return i;
  };
};



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
