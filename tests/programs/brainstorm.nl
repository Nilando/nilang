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

// Heres some map examples...
map = {1: "x", a: "y"};

// use 'map.key' to access symbolic keys
print(map.a == "y"); // true

// however map[key], will try to use the value stored in key
a = 1;
print(map[a] == "y"); // false

// use 'map[*value*]' to access value keys
print(map[1] == "x"); // true

// ==================== Function
// There are 2 kinds of ways to declare a function.

// anonymous func
// can be used as an expression
fn () {};

// named func
// can only be written where a statement is needed

fn test (arg1, arg2, arg3) {
  print("Hello from function!");
  return arg1 + arg2 + arg3;
}


// functions can be assigned to values
fn my_fn () { 
  return 1 + 1; 
}

// or like this
my_fn = fn() { 
  return 1 + 1; 
};

// functions are first class citizens. Store them wherever you like
list[0] = my_fn;
map.b = my_fn;
var = my_fn;
other_func(my_fn);

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
