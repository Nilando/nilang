fn foo() {
  x = 123;
  y = "pancake";
  print(x + y); // Type Error!
}

fn bar() {
  foo();
}

bar();
