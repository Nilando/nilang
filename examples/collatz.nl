fn step_function(n) {
  if (n % 2) == 0 {
    return n / 2;
  } else {
    return ((3*n) + 1);
  }
}

steps = 0;
x = num(read);

if (x == null) || (x != x.floor()) {
  print("Invalid integer");
  return;
}

while x != 1 {
  print(x);
  x = step_function(x);
  steps = steps + 1;
}

print(steps);
