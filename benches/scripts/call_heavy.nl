// Function-call-heavy benchmark: maximizes fn calls per instruction
// to expose call/return overhead. Each call does almost no work.

fn recurse(n) {
  if n <= 0 {
    return 0;
  }
  return recurse(n - 1);
}

i = 0;
while i < 10000 {
  recurse(100);
  i = i + 1;
}

print("done");
