fn raise(n, x) {
  if x == 0 {
    return 1;
  }

  result = n;

  x = x - 1;

  while x > 0 {
    result = result * n;

    x = x - 1;
  }

  return result;
}

p = 0;
while p < 100 {
  print(raise(2, p));
  p = p + 1;
}
