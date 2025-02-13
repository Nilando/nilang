doubler = fn { return @ * 2; };
a = 2;

while a <= 1024 {
  print(a);

  a = doubler(a);
}
