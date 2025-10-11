a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
b = "abcdefghij";

i = 0;
while i < 12 {
  x = ^a;
  y = ^b;

  print([x, y]);

  i = i + 1;
}
