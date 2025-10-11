m = {};

i = 0;
while i < 1000 {
  m[i] = i;

  i = i + 1;
}

i = 0;
while i < 1000 {
  delete(m, i);

  m[i] = "DELETED";

  i = i + 2;
}

i = 0;
while i < 1000 {
  print(m[i]);

  i = i + 1;
}
