vals = [
  123,
  0.123,
  0.999,
  null,
  false,
  true,
  "123",
  "1.0",
  "nan"
];

i = 0;
while i < #vals {
  print(int(vals[i]));

  i = i + 1;
}
