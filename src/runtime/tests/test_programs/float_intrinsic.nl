vals = [
  123,
  0.123,
  null,
  false,
  true,
  "123",
  "1.0",
  "nan",
  "bad"
];

i = 0;
while i < #vals {
  print(float(vals[i]));

  i = i + 1;
}
