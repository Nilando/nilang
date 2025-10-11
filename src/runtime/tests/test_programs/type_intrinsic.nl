vals = [
  null,
  0,
  1.0,
  "foo",
  true,
  [],
  {},
  fn() {},
  $bar
];


i = 0;
while i < #vals {
  val = vals[i];
  print(type(val));
  i = i + 1;
}
