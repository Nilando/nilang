vals = [
  null,
  false,
  0,
  true,
  1,
  1.234,
  [],
  {},
  "",
  "false",
  fn(a,b,c) {},
  $sym,
  $null,
  $false,
];

i = 0;
while i < #vals {
  v = vals[i];
  print(bool(v));
  i = i + 1;
}
