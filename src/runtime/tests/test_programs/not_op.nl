vals = [
  null,
  false,
  true,
  0,
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
  print(!!v);
  i = i + 1;
}
