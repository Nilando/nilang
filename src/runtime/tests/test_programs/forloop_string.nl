// For loop iterating over a string
// Manually patch iter function onto string type
patch($str, $iter, fn(self) {
  i = [0];
  return fn() {
    n = i[0];
    if i[0] < #self {
      i[0] = i[0] + 1;
      return self[n];
    } else {
      return $iter_end;
    }
  };
});

text = "Hello";

for char in text {
  print(char);
}
