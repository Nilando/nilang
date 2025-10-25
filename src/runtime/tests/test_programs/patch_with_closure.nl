// Patch with closure capturing state
multiplier = 10;

patch($int, $scale, fn(self) {
  return self * multiplier;
});

value = 1;
print(value.scale());

value = 2;
print(value.scale());
