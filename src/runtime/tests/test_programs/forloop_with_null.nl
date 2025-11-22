// Test that for-loops can iterate over lists containing null values and mixed types
patch($list, $iter, fn(self) {
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

// List with null values and mixed types
mixed_list = [1, null, "hello", null, true, false, null, 42];

count = 0;
for x in mixed_list {
  count = count + 1;
}

// Should iterate all 8 elements including the nulls
print(count);
