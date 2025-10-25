// For loop with custom iterator object
// Create a range object with an iter method
fn make_range(start, end) {
  range = {};
  range.start = start;
  range.end = end;

  range.iter = fn(self) {
    current = [self.start];
    return fn() {
      if current[0] < self.end {
        val = current[0];
        current[0] = current[0] + 1;
        return val;
      } else {
        return null;
      }
    };
  };

  return range;
}

range = make_range(2, 7);

for n in range {
  print(n);
}
