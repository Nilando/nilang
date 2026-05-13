// Synthetic benchmark exercising a mix of language features.

fn factorial(n) {
  if n <= 1 {
    return 1;
  }
  return n * factorial(n - 1);
}

fn make_counter(start) {
  state = {value: start};
  return fn() {
    state.value = state.value + 1;
    return state.value;
  };
}

fn build_range(n) {
  result = [];
  i = 0;
  while i < n {
    result << i;
    i = i + 1;
  }
  return result;
}

fn sum_array(xs) {
  total = 0;
  for x in xs {
    total = total + x;
  }
  return total;
}

fn product_array(xs) {
  total = 1;
  for x in xs {
    if x == 0 {
      return 0;
    }
    total = total * x;
  }
  return total;
}

fn map_doubled(xs) {
  result = [];
  for x in xs {
    result << (x * 2);
  }
  return result;
}

fn filter_evens(xs) {
  result = [];
  for x in xs {
    if (x / 2) * 2 == x {
      result << x;
    }
  }
  return result;
}

fn new_accumulator() {
  return {
    items: [],
    add: fn(self, x) {
      self.items << x;
    },
    mean: fn(self) {
      if #self.items == 0 {
        return 0;
      }
      total = 0;
      for v in self.items {
        total = total + v;
      }
      return total / #self.items;
    },
    max: fn(self) {
      if #self.items == 0 {
        return 0;
      }
      best = self.items[0];
      for v in self.items {
        if v > best {
          best = v;
        }
      }
      return best;
    }
  };
}

print("=== factorials ===");
i = 1;
while i <= 8 {
  print("factorial({str(i)}) = {str(factorial(i))}");
  i = i + 1;
}

print("=== counter ===");
counter = make_counter(100);
i = 0;
while i < 5 {
  print(counter());
  i = i + 1;
}

print("=== array ops ===");
range = build_range(30);
print("range sum: {str(sum_array(range))}");
print("evens sum: {str(sum_array(filter_evens(range)))}");
print("doubled sum: {str(sum_array(map_doubled(range)))}");

small = [1, 2, 3, 4, 5];
print("small product: {str(product_array(small))}");

print("=== accumulator ===");
acc = new_accumulator();
for n in range {
  acc.add(n * 3);
}
print("mean: {str(acc.mean())}");
print("max: {str(acc.max())}");

print("=== branches ===");
flag = true;
if flag {
  print("truthy");
} else {
  print("falsy");
}

label = "answer";
value = 42;
print("{label}: {str(value)}");
