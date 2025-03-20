fn fibonacci_sequence(n) {
  result = [];
  prev = 0;
  cur = 1;

  while result.length < n {
    if result.length == 0 {
      result.push(0);

      continue;
    }

    next = prev + cur;
    result.push(next);
    prev = cur;
    cur = next;
  }

  return result;
}

// print the first 100 fibonacci numbers
print fibonacci_sequence(100);
