fn fibonacci_sequence(n) {
  result = [];
  prev = 0;
  cur = 1;

  while len(result) < n {
    if len(result) == 0 {
      push(result, 0);

      continue;
    }

    next = prev + cur;
    push(result, next);
    prev = cur;
    cur = next;
  }

  return result;
}

// print the first 100 fibonacci numbers
print(fibonacci_sequence(50));
