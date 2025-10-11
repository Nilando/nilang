fn fibonacci_sequence(n) {
  result = [0];
  prev = 0;
  cur = 1;

  while #result < n {
    next = prev + cur;
    result << next;
    prev = cur;
    cur = next;
  }

  return result;
}

// print the first 100 fibonacci numbers
print(fibonacci_sequence(50));
