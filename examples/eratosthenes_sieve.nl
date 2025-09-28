fn eratosthenes(n) {
  primes = [];

  // init with all numbers marked as prime
  i = 1;
  while i <= n {
    push(primes, i != 1);
    i = i + 1;
  }

  // sieve
  i = 1;
  while i <= n {
    if primes[i - 1] {
      k = i * 2;

      while k <= n {
        primes[k - 1] = false;
        k = i + k;
      }
    }

    i = i + 1;
  }

  // collect results
  result = [];
  i = 1;
  while i <= n {
    if primes[i - 1] {
      push(result, i);
    }
    i = i + 1;
  }

  return result;
}

print(eratosthenes(1000000));
