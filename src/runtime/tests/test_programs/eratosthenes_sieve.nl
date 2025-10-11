fn eratosthenes(n) {
  primes = [];

  // init with all numbers marked as prime
  i = 2;
  while i <= n {
    primes << true;
    i = i + 1;
  }

  // sieve
  i = 2;
  while i <= n {
    if primes[i - 2] {
      k = i * 2;

      while k <= n {
        primes[k - 2] = false;
        k = i + k;
      }
    }

    i = i + 1;
  }

  // collect results
  result = [];
  i = 2;
  while i <= n {
    if primes[i - 2] {
      result << i;
    }
    i = i + 1;
  }

  return result;
}

print(eratosthenes(1000));
