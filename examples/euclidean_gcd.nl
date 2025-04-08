fn find_gcd(a, b) {
  if a == b {
    return a;
  }

  if a < b {
    t = a;
    a = b;
    b = t;
  }

  remainder = a % b;

  if remainder == 0 {
    return b;
  } else {
    return find_gcd(b, remainder);
  }
}

print find_gcd(31256365662, 14347398432); // 6
