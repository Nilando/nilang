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

print(find_gcd(0, 0));
print(find_gcd(1, 1));
print(find_gcd(13, 13));
print(find_gcd(16, 32));
print(find_gcd(81, 18));
print(find_gcd(31256365662, 14347398432));
print(find_gcd(1023, 64));
print(find_gcd(1024, 64));
