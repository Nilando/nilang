fn contains(s, digit) {
  for c in s {
    if c == digit {
      return true;
    }
  }
  return false;
}

fn fizzbuzz(n) {
  s = str(n);
  fizz = ((n % 5) == 0) || contains(s, "5");
  buzz = ((n % 7) == 0) || contains(s, "7");

  if fizz && buzz {
    return "fizzbuzz";
  }

  if fizz {
    return "fizz";
  }

  if buzz {
    return "buzz";
  }
}

print(fizzbuzz(51)); // fizz
print(fizzbuzz(10)); // fizz
print(fizzbuzz(17)); // buzz
print(fizzbuzz(14)); // buzz
print(fizzbuzz(70)); // fizzbuzz
print(fizzbuzz(35)); // fizzbuzz
print(fizzbuzz(57)); // fizzbuzz
