fn contains_letter(s, letter) {
  for c in s {
    if letter == c {
      return true;
    }
  }

  return false;
}

fn fizzbuzz(n) {
  s = str(n);
  fizz = ((n % 5) == 0) || contains_letter(s "5");
  buzz = ((n % 7) == 0) || contains_letter(s, "7");

  if fizz && buzz {
    print("fizzbuzz");
  }

  if fizz {
    print("fizz");
  }

  if buzz {
    print("buzz");
  }
}

print(fizzbuzz(51)); // fizz
print(fizzbuzz(10)); // fizz
print(fizzbuzz(17)); // buzz
print(fizzbuzz(14)); // buzz
print(fizzbuzz(70)); // fizzbuzz
print(fizzbuzz(35)); // fizzbuzz
