fn fizzbuzz(n) {
  if (((n % 5) == 0) || n.to_s().contains("5"))
  &&  (((n % 7) == 0) || n.to_s().contains("7"))
  {
    print "fizzbuzz";
  }

  if n % 5 == 0 || n.to_s().contains("5") 
  {
    print "fizz";
  }

  if n % 7 == 0 || n.to_s().contains("7") {
    print "buzz";
  }
}

print fizzbuzz(51); // fizz
print fizzbuzz(10); // fizz
print fizzbuzz(17); // buzz
print fizzbuzz(14); // buzz
print fizzbuzz(70); // fizzbuzz
print fizzbuzz(35); // fizzbuzz
