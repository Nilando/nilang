lucky_number = 42;
guess = 10;

print("guess the lucky number: ");

if guess == lucky_number {
  print("You got it!");
} else {
  if guess > lucky_number {
    print("too high!");
  } else {
    print("too low :(");
  }
}
