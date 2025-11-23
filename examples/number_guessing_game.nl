// Simple Linear Congruential Generator for random numbers
fn lcg(seed, min, max) {
  a = 1664525;
  c = 1013904223;
  m = 2.pow(32);

  random_val = ((a * seed + c) % m);
  range = max - min + 1;

  return min + (random_val % range);
}

// Get a pseudo-random seed based on current "randomness"
// In a real game, you could ask the user for a number as seed
fn get_seed() {
  // Using a hardcoded seed for reproducibility
  // You can change this number to get different target numbers
  print("enter random seed:");
  return int(read);
}

fn play_game() {
  print("=================================");
  print("  Welcome to the Number Guessing Game!");
  print("=================================");
  print("");

  // Generate a random number between 1 and 100
  seed = get_seed();
  target = lcg(seed, 1, 100);
  attempts = 0;
  max_attempts = 10;

  print("I'm thinking of a number between 1 and 100.");
  print("You have {max_attempts} attempts to guess it!");
  print("");

  while attempts < max_attempts {
    attempts_left = max_attempts - attempts;
    print("Attempt {attempts + 1}/{max_attempts} - Enter your guess:");

    guess_str = read;
    guess = int(guess_str);

    if guess == null {
      print("Please enter a valid number!");
      print("");
      continue;
    }

    attempts = attempts + 1;

    if guess == target {
      print("");
      print("🎉 Congratulations! You guessed it in {attempts} attempts!");
      print("The number was {target}!");
      return true;
    }

    if guess < target {
      print("Too low! Try again.");
    } else {
      print("Too high! Try again.");
    }

    print("");
  }

  print("Game Over! You've run out of attempts.");
  print("The number was {target}.");
  return false;
}

fn ask_to_play_again() {
  print("");
  print("Would you like to play again? (y/n)");
  answer = read.trim().to_lower();

  return answer == "y" || answer == "yes";
}

// Main game loop
fn main() {
  play_game();

  while ask_to_play_again() {
    print("");
    play_game();
  }

  print("");
  print("Thanks for playing! Goodbye!");
}

main();
