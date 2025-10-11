fn recursive_function(i) {
  if i == 20 {
    print("oh no look out for that number!" + 1);
  } else {
    recursive_function(i + 1);
  }
}

recursive_function(0);
