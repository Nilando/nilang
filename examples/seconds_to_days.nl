fn print_seconds_in_days(seconds) {
  m = 60;
  h = m * 60;
  d = 24 * h;

  print(seconds / d);
}

print_seconds_in_days(read);
