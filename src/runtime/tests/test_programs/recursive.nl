fn count_to_ten_from(i) {
  print(i);

  if i == 10 {
    return;
  }

  count_to_ten_from(i + 1);
}

count_to_ten_from(0);
