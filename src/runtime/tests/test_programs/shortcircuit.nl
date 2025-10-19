fn test(flag, a, b) {
  if flag && ((a != 2) && (b != 3)) {
    print("test");
  }
}

test(false, 2, 3);
test(true, 2, 3);
test(true, 0, 3); 
test(true, 0, 0); // test
test(false, 0, 3);
test(false, 2, 3);
