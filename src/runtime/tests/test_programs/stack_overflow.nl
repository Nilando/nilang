// Test that stack overflow is detected correctly
fn recurse(n) {
  return recurse(n + 1);
}

recurse(0);
