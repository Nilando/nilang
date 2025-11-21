fn infinite_recursion(depth) {
  return infinite_recursion(depth + 1);
}

infinite_recursion(0);
