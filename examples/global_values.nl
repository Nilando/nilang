fn add_global(x) {
  @y = @y + x;
  return @y;
}

@y = 0;

print add_global(1); // 1
print add_global(1); // 2
print add_global(1); // 3
