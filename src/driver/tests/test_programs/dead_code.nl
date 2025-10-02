// these instructions are all dead
a = 12345;
a = "foo";
a = 1 + 1;
a = @my_global;
a = a[0];
a = x.y;
a = a * a;

// can't remove these instructions due to side effects
b = fn() {};
b = b();
b = [0];
b[0] = 0;
print(b);
