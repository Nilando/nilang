a = [0];
b = clone(a);
b[0] = 1;

print(a);
print(b);

a = "foo";
b = clone(a);
b << "bar";

print(a);
print(b);
