print("=====  ADDITION");
print(1 + 1.0);
print(1.0 + 1);
print(1 + 1.0);
print(1 + 1);
print(1 + 1.123);
print(1.123 + 1);
print(1.120 + 1.003);

// should cause overflow
print(2147483647 + 1);
print(2147483647 + 1.123);

print("=====  SUBTRACTION");
print(1 - 1.0);
print(1.0 - 1);
print(1 - 1.0);
print(1 - 1);
print(1 - 1.123);
print(1.123 - 1);
print(1.123 - 0.123);

print("=====  MULTIPLICATION");
print(3 * 3);
print(3.3 * 3);
print(3 * 3.3);
print(3.3 * 3.3);
print(3.3 * -1);
print(1247928394.32423423 * 23423497293.12313);

print("=====  DIVISION");
print(1/1);
print(2/1);
print(1/2);
print(1.0/2);
print(1/2.0);
print(1.0/2.0);
print(2.0/1.0);

print("=====  MODULO");
print(1%1);
print(2%1);
print(1%2);
print(1.0%2);
print(1%2.0);
print(1.0%2.0);
print(2.0%1.0);

print("=====  MATH INTRINSICS");
print(pow(2, 4));
print(log(16, 2));
print(abs(-1));
print(ceil(0));
print(floor(0));

print("=====  COMPARISONS");
print(1 > 0);
print(1 >= 0);
print(1 < 0);
print(1 <= 0);
print(1 == 0);
print(1 != 0);
print(1 > 1);
print(1 >= 1);
print(1 < 1);
print(1 <= 1);
print(1 == 1);
print(1 != 1);
print(0 > 1);
print(0 >= 1);
print(0 < 1);
print(0 <= 1);
print(0 == 1);
print(0 != 1);
