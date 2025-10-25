// Basic patch test - adding a method to str type
patch($str, $exclaim, fn(self) {
  result = self;
  result << "!";
  return result;
});

message = "Hello";
result = message.exclaim();
print(result);
