x = "this will be closed";

fn get_closure() {
  return fn() {
    return fn() {
      return fn() {
        return fn() {
          return fn() {
            return x;
          };
        };
      };
    };
  };
}

closure = get_closure();

x = "this value is not closed";

print closure(); // "this will be closed"
