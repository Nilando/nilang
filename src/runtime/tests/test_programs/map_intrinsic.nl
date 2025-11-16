// Test map() intrinsic

// Test 1: Empty map
empty = map();
print(type(empty));

// Test 2: Identity on existing map
original = {a: 1, b: 2};
same = map(original);
print(same.a);
print(same.b);

// Test 3: Create map from list of pairs
pairs = [[$a, 1], [$b, 2], [$c, 3]];
m = map(pairs);
print(m.a);
print(m.b);
print(m.c);

// Test 4: Empty list to empty map
empty_list = [];
empty_map = map(empty_list);
print(type(empty_map));

// Test 5: Nested values in pairs
nested_pairs = [[$name, "Alice"], [$age, 30], [$hobbies, ["reading", "coding"]]];
person = map(nested_pairs);
print(person.name);
print(person.age);
print(person.hobbies[0]);
print(person.hobbies[1]);
