fn rotate_array(array) {
  prev = array[-1];

  i = 0;
  while i < #array {
    temp = array[i];
    array[i] = prev;
    prev = temp;
    i = i + 1;
  }
}

my_array = [1, 2, 3, 4];

rotate_array(my_array);
print(my_array); // [4, 1, 2, 3]
rotate_array(my_array);
print(my_array); // [3, 4, 1, 2]
rotate_array(my_array);
print(my_array); // [2, 3, 4, 1]
rotate_array(my_array);
print(my_array); // [1, 2, 3, 4]
