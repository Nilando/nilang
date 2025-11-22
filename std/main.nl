fn default_iter(self) {
  i = [0];

  return fn() {
    n = i[0];
    if n < #self {
      i[0] = n + 1;
      return self[n];
    } else {
      return $iter_end;
    }
  };
};

fn default_enumerate(self) {
  i = [0];

  return fn() {
    n = i[0];
    if n < #self {
      i[0] = n + 1;
      return [n, self[n]];
    } else {
      return $iter_end;
    }
  };
}

fn reverse_list(self) {
  i = #self;
  if i == 0 {
    return [];
  }
  if i == 1 {
    return [self[0]];
  }

  result = [];
  while i > 0 {
    i = i - 1;
    result << self[i];
  }
  return result;
}

fn reverse_str(self) {
  i = #self;
  if i == 0 {
    return "";
  }
  if i == 1 {
    return self;
  }

  result = "";
  while i > 0 {
    i = i - 1;
    result << (self[i]);
  }
  return result;
}

fn split(self, delimiter) {
  result = [];
  current = "";
  delim_len = #delimiter;
  i = 0;

  if delim_len == 0 {
    for char in self {
      result << char;
    }
    return result;
  }

  while i < #self {
    match = true;
    j = 0;
    while j < delim_len {
      if i + j >= #self {
        match = false;
        j = delim_len;
      } else {
        if self[i + j] != delimiter[j] {
          match = false;
          j = delim_len;
        } else {
          j = j + 1;
        }
      }
    }

    if match {
      result << current;
      current = "";
      i = i + delim_len;
    } else {
      current << self[i];
      i = i + 1;
    }
  }

  result << current;
  return result;
}

fn contains(self, substring) {
  sub_len = #substring;

  if sub_len == 0 {
    return true;
  }

  if sub_len > #self {
    return false;
  }

  i = 0;
  while i <= #self - sub_len {
    match = true;
    j = 0;
    while j < sub_len {
      if self[i + j] != substring[j] {
        match = false;
        j = sub_len;
      } else {
        j = j + 1;
      }
    }

    if match {
      return true;
    }

    i = i + 1;
  }

  return false;
}

fn starts_with(self, prefix) {
  prefix_len = #prefix;

  if prefix_len == 0 {
    return true;
  }

  if prefix_len > #self {
    return false;
  }

  i = 0;
  while i < prefix_len {
    if self[i] != prefix[i] {
      return false;
    }
    i = i + 1;
  }

  return true;
}

fn ends_with(self, suffix) {
  suffix_len = #suffix;

  if suffix_len == 0 {
    return true;
  }

  if suffix_len > #self {
    return false;
  }

  self_len = #self;
  i = 0;
  while i < suffix_len {
    if self[self_len - suffix_len + i] != suffix[i] {
      return false;
    }
    i = i + 1;
  }

  return true;
}

fn is_whitespace(char) {
  return char == " " || char == "\t" || char == "\n" || char == "\r";
}

fn trim_left(self) {
  start = 0;
  while start < #self & is_whitespace(self[start]) {
    start = start + 1;
  }

  result = "";
  i = start;
  while i < #self {
    result << self[i];
    i = i + 1;
  }

  return result;
}

fn trim_right(self) {
  end = #self;
  while end > 0 & is_whitespace(self[end - 1]) {
    end = end - 1;
  }

  result = "";
  i = 0;
  while i < end {
    result << self[i];
    i = i + 1;
  }

  return result;
}

fn trim(self) {
  return self.trim_left().trim_right();
}

fn char_to_upper(char) {
  lowercase = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"];
  uppercase = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"];

  idx = lowercase.find(char);
  if idx != null {
    return uppercase[idx];
  }
  return char;
}

fn char_to_lower(char) {
  lowercase = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"];
  uppercase = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"];

  idx = uppercase.find(char);
  if idx != null {
    return lowercase[idx];
  }
  return char;
}

fn to_upper(self) {
  result = "";
  for char in self {
    result << char_to_upper(char);
  }
  return result;
}

fn to_lower(self) {
  result = "";
  for char in self {
    result << char_to_lower(char);
  }
  return result;
}

fn abs(self) {
  if self < 0 {
    return -self;
  }
  return self;
}

fn min(a, b) {
  if a < b {
    return a;
  }
  return b;
}

fn max(a, b) {
  if a > b {
    return a;
  }
  return b;
}

fn floor(self) {
  if type(self) == $int {
    return self;
  }

  if self >= 0 {
    return int(self);
  } else {
    int_part = int(self);
    if float(int_part) == self {
      return int_part;
    } else {
      return int_part - 1;
    }
  }
}

fn ceil(self) {
  if type(self) == $int {
    return self;
  }

  if self <= 0 {
    return int(self);
  } else {
    int_part = int(self);
    if float(int_part) == self {
      return int_part;
    } else {
      return int_part + 1;
    }
  }
}

fn round(self) {
  if type(self) == $int {
    return self;
  }

  if self >= 0 {
    return int(self + 0.5);
  } else {
    return int(self - 0.5);
  }
}

fn pow(self, exp) {
  if exp == 0 {
    return 1;
  }

  if exp < 0 {
    return 1.0 / pow(self, -exp);
  }

  if type(exp) == $float {
    if type(self) == $int {
      base = float(self);
    } else {
      base = self;
    }
    result = 1.0;
    e = exp;
    while e > 0 {
      result = result * base;
      e = e - 1;
    }
    return result;
  }

  result = 1;
  i = 0;
  while i < exp {
    result = result * self;
    i = i + 1;
  }

  if type(self) == $float {
    return float(result);
  }

  return result;
}

fn sqrt(self) {
  if self < 0 {
    return null;
  }

  if self == 0 {
    return 0;
  }

  x = float(self);
  guess = x / 2.0;
  epsilon = 0.00001;

  i = 0;
  while i < 100 {
    next_guess = (guess + x / guess) / 2.0;
    diff = next_guess - guess;
    if diff < 0 {
      diff = -diff;
    }

    if diff < epsilon {
      return next_guess;
    }

    guess = next_guess;
    i = i + 1;
  }

  return guess;
}

fn default_map_iter(self) {
  pairs = list(self);

  return pairs.iter();
}

fn find(self, value) {
  for pair in self.enumerate() {
    if pair[1] == value {
      return pair[0];
    }
  }
}

fn filter(self, condition) {
  result = [];

  for item in self {
    if condition(item) {
      result << item;
    }
  }

  return result;
}

fn join(self, join_str) {
  result = "";
  flag = true;

  for v in self {
    if flag {
      flag = false;
    } else {
      result << join_str;
    }

    result << str(v);
  }

  return result;
}

fn keys(self) {
  result = [];

  for pair in list(self) {
    result << pair[0];
  }

  return result;
}

fn values(self) {
  result = [];

  for pair in list(self) {
    result << pair[1];
  }

  return result;
}

fn contains_key(self, value) {
  return self.keys().find(value) != null;
}

fn map(self, f) {
  result = [];

  for v in self {
    result << f(v);
  }

  return result;
}

fn list_compact(self) {
  return self.filter(fn(v) { return v != null; });
}

fn map_compact(self) {
  result = {};

  for pair in self {
    if pair[1] != null {
      result[pair[0]] = pair[1];
    }
  }

  return result;
}

fn map_merge(self, other) {
  result = {};

  for pair in self {
    result[pair[0]] = pair[1];
  }

  for pair in other {
    result[pair[0]] = pair[1];
  }

  return result;
}

fn map_has_value(self, value) {
  for pair in self {
    if pair[1] == value {
      return true;
    }
  }
  return false;
}

fn map_filter(self, predicate) {
  result = {};

  for pair in self {
    if predicate(pair) {
      result[pair[0]] = pair[1];
    }
  }

  return result;
}

fn flatten(self) {
  result = [];

  for v in self {
    if type(v) == $list {
      for k in v.flatten() {
        result << k;
      }
    } else {
      result << v;
    }
  }

  return result;
}

fn list_slice(self, start, end) {
  result = [];
  actual_start = start;
  actual_end = end;

  if actual_start < 0 {
    actual_start = 0;
  }

  if actual_end > #self {
    actual_end = #self;
  }

  if actual_start >= actual_end {
    return result;
  }

  i = actual_start;
  while i < actual_end {
    result << self[i];
    i = i + 1;
  }

  return result;
}

fn list_concat(self, other) {
  result = [];
  for item in self {
    result << item;
  }
  for item in other {
    result << item;
  }
  return result;
}

fn list_reduce(self, f, initial) {
  acc = initial;
  for item in self {
    acc = f(acc, item);
  }
  return acc;
}

fn list_zip(self, other) {
  result = [];
  len = #self;
  other_len = #other;

  if other_len < len {
    len = other_len;
  }

  i = 0;
  while i < len {
    result << [self[i], other[i]];
    i = i + 1;
  }

  return result;
}

fn list_sort(self, comparator) {
  if #self <= 1 {
    return self;
  }

  result = [];
  for item in self {
    result << item;
  }

  n = #result;
  i = 0;
  while i < n - 1 {
    j = 0;
    while j < n - i - 1 {
      compare_result = null;
      if comparator != null {
        compare_result = comparator(result[j], result[j + 1]);
      } else {
        if result[j] > result[j + 1] {
          compare_result = 1;
        } else {
          compare_result = -1;
        }
      }

      if compare_result > 0 {
        temp = result[j];
        result[j] = result[j + 1];
        result[j + 1] = temp;
      }

      j = j + 1;
    }
    i = i + 1;
  }

  return result;
}

patch($list, $first, fn(self) {
  if #self > 0 {
    return self[0];
  }
  return null;
});

patch($list, $last, fn(self) {
  if #self > 0 {
    return self[#self - 1];
  }
  return null;
});

patch($list, $sum, fn(self) {
  total = 0;
  for item in self {
    total = total + item;
  }
  return total;
});

patch($list, $join, join);
patch($list, $iter, default_iter);
patch($list, $find, find);
patch($list, $enumerate, default_enumerate);
patch($list, $reverse, reverse_list);
patch($list, $map, map);
patch($list, $filter, filter);
patch($list, $compact, list_compact);
patch($list, $flatten, flatten);
patch($list, $slice, list_slice);
patch($list, $concat, list_concat);
patch($list, $reduce, list_reduce);
patch($list, $zip, list_zip);
patch($list, $sort, list_sort);

patch($str, $iter, default_iter);
patch($str, $enumerate, default_enumerate);
patch($str, $reverse, reverse_str);
patch($str, $split, split);
patch($str, $contains, contains);
patch($str, $starts_with, starts_with);
patch($str, $ends_with, ends_with);
patch($str, $trim, trim);
patch($str, $trim_left, trim_left);
patch($str, $trim_right, trim_right);
patch($str, $to_upper, to_upper);
patch($str, $to_lower, to_lower);

patch($map, $iter, default_map_iter);
patch($map, $keys, keys);
patch($map, $values, values);
patch($map, $contains_key, contains_key);
patch($map, $compact, map_compact);
patch($map, $merge, map_merge);
patch($map, $has_value, map_has_value);
patch($map, $filter, map_filter);

patch($fn, $iter, fn(self) { self; });

patch($int, $times, fn(self, callback) {
  i = 0;
  result = [];
  while i < self {
    result << (callback(i));
    i = i + 1;
  }
  result;
});

patch($int, $abs, abs);
patch($int, $floor, floor);
patch($int, $ceil, ceil);
patch($int, $round, round);
patch($int, $pow, pow);
patch($int, $sqrt, sqrt);

patch($float, $abs, abs);
patch($float, $floor, floor);
patch($float, $ceil, ceil);
patch($float, $round, round);
patch($float, $pow, pow);
patch($float, $sqrt, sqrt);
