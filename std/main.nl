fn default_iter(self) {
  i = [0];

  return fn() {
    n = i[0];
    if n < #self {
      i[0] = n + 1;
      return self[n];
    } else {
      return null;
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
      return null;
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
    result << self[i];
  }
  return result;
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

patch($list, $join, join);
patch($list, $iter, default_iter);
patch($list, $find, find);
patch($list, $enumerate, default_enumerate);
patch($list, $reverse, reverse_list);
patch($list, $map, map);
patch($list, $filter, filter);

patch($str, $iter, default_iter);
patch($str, $enumerate, default_enumerate);
patch($str, $reverse, reverse_str);

patch($map, $iter, default_map_iter);
patch($map, $keys, keys);
patch($map, $values, values);
patch($map, $contains_key, contains_key);

patch($fn, $iter, fn(self) { return self; });
