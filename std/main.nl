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

patch($str, $iter, default_iter);
patch($str, $enumerate, default_enumerate);
patch($str, $reverse, reverse_str);

patch($map, $iter, default_map_iter);
patch($map, $keys, keys);
patch($map, $values, values);
patch($map, $contains_key, contains_key);
patch($map, $compact, map_compact);

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
