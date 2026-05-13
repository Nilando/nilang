// Bubble sort a reverse-sorted array of N integers (worst case).
// Pure dispatch-loop workload — minimal language feature variety.

fn make_reverse(n) {
  result = [];
  i = n;
  while i > 0 {
    result << i;
    i = i - 1;
  }
  return result;
}

fn bubble_sort(xs) {
  n = #xs;
  i = 0;
  while i < n {
    j = 0;
    limit = n - 1 - i;
    while j < limit {
      if xs[j] > xs[j + 1] {
        t = xs[j];
        xs[j] = xs[j + 1];
        xs[j + 1] = t;
      }
      j = j + 1;
    }
    i = i + 1;
  }
  return xs;
}

n = 5000;
arr = make_reverse(n);
bubble_sort(arr);

print("first 5: {str(arr[0])} {str(arr[1])} {str(arr[2])} {str(arr[3])} {str(arr[4])}");
print("last 5: {str(arr[n - 5])} {str(arr[n - 4])} {str(arr[n - 3])} {str(arr[n - 2])} {str(arr[n - 1])}");
