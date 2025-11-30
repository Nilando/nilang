fn partition(A, lo, hi) {
  // Use median-of-three pivot selection for better performance
  mid = lo + ((hi - lo) / 2);

  // Sort lo, mid, hi and use middle value as pivot
  if A[lo] > A[mid] {
    t = A[lo];
    A[lo] = A[mid];
    A[mid] = t;
  }
  if A[lo] > A[hi] {
    t = A[lo];
    A[lo] = A[hi];
    A[hi] = t;
  }
  if A[mid] > A[hi] {
    t = A[mid];
    A[mid] = A[hi];
    A[hi] = t;
  }

  pivot = A[mid];

  i = lo - 1;
  k = hi + 1;

  while true {
    i = i + 1;
    k = k - 1;

    while A[i] < pivot {
      i = i + 1;
    }

    while A[k] > pivot {
      k = k - 1;
    }

    if i >= k {
      return k;
    }

    t = A[i];
    A[i] = A[k];
    A[k] = t;
  }
}

fn inner_quicksort(A, lo, hi) {
  if lo < hi {
    p = partition(A, lo, hi);
    inner_quicksort(A, lo, p);
    inner_quicksort(A, p + 1, hi);
  }
}

fn sort(numbers) {
  inner_quicksort(numbers, 0, #numbers - 1);
  return numbers;
}

unsorted_list = [3, 5, 9, 0, 8, 7, 4, 6, 2, 1];

print(unsorted_list);

sort(unsorted_list);

print(unsorted_list);
