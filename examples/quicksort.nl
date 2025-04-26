fn quicksort(numbers) {
  inner_quicksort(numbers, 0, len(numbers) - 1);
}

fn inner_quicksort(A, lo, hi) {
  if (lo >= 0) && (hi >= 0) && (lo < hi) {
    p = partition(A, lo, hi);
    inner_quicksort(A, lo, p);
    inner_quicksort(A, p + 1, hi);
  }
}

fn partition(A, lo, hi) {
  pivot = A[lo];

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

unsorted_list = [6, 3, 5, 9, 0, 8, 7, 4, 6, 2, 1];

print(quicksort(unsorted_list)); // [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
