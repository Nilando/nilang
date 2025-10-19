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

fn inner_quicksort(A, lo, hi) {
  if (lo >= 0) && (hi >= 0) && (lo < hi) {
    p = partition(A, lo, hi);
    inner_quicksort(A, lo, p);
    inner_quicksort(A, p + 1, hi);
  }
}

fn sort(numbers) {
  inner_quicksort(numbers, 0, #numbers - 1);
}

unsorted_list = [3, 5, 9, 0, 8, 7, 4, 6, 2, 1];

print(unsorted_list);

sort(unsorted_list);

print(unsorted_list);
