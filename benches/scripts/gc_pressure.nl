// GC-pressure benchmark: builds many linked lists then drops them.
// Each iteration allocates a fresh 1000-node list, sums it, then
// the list becomes garbage. Total: ~1M object allocations.

fn make_list(n) {
  result = null;
  while n > 0 {
    result = {val: n, next: result};
    n = n - 1;
  }
  return result;
}

fn list_sum(list) {
  total = 0;
  while list {
    total = total + list.val;
    list = list.next;
  }
  return total;
}

total = 0;
i = 0;
while i < 1000 {
  list = make_list(1000);
  total = total + list_sum(list);
  i = i + 1;
}

print("total: {str(total)}");
