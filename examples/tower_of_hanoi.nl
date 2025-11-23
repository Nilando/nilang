// Tower of Hanoi - Classic recursive puzzle solution
// Move n disks from source rod to destination rod using auxiliary rod
// Rules: Only one disk can be moved at a time, and a larger disk
// cannot be placed on top of a smaller disk

fn print_towers(state) {
  lines = [""];
  line = "-----------";
  key = " A | B | C ";

  lines << key;
  lines << line;

  i = 0;
  while true {
    a = i < #state["A"] && (state["A"][i] + 1);
    b = i < #state["B"] && (state["B"][i] + 1);
    c = i < #state["C"] && (state["C"][i] + 1);

    if !(a || b || c) {
      break;
    }

    lines << " {a || ' '} | {b || ' '} | {c || ' '}";

    i = i + 1;
  } 


  for line in lines.reverse() {
    print(line);
  }
}

fn inner_hanoi(n, source, destination, auxiliary, state) {

  if n == 1 {
    print("Move disk {n} from {source} to {destination}");

    disk = ^(state[source]);
    state[destination] << disk;

    print_towers(state);
    return;
  }

  // Move n-1 disks from source to auxiliary using destination
  inner_hanoi(n - 1, source, auxiliary, destination, state);

  // Move the nth disk from source to destination
  print("Move disk {n} from {source} to {destination}");
  disk = ^(state[source]);
  state[destination] << disk;

  print_towers(state);

  // Move n-1 disks from auxiliary to destination using source
  inner_hanoi(n - 1, auxiliary, destination, source, state);
}

fn hanoi(n) {
  disks = n.times(fn(i) { i; }).reverse();
  state = {
    "A": disks,
    "B": [],
    "C": [],
  };

  print_towers(state);

  inner_hanoi(n, "A", "C", "B", state);
}

// Solve Tower of Hanoi with 3 disks
print("Solving Tower of Hanoi with N disks:");
print("========================================");
print("enter disk number:");
hanoi(int(read));
