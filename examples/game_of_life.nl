// Linear Congruential Generator for random numbers
fn lcg(seed) {
  a = 1664525;
  c = 1013904223;
  m = 2.pow(32);

  return ((a * seed + c) % m);
}

fn new_grid(size, seed) {
  grid = [];
  current_seed = seed;

  row = 0;
  while row < size {
    vals = [];
    col = 0;
    while col < size {
      // Generate random number between 0 and 99
      current_seed = lcg(current_seed);
      random_val = current_seed % 100;

      // 30% chance of cell being alive
      vals << (random_val < 30);

      col = col + 1;
    }
    grid << vals;
    row = row + 1;
  }

  grid;
}

fn count_neighbors(grid, row, col) {
  count = 0;
  not_top = 0 < row;
  not_left = 0 < col;
  not_right = (col+1) < #grid;
  not_bottom = (row+1) < #grid;

  if not_top && grid[row - 1][col] {
      count = count + 1;
  }

  if not_top && not_left && grid[row - 1][col - 1] {
      count = count + 1;
  }

  if not_top && not_right && grid[row - 1][col + 1] {
      count = count + 1;
  }

  if not_left && grid[row][col - 1] {
      count = count + 1;
  }

  if not_right && grid[row][col + 1] {
      count = count + 1;
  }

  if not_bottom && grid[row + 1][col] {
      count = count + 1;
  }

  if not_bottom && not_left && grid[row + 1][col - 1] {
      count = count + 1;
  }

  if not_bottom && not_right && grid[row + 1][col + 1] {
      count = count + 1;
  }

  return count;
}

// 1. If a cell is alive it must have exactly 2 or 3 neighors alive
// to live, otherwise it dies.
// 2. If a cell is dead and has exactly 3 alive neighbors it becomes alive.
fn find_cells_to_update(grid) {
  result = { dead: [], new: [] };

  row = 0;
  while row < #grid {
    col = 0;
    while col < #grid {
      neighbors = count_neighbors(grid, row, col);
      cell_is_alive = grid[row][col];

      if !cell_is_alive && (neighbors == 3) {
        result.new << [row, col];
      }

      if cell_is_alive && ((neighbors != 2) && (neighbors != 3)) {
        result.dead << [row, col];
      }

      col = col + 1;
    }
    row = row + 1;
  }

  return result;
}

fn update_grid(grid) {
  cells_to_update = find_cells_to_update(grid);
  dead_cells = cells_to_update.dead;
  new_cells = cells_to_update.new;
  
  while #dead_cells {
      dead_cell = ^dead_cells;
      grid[dead_cell[0]][dead_cell[1]] = false;
  }

  while #new_cells {
      new_cell = ^new_cells;
      grid[new_cell[0]][new_cell[1]] = true;
  }
}

fn print_grid(grid) {
  s = "";
  for row in grid {
    for cell in row {
      if cell {
        s << "\033[32m██\033[0m";
      } else {
        s << "  ";
      }
    }
    s << "\n";
  }
  print(s);
}

fn run(grid) {
  while true {
    print_grid(grid);
    update_grid(grid);
  }
}

print("Enter a random seed:");
seed = int(read);

grid = new_grid(37, seed);

run(grid);
