fn new_grid(size) {
  grid = [];

  row = 0;
  while row < size {
    vals = [];
    col = 0;
    while col < size {
      // just doing something random to try and get something to happen
      vals << ((row * col) % 1023) == 0;
      col = col + 1;
    }
    grid << vals;
    row = row + 1;
  }

  return grid;
}

fn count_neighbors(grid, row, col) {
  count = 0;
  not_top = 0 < row;
  not_left = 0 < col;
  not_right = (col+1) < len(grid);
  not_bottom = (row+1) < len(grid);

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
  while row < len(grid) {
    col = 0;
    while col < len(grid) {
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
  
  while len(dead_cells) != 0 {
      dead_cell = pop(dead_cells);
      grid[dead_cell[0]][dead_cell[1]] = false;
  }

  while len(new_cells) != 0 {
      new_cell = pop(new_cells);
      grid[new_cell[0]][new_cell[1]] = true;
  }
}

fn print_grid(grid) {
  row = 0;
  while row < len(grid) {
    s = "";
    col = 0;
    while col < len(grid) {
      if grid[row][col] {
        s << "██";
      } else {
        s << "  ";
      }
      col = col + 1;
    }
    print(s);
    row = row + 1;
  }
}

fn run(grid) {
  i = 0;
  while i <= 50 {
    update_grid(grid);
    print_grid(grid);
    i = i + 1;
  }
}

grid = new_grid(38);

run(grid);
