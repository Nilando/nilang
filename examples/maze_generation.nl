// Maze Generation using Recursive Backtracking
// Creates a perfect maze (all cells connected, no loops)

// Global seed for random number generation
seed = 12345;

// Multiplicative congruential generator
fn random(min, max) {
  // Constants from Numerical Recipes
  a = 1103515245;
  c = 12345;
  m = 2147483648;  // 2^31

  // Update seed
  seed = ((a * seed + c) % m);

  // Make positive if negative
  if seed < 0 {
    seed = seed + m;
  }

  // Extract high-order bits by dividing before modulo
  // This gives better distribution for small ranges
  range = max - min + 1;
  bucket_size = m / range;

  if bucket_size > 0 {
    result = seed / bucket_size;
    if result >= range {
      result = range - 1;
    }
    return min + result;
  }

  // Fallback for very large ranges
  return min + (seed % range);
}

// Shuffle a list using Fisher-Yates algorithm
fn shuffle(list) {
  i = #list - 1;
  while i > 0 {
    j = random(0, i);

    // Swap elements
    temp = list[i];
    list[i] = list[j];
    list[j] = temp;

    i = i - 1;
  }
  return list;
}

fn create_grid(width, height) {
  height.times(fn(h) {
    row = [];
    width.times(fn(w) {
      row << true;
    });
    row;
  });
}

fn display_maze(grid) {
  top_border = "";
  width = #grid[0];
  height = #grid;

  // add 2 to account for left & right walls
  (width + 2).times(fn(i) {
    top_border << "██";
  });

  print(top_border);

  for row in grid.enumerate() {
    row_str = "██";
    for cell in row[1].enumerate() {
      if row[0] == width - 1 && cell[0] == height - 1 {
          row_str << "EE";
          continue;
      }
      if cell[0] == 0 && row[0] == 0 {
        row_str << "SS";
        continue;
      }

      if cell[1] {
        row_str << "██";
      } else {
        row_str << "  ";
      }
    }
    row_str << "██";
    print(row_str);

  }

  print(top_border);

}

fn count_empty_neighbors(cell, grid) {
  row = cell[0];
  col = cell[1];
  not_top = 0 < row;
  not_left = 0 < col;
  not_right = (col+1) < #grid[0];
  not_bottom = (row+1) < #grid;
  count = 0;

  if not_top && !grid[row - 1][col] {
    count = count + 1;
  }

  if not_left && !grid[row][col - 1] {
    count = count + 1;
  }

  if not_right && !grid[row][col + 1] {
    count = count + 1;
  }

  if not_bottom && !grid[row + 1][col] {
    count = count + 1;
  }

  return count;
}

fn get_wall_neighbors(cell, grid) {
  row = cell[0];
  col = cell[1];
  not_top = 0 < row;
  not_left = 0 < col;
  not_right = (col+1) < #grid[0];
  not_bottom = (row+1) < #grid;
  result = [];

  if not_top && grid[row - 1][col] {
      result << [row - 1, col];
  }

  if not_left && grid[row][col - 1] {
      result << [row, col - 1];
  }

  if not_right && grid[row][col + 1] {
      result << [row, col + 1];
  }

  if not_bottom && grid[row + 1][col] {
      result << [row + 1, col];
  }

  return result;
}

fn carve_passages(grid) {
  grid[0][0] = false;

  walls = [[0,1], [1, 0]];

  while #walls  {
    i = random(0, #walls - 1);
    wall_cell = walls.swap_remove(i);
    empty_neighbors = count_empty_neighbors(wall_cell, grid);
    if empty_neighbors != 1 { 
      continue; 
    }

    grid[wall_cell[0]][wall_cell[1]] = false;

    wall_neighbors = get_wall_neighbors(wall_cell, grid);
    for cell in wall_neighbors {
      walls << cell;
    }
  }
}

fn main() {
  print("=================================");
  print("    Maze Generator");
  print("=================================");
  print("");
  print("Enter maze width (try 10-20):");
  width_str = read.trim();
  width = int(width_str);

  print("Enter maze height (try 10-20):");
  height_str = read.trim();
  height = int(height_str);

  print("Enter random seed:");
  seed_str = read.trim();
  input_seed = int(seed_str);

  if width == null || height == null || input_seed == null {
    print("Invalid input!");
    return;
  }

  // Update global seed
  seed = input_seed;

  if width < 2 || height < 2 {
    print("Maze must be at least 2x2!");
    return;
  }

  print("");
  print("Generating {width}x{height} maze...");

  grid = create_grid(width, height);
  start_cell = [0, 0];
  carve_passages(grid);

  display_maze(grid);

  print("Done! The maze starts at the top-left.");
}

main();
