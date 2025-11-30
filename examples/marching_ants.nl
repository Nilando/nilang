// Marching Ants - Extended Langton's Ant with multiple colors
// The ant moves on a grid and changes cell colors based on rules
//
// Rules (LLRR pattern):
// State 0 (empty): Turn Left, change to state 1
// State 1 (red): Turn Left, change to state 2
// State 2 (yellow): Turn Right, change to state 3
// State 3 (green): Turn Right, change to state 0
//
// Directions: 0=North, 1=East, 2=South, 3=West

fn new_grid(size) {
  grid = [];

  row = 0;
  while row < size {
    vals = [];
    col = 0;
    while col < size {
      vals << 0;  // All cells start empty
      col = col + 1;
    }
    grid << vals;
    row = row + 1;
  }

  grid;
}

fn turn_left(direction) {
  return (direction + 3) % 4;
}

fn turn_right(direction) {
  return (direction + 1) % 4;
}

fn move_forward(ant, size) {
  new_ant = {
    row: ant.row,
    col: ant.col,
    dir: ant.dir
  };

  if ant.dir == 0 {
    // North
    new_ant.row = (ant.row - 1 + size) % size;
  } else {
    if ant.dir == 1 {
      // East
      new_ant.col = (ant.col + 1) % size;
    } else {
      if ant.dir == 2 {
        // South
        new_ant.row = (ant.row + 1) % size;
      } else {
        // West
        new_ant.col = (ant.col - 1 + size) % size;
      }
    }
  }

  return new_ant;
}

fn update(grid, ant) {
  current_state = grid[ant.row][ant.col];
  new_direction = ant.dir;
  new_state = 0;

  if current_state == 0 {
    // Empty -> Red, Turn Left
    new_state = 1;
    new_direction = turn_left(ant.dir);
  } else {
    if current_state == 1 {
      // Red -> Yellow, Turn Left
      new_state = 2;
      new_direction = turn_left(ant.dir);
    } else {
      if current_state == 2 {
        // Yellow -> Green, Turn Right
        new_state = 3;
        new_direction = turn_right(ant.dir);
      } else {
        // Green -> Empty, Turn Right
        new_state = 0;
        new_direction = turn_right(ant.dir);
      }
    }
  }

  grid[ant.row][ant.col] = new_state;

  new_ant = {
    row: ant.row,
    col: ant.col,
    dir: new_direction
  };

  return move_forward(new_ant, #grid);
}

fn print_grid(grid, ant) {
  s = "";
  row = 0;
  while row < #grid {
    col = 0;
    while col < #grid {
      cell = grid[row][col];
      is_ant = (row == ant.row) && (col == ant.col);

      if is_ant {
        // Ant position - Bright White
        s << "\033[97m██\033[0m";
      } else {
        if cell == 0 {
          // Empty - Dark gray
          s << "\033[90m░░\033[0m";
        } else {
          if cell == 1 {
            // State 1 - Red
            s << "\033[31m██\033[0m";
          } else {
            if cell == 2 {
              // State 2 - Yellow
              s << "\033[33m██\033[0m";
            } else {
              // State 3 - Green
              s << "\033[32m██\033[0m";
            }
          }
        }
      }

      col = col + 1;
    }
    s << "\n";
    row = row + 1;
  }
  print(s);
}

fn run(grid, ant) {
  current_ant = ant;
  i = 0;
  while true {
    if i % 1000 == 0 {
      print_grid(grid, current_ant);
    }
    current_ant = update(grid, current_ant);
    i = i + 1;
  }
}

print("Marching Ants - Colorful Langton's Ant");
print("Watch the ant create complex patterns!");
print("");

size = 40;
grid = new_grid(size);

// Start ant in the center facing north
ant = {
  row: size / 2,
  col: size / 2,
  dir: 0
};

run(grid, ant);
