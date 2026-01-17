// Kaleidoscope - Generate colorful symmetric patterns
// Creates beautiful patterns with 4-way symmetry

// Linear Congruential Generator for random numbers
fn lcg(seed) {
  a = 1664525;
  c = 1013904223;
  m = 2.pow(32);

  return ((a * seed + c) % m);
}

fn new_grid(size) {
  grid = [];

  row = 0;
  while row < size {
    vals = [];
    col = 0;
    while col < size {
      vals << 0;
      col = col + 1;
    }
    grid << vals;
    row = row + 1;
  }

  grid;
}

fn generate_kaleidoscope(grid, seed) {
  size = #grid;
  half_size = size / 2;
  current_seed = seed;

  // Generate one quadrant, then mirror to create symmetry
  row = 0;
  while row < half_size {
    col = 0;
    while col < half_size {
      // Generate random color (0-7)
      current_seed = lcg(current_seed);
      color = (current_seed % 100) / 12;  // 0-7 range

      // Apply color to all 4 quadrants with symmetry
      // Top-left
      grid[row][col] = color;
      // Top-right (mirror horizontally)
      grid[row][size - 1 - col] = color;
      // Bottom-left (mirror vertically)
      grid[size - 1 - row][col] = color;
      // Bottom-right (mirror both)
      grid[size - 1 - row][size - 1 - col] = color;

      col = col + 1;
    }
    row = row + 1;
  }
}

fn get_color_code(color) {
  code = "\033[90m";  // Default dark gray

  if color == 0 {
    code = "\033[90m";  // Dark gray
  } else {
    if color == 1 {
      code = "\033[91m";  // Bright red
    } else {
      if color == 2 {
        code = "\033[93m";  // Bright yellow
      } else {
        if color == 3 {
          code = "\033[92m";  // Bright green
        } else {
          if color == 4 {
            code = "\033[96m";  // Bright cyan
          } else {
            if color == 5 {
              code = "\033[94m";  // Bright blue
            } else {
              if color == 6 {
                code = "\033[95m";  // Bright magenta
              } else {
                code = "\033[97m";  // Bright white
              }
            }
          }
        }
      }
    }
  }

  return code;
}

fn print_grid(grid) {
  s = "";
  for row in grid {
    for cell in row {
      color_code = get_color_code(cell);
      s << color_code;
      s << "██";
      s << "\033[0m";
    }
    s << "\n";
  }
  print(s);
}

fn animate_kaleidoscope(grid, initial_seed, frames) {
  current_seed = initial_seed;
  frame = 0;

  while frame < frames {
    generate_kaleidoscope(grid, current_seed);
    print_grid(grid);

    // Generate next seed for animation
    current_seed = lcg(current_seed);

    frame = frame + 1;
  }
}

print("Kaleidoscope - Symmetric Colorful Patterns");
print("Enter a random seed:");
seed = int(read);
print("");

size = 30;
grid = new_grid(size);

// Generate and display a single kaleidoscope
generate_kaleidoscope(grid, seed);
print_grid(grid);

print("");
print("Press Enter to see animated kaleidoscopes...");
read;

// Animate with evolving patterns
animate_kaleidoscope(grid, seed, 50);

print("");
print("Kaleidoscope complete!");
