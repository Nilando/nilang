// Snake Game - GPU Plugin
// Arrow keys to move, eat food to grow, avoid walls and yourself!

gpu = import("./examples/gpu_plugin/target/debug/libgpu_plugin.so");

// Cache GPU functions to prevent GC from collecting them
create_window = gpu["create_window"];
draw_rect = gpu["draw_rect"];
clear_screen = gpu["clear"];
update_screen = gpu["update"];
close_window = gpu["close"];
get_key = gpu["get_key"];
sleep_ms = gpu["sleep_ms"];

// Grid settings
grid_size = 40;
cell_size = 40;
window_size = grid_size * cell_size;

// Colors (0xRRGGBB as decimal)
color_bg = 2105376;        // 0x202020 dark gray background
color_snake = 3394611;     // 0x33CC33 green
color_head = 2263842;      // 0x228B22 darker green
color_food = 13369344;     // 0xCC0000 red

// Direction constants: 1=up, 2=down, 3=left, 4=right
dir = 4;

// LCG random number generator - use array so closures mutate by reference
rng = [12345];
fn lcg(seed) {
  a = 1664525;
  c = 1013904223;
  m = 2.pow(32);
  return ((a * seed + c) % m);
}

fn random_pos() {
  rng[0] = lcg(rng[0]);
  x = rng[0] % grid_size;
  rng[0] = lcg(rng[0]);
  y = rng[0] % grid_size;
  return [x, y];
}

// Initialize snake in the center, length 3, going right
snake = [];
snake << [10, 10];
snake << [9, 10];
snake << [8, 10];

// Place initial food
food = random_pos();

// Make sure food doesn't start on the snake
fn food_on_snake() {
  i = 0;
  while i < #snake {
    seg = snake[i];
    if seg[0] == food[0] {
      if seg[1] == food[1] {
        return true;
      }
    }
    i = i + 1;
  }
  return false;
}

while food_on_snake() {
  food = random_pos();
}

// Create window
create_window(window_size, window_size, "Snake");

game_over = false;
score = 0;
tick = 0;
frames_per_move = 4;

while !game_over {
  // 1. Poll input every frame - remember last directional press
  key = get_key();

  // Update direction, prevent 180-degree reversal
  if key == 1 {
    if dir != 2 {
      dir = 1;
    }
  } else {
    if key == 2 {
      if dir != 1 {
        dir = 2;
      }
    } else {
      if key == 3 {
        if dir != 4 {
          dir = 3;
        }
      } else {
        if key == 4 {
          if dir != 3 {
            dir = 4;
          }
        }
      }
    }
  }

  tick = tick + 1;

  // Only move the snake every N frames
  if tick >= frames_per_move {
    tick = 0;

    // 2. Calculate new head position
    head = snake[0];
    new_x = head[0];
    new_y = head[1];

    if dir == 1 {
      new_y = new_y - 1;
    } else {
      if dir == 2 {
        new_y = new_y + 1;
      } else {
        if dir == 3 {
          new_x = new_x - 1;
        } else {
          new_x = new_x + 1;
        }
      }
    }

    // 3. Check wall collision
    if new_x < 0 {
      game_over = true;
    }
    if new_x >= grid_size {
      game_over = true;
    }
    if new_y < 0 {
      game_over = true;
    }
    if new_y >= grid_size {
      game_over = true;
    }

    if !game_over {
      // 4. Check self collision
      i = 0;
      while i < #snake {
        seg = snake[i];
        if seg[0] == new_x {
          if seg[1] == new_y {
            game_over = true;
          }
        }
        i = i + 1;
      }
    }

    if !game_over {
      // 5. Move snake: insert new head at front
      new_head = [new_x, new_y];

      // Build new snake with new head first
      new_snake = [];
      new_snake << new_head;
      i = 0;
      while i < #snake {
        new_snake << snake[i];
        i = i + 1;
      }
      snake = new_snake;

      // 6. Check food collision
      ate_food = false;
      if new_x == food[0] {
        if new_y == food[1] {
          ate_food = true;
        }
      }

      if ate_food {
        score = score + 1;
        // Place new food
        food = random_pos();
        while food_on_snake() {
          food = random_pos();
        }
      } else {
        // Remove tail (pop last element)
        trimmed = [];
        i = 0;
        while i < (#snake - 1) {
          trimmed << snake[i];
          i = i + 1;
        }
        snake = trimmed;
      }
    }
  }

  if !game_over {
    // 7. Draw everything
    clear_screen(color_bg);

    // Draw food
    fx = food[0] * cell_size;
    fy = food[1] * cell_size;
    draw_rect(fx + 1, fy + 1, cell_size - 2, cell_size - 2, color_food);

    // Draw snake body
    i = 1;
    while i < #snake {
      seg = snake[i];
      sx = seg[0] * cell_size;
      sy = seg[1] * cell_size;
      draw_rect(sx + 1, sy + 1, cell_size - 2, cell_size - 2, color_snake);
      i = i + 1;
    }

    // Draw snake head
    h = snake[0];
    hx = h[0] * cell_size;
    hy = h[1] * cell_size;
    draw_rect(hx + 1, hy + 1, cell_size - 2, cell_size - 2, color_head);

    update_screen();

    // 8. Control frame rate (~60fps, move every 8 frames = ~133ms per move)
    sleep_ms(4);
  }
}

// Game over
print("Game Over! Score: {str(score)}");
close_window();
