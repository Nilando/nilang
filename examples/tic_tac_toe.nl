fn print_row(row, board) {
  a = board[row][0] || " ";
  b = board[row][1] || " ";
  c = board[row][2] || " ";

  row = str(row);
  row.push(" ║ ");
  row.push(a);
  row.push(" ║ ");
  row.push(b);
  row.push(" ║ ");
  row.push(c);
  row.push(" ║");

  print(row);
}

fn print_board(board) {
  print("  ╔═══╦═══╦═══╗");
  print_row(2, board);
  print("  ╠═══╬═══╬═══╣");
  print_row(1, board);
  print("  ╠═══╬═══╬═══╣");
  print_row(0, board);
  print("  ╚═══╩═══╩═══╝");
  print("    0   1   2  ");
}

fn print_move_help() {
  print("Enter your move like: x, y");
  print("Example: 0 1");
}

fn read_move() {
  while true {
    move = read.trim().split(" ");
    if move.len() != 2 {
      print_move_help();
      continue;
    }

    col = num(move[0]);
    row = num(move[1]);

    if (col == null) || (row == null) {
      print_move_help();
    } else {
      return [row, col];
    }
  }
}

fn new_board() {
  return [
    [null, null, null],
    [null, null, null],
    [null, null, null],
  ];
}

fn play_move(row, col, board, move) {
  if (row > 2) || (row < 0) || (col > 2) || (col < 0) {
    print("move must be 0, 1, or 2");
    return false;
  }

  if board[row][col] != null {
    print("that space is taken");
    return false;
  }

  board[row][col] = move;
}

fn is_row_complete(board, row) {
  return (board[row][0] == board[row][1])
    && (board[row][0] == board[row][2])
    && (board[row][0] != null);
}

fn is_col_complete(board, col) {
  return (board[0][col] == board[1][col])
    && (board[0][col] == board[2][col])
    && (board[0][col] != null);
}

fn game_over(board) {
  i = 0;

  while i < 3 {
    if is_row_complete(board, i) || is_col_complete(board, i) {
      return true;
    }

    i = i + 1;
  }

  if board[1][1] == null {
    return false;
  }

  return ((board[0][0] == board[1][1]) && (board[0][0] == board[2][2]))
    || ((board[2][0] == board[1][1]) && (board[2][0] == board[0][2]));
}


fn game_loop() {
  board = new_board();
  move = "x";

  while true {
    print_board(board);

    if game_over(board) {
        print("Game over!");
        return;
    }

    move_cord = read_move();

    play_move(move_cord[0], move_cord[1], board, move);

    if move != "x" {
      move = "x";
    } else {
      move = "y";
    }
  }
}

fn ask_to_play() {
  print("Ready to Play? (y/n)");

  answer = read;

  if (answer == "y") || (answer == "yes") {
    game_loop();
  } else {
    print("Okay, Goodbye!");
  }
}

ask_to_play();
