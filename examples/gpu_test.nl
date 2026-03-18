gpu = import("./examples/gpu_plugin/target/debug/libgpu_plugin.so");

gpu["create_window"](800, 600, "Nilang GPU Test");

// Box positions
rx = 50;
gx = 300;
bx = 550;

// Velocities
rv = 3;
gv = -2;
bv = 4;

frame = 0;
while true {
    // Clear to dark blue
    gpu["clear"](1711150);

    // Draw three bouncing rectangles
    gpu["draw_rect"](rx, 200, 100, 100, 16711680);
    gpu["draw_rect"](gx, 250, 100, 100, 65280);
    gpu["draw_rect"](bx, 300, 100, 100, 255);

    gpu["update"]();

    // Move boxes
    rx = rx + rv;
    gx = gx + gv;
    bx = bx + bv;

    // Bounce off walls
    if rx < 0 {
        rv = 0 - rv;
        rx = 0;
    }
    if rx > 700 {
        rv = 0 - rv;
        rx = 700;
    }
    if gx < 0 {
        gv = 0 - gv;
        gx = 0;
    }
    if gx > 700 {
        gv = 0 - gv;
        gx = 700;
    }
    if bx < 0 {
        bv = 0 - bv;
        bx = 0;
    }
    if bx > 700 {
        bv = 0 - bv;
        bx = 700;
    }

    frame = frame + 1;
}

gpu["close"]();
