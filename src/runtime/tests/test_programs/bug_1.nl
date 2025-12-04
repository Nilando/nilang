// NOTES: Running this script more than half the time causes a bus error
// I believe this error is orginiating somewhere from bad logic in the sandpit
// garbage collector, but I have not diagnosed exactly what the bug is, or what
// part of this code is causing the the bug to manifest.
// 
// This error was first found in the kaleidoscope example, but
// instead of that script crashing with a bus error, it would often crash with
// an error about a pointer to a header not being aligned. Also sometimes this
// exaple crashes due to an out of bounds index in an instruction stream. There
// may be multiple bugs at play here, but my suspicion is there is one bug that
// is the root cause of all of these various failures.

// Intestingly, I have noticed that running this code sometimes works fine, and
// if the flag is set to not load the standard library the code also runs fine.
// I'm quite puzzled as to why not loading the standard library would make any
// difference here, but for some reason the flakeyness of the failure makes me
// more certain about this being a bug in the garbage collector. Many garbage
// collector bugs in the past have been found to be flakey due to the multi
// threaded race conditions which may only be hit by random chance of timing.
//
// To debug this further, I have the sandpit library being loaded locally so
// that I can insert print statements to see where exactly the bus error occurs.
// 
// It seems that the error is not occuring in a tracer thread, as enabling
// GC_DEBUG there is no trace start message that occurs before the bus error is
// hit, so it appears that the bus error is being caused somewhere in the
// runtime, but likely b/c of some invalid mark/trace that the GC did to corrupt
// a pointer or piece of memory


// Solution: the write barrier for gc vec was wrong. GcVec doesn't initialize
// the full underlying array, but its write barrier would cause the full
// underlying array to be retraced. Fixing this made the bug go away, but I
// still have no idea what it is about this program that was causing that bug
// (which is a race condition) to manifest. Weird!

fn foo(x) {
  if x == 0 {
    print("0");
  } else {
    if x == 1 {
      print("1");
    } else {
      if x == 2 {
        print("2");
      } else {
        if x == 3 {
          print("3");
        }
      }
    }
  }
}

i = 0;
while i < 1000 {
 foo(i);
 i = i + 1;
}
