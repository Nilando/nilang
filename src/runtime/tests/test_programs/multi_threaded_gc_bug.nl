// This test fails when sandpit is in multi threaded mode, and I can't for the life 
// of me figure out why. Its a hard to debug race condition, at least I think it is.
//
// Example Error(this is not the only error that generates but the most common):
//
// ```
// thread '<unnamed>' panicked at /home/lando/Dev/sandpit/src/pointee.rs:95:9:
// Header pointer 0x5c757b68e0f2 is not aligned to 8 bytes (required for SliceHeader<nilang::runtime::func::LoadedLocal>)
// note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
// thread panicked, shutting down process
// ```
//
// My leading idea is that for some reason, the write barrier on runtime functions is 
// not working properly, and a use after free scenario is happening. The use after free manifests 
// as a memory issue of a pointer being unaligned, or invalid memory access, etc..
//
// I originally found this bug after creating the kaleidoscope example, and then got to
// this by slowly ripping out code from the example to try and distill what was causing 
// the error.

fn get_msg() {
  "test";
}

fn append_messages() {
  s = "";
  while true {
    s << get_msg();
  }
}

append_messages();
