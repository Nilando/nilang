fn h() {
  msg = "test";
  return msg;
}
fn g() {
  h();
}
fn f() {
  g();
}
fn e() {
  f();
}
fn d() {
  e();
}
fn c() {
  d();
}
fn b() {
  c();
}
fn a() {
  b();
}

100.times(fn(x) {
  msg = a();
  print("{x} : {msg}");
});
