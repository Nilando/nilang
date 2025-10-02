@test = 0;

@a = fn() {
  print(@test);
  @test = @test + 1;
  if @test >= 10 {
    return;
  }
  @b();
};

@b = fn() {
  print(@test);
  @test = @test + 1;
  @c();
};

@c = fn() {
  print(@test);
  @test = @test + 1;
  @a();
};

@a();
