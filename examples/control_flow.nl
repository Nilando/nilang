

if true {
  print "things might be working!";
} else {
  print "ahh shucks";
}

a = 0;
while true {
  if a < 10000 {
    a = a * 2;
    continue;
  }

  a = a + 1;

  if a == 1000000 {
    break;
  }

  if a > 1000000 {
    print "shouldn't be possible to get here";
    return null;
  }
}

return "all done";

print "unreachable!";
