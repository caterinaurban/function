// -domain polyhedra
// -ctl "AND{EF{x == 2}}{EF{x==3}}"
// TODO: fix, now it only works with -ast for some reason!

int main() {
  int x;
  if (?) {
    x = 2;
  } else {
    x = 3;
  }
  return 0;
}
