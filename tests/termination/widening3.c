/*
TERMINATION for x <= 0 || y > 0

suggested parameters:
- partition abstract domain = polyhedra
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
- SMART WIDENING
*/

int main() {
  int x, y;
  while (x > 0) {
    x = x - y;
  }
  return 0;
}