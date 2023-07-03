/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
- conflict-driven analysis = 1
*/

int main() {
  int x, y;
  if (y < 0 && y % 2 == 0) {
    x = 2;
  } else if (y < 0) {
    x = 1;
  } else if (y >= 0 && y % 2 == 0) {
    x = -2;
  } else {
    x = -1;
  }
  while (y < -2 || y > 2) {
    y = y + x;
  }
  return 0;
}
