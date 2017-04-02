/* from Velroyen & Rummer TAP 2008 paper
TERMINATION for x <= 0

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x, up = 0;
  while (x > 0) {
    if (x == 1) {
      up = 1;
    }
    if (x == 10) {
      up = 0;
    }
    if (up == 1) {
      x = x + 1;
    } else {
      x = x - 1;
    }
  }
  return 0;
}