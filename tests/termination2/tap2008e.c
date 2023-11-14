/* from Velroyen & Rummer TAP 2008 paper
TERMINATION for x <= 11 OR x >= 40

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x;
  while (x > 0 && x < 50) {
    if (x < 20) {
      x = x - 1;
    }
    if (x > 10) {
      x = x + 1;
    }
    if (30 <= x && x <= 40) {
      x = x - 1;
    }
  }
  return 0;
}