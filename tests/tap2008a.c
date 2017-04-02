/* from Velroyen & Rummer TAP 2008 paper
TERMINATION for x < 25 OR x > 30

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x;
  while (x > 10) {
    if (x == 25) {
      x = 30;
    }
    if (x <= 30) {
      x = x - 1;
    } else {
      x = 20;
    }
  }
  return 0;
}