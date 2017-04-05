/* from Velroyen & Rummer TAP 2008 paper
TERMINATION for x < 30

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x, brake = 0;
  while (x > 10 && brake != 1) {
    if (x > 20) {
      x = x + 1;
    } else {
      x = x - 1;
    }
    if (x == 30) {
      brake = 1;
    }
  }
  return 0;
}