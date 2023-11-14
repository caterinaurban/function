/* from Velroyen & Rummer TAP 2008 paper
TERMINATION for x < -5 OR 0 <= x <= 30 OR x > 35

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x;
  while (x != 0) {
    if (-5 <= x && x <= 35) {
      if (x < 0) {
        x = -5;
      } else {
        if (x > 30) {
          x = 35;
        } else {
          x = x - 1;
        }
      }
    } else {
      x = 0;
    }
  }
  return 0;
}