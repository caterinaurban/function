/* from Urban Miné VMCAI 2015 paper
GUARANTEE (x == 3) for x <= 3
RECURRENCE (x == 3) for x < 0

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 3
*/

void main() {
  int x;
  while (x >= 0) {
    x = x + 1;
  }
  while (true) {
    if (x < 10) {
      x = x + 1;
    } else {
      x = -x;
    }
  }
}