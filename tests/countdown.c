/* from Urban Miné VMCAI 2015 paper
GUARANTEE/RECURRENCE (x == 0)

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

void main() {
  int x, c = 1;
  while (true) {
    x = c;
    while (x > 0) {
      x = x - 1;
      c = c + 1;
    }
  }
}