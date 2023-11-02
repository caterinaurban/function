/* from Massé VMCAI 2014 paper
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 7
*/

int main() {
  int x = ?;
  while (x <= 100) {
    if (?)
      x = -2 * x + 2;
    else
      x = -3 * x - 2;
  }
  return 0;
}