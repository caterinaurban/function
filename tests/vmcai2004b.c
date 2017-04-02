/*
TERMINATION for x != 3

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 4
*/

void main() {
  int x;
  while (x >= 0)
    x = - 2 * x + 9;
}