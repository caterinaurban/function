/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
- SMART WIDENING
*/

void main() {
  int x;
  while (x != 0) {
    if (x > 0)
      x = x - 2;
    else
      x = x + 1;
  }
}
