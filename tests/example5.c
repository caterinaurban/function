/*
TERMINATION for x > 0

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 5
*/

int main() {
  int x;
  while (x < 10)
    x = 2 * x;
  return 0;
}