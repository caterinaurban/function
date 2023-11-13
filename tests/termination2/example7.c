/*
TERMINATION for x > 6

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x;
  while (x <= 10) {
    if (x > 6) {
      x = x + 2;
    }
  }
  return 0;
}