/*
TERMINATION for x > 10 OR (x <= 10 AND x odd) 

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x;
  while (x <= 10) {
    if (x <= 9) {
      x = x + 2;
    }
  }
  return 0;
}