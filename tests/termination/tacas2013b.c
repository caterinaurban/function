/*
TERMINATION

suggested parameters:
- partition abstract domain = octagons/polyhedra
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x, K;
  while (x != K) {
    if (x > K) {
      x = x - 1;
    } else {
      x = x + 1;
    }
  }
  return 0;
}