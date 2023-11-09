/* (adapted) from Cook & Podelski & Rybalchenko CACM 2009 paper
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
- refine
*/

int main() {
  int x, y;
  while (x > 0 && y > 0) {
      x = x - 1;
      y = y + 1;
  }
  return y;
}
