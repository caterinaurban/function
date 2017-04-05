/* from Gopan & Reps CAV 2006 paper
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 3
*/

void main() {
  int x, y;
  while (x >= 0) {
    if (y <= 50)
      x = x + 1;
    else
      x = x - 1;
    y = y + 1;
  }
}