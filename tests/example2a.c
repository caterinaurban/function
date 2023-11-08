w/*
TERMINATION

suggested parameters:
- conflict-driven conditional termination
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int y;
  while (y< -1 || y > 1) {
    if (y < 0) {
      y = y + 1;
    } else {
      y = y - 1;
    }
  }
  return 0;
}