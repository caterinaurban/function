/*
TERMINATION

suggested parameters:
- conflict-driven conditional termination
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x, y;
  while (y< -2 || y > 2) {
    if (y < 0 && x > 10) {
      y = y + 2;
    } else if (y < 0 && x <= 10){
      y = y + 1;
    } else if (y >= 0 && x > 10){
      y = y - 2;
    } else if (y >= 0 && x <= 10){
      y = y - 1;
    } 
  }
  return 0;
}