/* from Cook Podelski Rybalchenko CACM 2009 paper
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = ordinals 1
- backward widening delay = 3
*/

void main() {
  int x, y;
  while (x > 0 && y > 0)
    if (?) {
      x = x - 1;
      y = ?;
    } else
      y = y - 1;
}