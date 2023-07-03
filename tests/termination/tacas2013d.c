/* from Cook & See & Zuleger TACAS 2013 paper
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = ordinals 2
- backward widening delay = 3
*/

int main() {
  int x1, x2, x3;
  while (x1 > 0 && x2 > 0 && x3 > 0)
    if (?) {
      x1 = x1 - 1;
    } else if (?) {
      x2 = x2 - 1;
      x3 = ?;
    } else {
      x3 = x3 - 1;
      x1 = ?;  	
    }
  return 0;
}