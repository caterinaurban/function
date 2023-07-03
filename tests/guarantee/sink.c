/* from Urban Miné VMCAI 2015 paper
GUARANTEE/RECURRENCE (x == 0)

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = ordinals 1
- backward widening delay = 2 [default]
*/

void main() {
  int x;
  while (true) {
    x = ?;
    while (x != 0) {
      if (x > 0)
        x = x - 1;
      else
        x = x + 1;
    }
  }
}