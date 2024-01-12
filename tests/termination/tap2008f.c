/* from Velroyen & Rummer TAP 2008 paper
TERMINATION for x even

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x;
  while (x != 0) {
  	if (x < 0) {
  		x = x + 2;
  		if (x < 0) {
  			x = -x;			
  		}
  	} else {
  		x = x - 2;
  		if (x > 0) {
  			x = -x;
  		}
  	}
  }
  return 0;
}