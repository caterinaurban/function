/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main(int x) {
	if (x <= 0) {
		return 0;
	} else {
		main(x - 1);
	}
}