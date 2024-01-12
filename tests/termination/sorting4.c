/*
TERMINATION

suggested parameters:
- partition abstract domain = octagons/polyhedra
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
	int x1, x2, x3, x4;
	while (x1 > x2 || x2 > x3 || x3 > x4) {
		if (x1 > x2) {
			x1 = x2;
			x2 = x1;
		} else if (x2 > x3) {
			x2 = x3;
			x3 = x2;
		} else if (x3 > x4) {
			x4 = x3;
			x3 = x4;
		}
	}
	return 0;
}