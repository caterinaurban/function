/* from Urban Miné SAS 2014 paper
TERMINATION for r <= 0 || x < y

suggested parameters:
- partition abstract domain = octagons/polyhedra
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
- SMART WIDENING
*/

void main() {
  int x, y, r;
  while (r > 0) {
    r = r + x;
    r = r - y;
  }
}
