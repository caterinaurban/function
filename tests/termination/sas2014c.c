/* from Urban Miné SAS 2014 paper
TERMINATION for x <= 0 OR y > 0

suggested parameters:
- partition abstract domain = polyhedra
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
- SMART WIDENING
*/

void main() {
  int x, y;
  while (x > 0) {
    x = x - y;
  }
}