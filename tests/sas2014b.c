/* from Urban Miné SAS 2014 paper
TERMINATION

suggested parameters:
- partition abstract domain = polyhedra
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

void main() {
  int x, y;
  while (x > 0 && y > 0) {
    x = x - y;
  }
}