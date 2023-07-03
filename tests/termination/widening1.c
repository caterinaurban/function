/* 
TERMINATION

suggested parameters:
- partition abstract domain = polyhedra
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
- SMART WIDENING
*/

void main() {
  int y, z;
  while (y >= 0 || z >= 0) {
    y--;
    z--;
  }
}