/* Euclide's Algorithm
TERMINATION for x = y || (x > 0 && y > 0)

suggested parameters:
- partition abstract domain = polyhedra
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
- SMART WIDENING
*/

int main() {
  int x, y;
  while (x != y)
    if (x > y)
      x = x - y;
    else
      y = y - x;
  return x;
}
