/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x;
  while (x <= 10) 
    x = x + 1;
  return 0;
}