/*
TERMINATION

suggested parameters:
- conflict-driven conditional termination
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int get_step(int x) {
  if (x > 40) {
    return 5;
  } else if (x > 30) {
    return 4;
  } else if (x > 20) {
    return 3;
  } else if (x > 10) {
    return 2;
  }
  return 1;
}

int main()  {
  int y, x, step;
  x = y;
  while(y < -5 || y > 5) {
    if (x > 0) {
      y = y - get_step(x);
    } else {
      y = y + get_step(-x);
    }
  }
  return 0;
}