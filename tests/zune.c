/* http://techcrunch.com/2008/12/31/zune-bug-explained-in-detail/

suggested parameters:
- conflict-driven analysis 3 
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int days;
  int year = 1980;
  while (days > 365) {
    if ( isLeapYear(year) ) {
      if (days > 366) {
        days -= 366;
        year += 1;
      }
    } else {
      days -= 365;
      year += 1;
    }
  }
  return 0;
}



