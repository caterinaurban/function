int main() { 
  int i ; 
  int j ; 
  int n ; 
  int sn;
  int bug;
  j = 10; 
  sn = 0 ; 
  bug = 0 ; 
  if ( n < 0) {
    return 0;
  }
  for(i=0; i<n; i++) {
    if (i<j)  sn = sn + 2;
    j--;
  }
  if( sn != n*2 && sn != 0 )  bug = 1;
//  __VERIFIER_assert(sn==((long long) n)*a || sn == 0);
}
