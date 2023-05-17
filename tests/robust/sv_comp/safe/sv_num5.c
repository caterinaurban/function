
// https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/blob/svcomp22/c/loops/sum01-2.c
int main() { 
  int i;
  int n;
  int sn;
  int bug1 ; 
  int bug2;
  if ( n >= 0) return 0;
  sn = 0 ;
  
  bug1 = 2 * n ;
  bug2 = 0 ;
  
  
  for(i=0; i<=n; i++) {
    sn = sn + 2;
  }
  bug1 = sn ;
  bug2 = sn ;
}


/* __VERIFIER_assert(sn==n*2 || sn == 0); */


