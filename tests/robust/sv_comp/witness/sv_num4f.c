//https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/blob/main/c/loops/count_up_down-2.c
int main()
{
  int n ; 
  int x ; 
  int y ;
  int bug ;
  bug = 0 ;
  y = 0 ; 
  
  x=n;
  while(x>0)
  {
    
    x--;
    y++;
  }
  if ( n==y ) {
    bug = 1;
  }
  return 0;
}


/*
// __VERIFIER_assert(y!=n);
*/