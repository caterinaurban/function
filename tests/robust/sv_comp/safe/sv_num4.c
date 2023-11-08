//https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/blob/main/c/loops/count_up_down-2.c
int main()
{
  //int n ; 
  int x ; 
  int y ;
  int bug ; 
  y = 0 ; 
  //bug = 0 ;
  //if (n <  0) return 0 ; 
  x=100;
  while(x>0)
  {
    
    x--;
    y++;
  }; 
  if(y != 100) {
  bug = 1 ;
  }
  return 0;
}


/*
// __VERIFIER_assert(y==n);
*/