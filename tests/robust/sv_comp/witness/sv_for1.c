

// https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/blob/main/c/loops/sum01_bug02.c
int main() { 
  int x; 
  int bug1;
  bug1 = 0;
  x = 0;
  while(true)
  {
    x ++ ;
    if( x > 1000 ) bug1 = 1;
    
  }
  
}

