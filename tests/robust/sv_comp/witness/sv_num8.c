
// https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/blob/svcomp22/c/loop-invariants/linear-inequality-inv-a.c
int main() {
  int n ;
  int s;  
  int v ; 
  int i ;
  int bug ; 
  bug = 0 ;
  if (n == 0) {
    return 0;
  }
  
  while (i < n) {
    v = ?;
    s += v;
    i++;
  }
  if (s < v) {
    bug = 1 ; 
    return 1;
  }
  if (s > 65025) {
    bug = 1; 
    return 1;
  }
  return 0;
}
