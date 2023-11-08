// *************************************************************
//
//     Branching-time reasoning for infinite-state systems
//
//              Byron Cook * Eric Koskinen
//                     July 2010
//
// *************************************************************

// Benchmark: acqrel.c
// Property: AG(a => AF r)

// FuncTion arguments:
// -ctl "AG{OR{A!=1}{AF{R==1}}}" 
// -precondition "A==0 && R==0"


int A = 0;
int R = 0;

void main() {
  int n;
  while(?) {
    A = 1;
    A = 0;
    n = ?;
    while(n>0) {
      n--;
    }
    R = 1;
    R = 0;
  }
  while(1) { int x; x=x; }
}

