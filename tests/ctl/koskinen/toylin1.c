// *************************************************************
//
//     Branching-time reasoning for infinite-state systems
//
//              Byron Cook * Eric Koskinen
//                     July 2010
//
// *************************************************************

// -ctl_cfg "AF{resp > 5}"
// -precondition "c > 5"

int c; // assume c > 0
int servers = 4;
int resp = 0;
int curr_serv = servers;

void main() {
  while(curr_serv > 0) {
    if(?) {
      c--; curr_serv--;
      resp++;
    } else if (c < curr_serv) {
      curr_serv--;
    }
  }
  while(1) { int ddd; ddd=ddd; }
}

