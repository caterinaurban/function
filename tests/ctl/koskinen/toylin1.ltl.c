//#Safe
//@ ltl invariant positive: (AP(c <= 5) || <>AP(resp > 5) );

extern int __VERIFIER_nondet_int() __attribute__ ((__noreturn__));


int c;
int servers = 4;
int resp = 0;
int curr_serv = 4;

void main() {
  while(curr_serv > 0) {
    if(__VERIFIER_nondet_int()) {
      c--; curr_serv--;
      resp++;
    } else if (c < curr_serv) {
      curr_serv--;
    }
  }
  while(1) { int ddd; ddd=ddd; }
}

