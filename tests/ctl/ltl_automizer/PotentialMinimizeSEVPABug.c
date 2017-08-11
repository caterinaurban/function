// -ctl "AG{OR{x <= 0}{AF{y == 0}}}"
//#Safe
//@ ltl invariant positive: ([] ( AP(x > 0) ==> <>AP(y==0)));

extern int __VERIFIER_nondet_int();

int x,y;
	
int main() {
    while(1){
        x = __VERIFIER_nondet_int();
        y = 1;
        while (x>0) {
            x--;
            if (x <= 1) {
                y =0;
            }
        }
    }
}

