//#Safe
//@ ltl invariant positive: (AP(i >= 5) || <>AP(exit = 1));

typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int i;
int exit;

int main() {

    //i = __VERIFIER_nondet_int();
    
    while (i > 0) {
        if (i != 5) {
            i = i-1;
        }
    }
    exit = 1;
    return 0;
}

