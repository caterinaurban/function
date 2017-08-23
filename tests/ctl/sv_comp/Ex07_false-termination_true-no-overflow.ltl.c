//#Safe
//@ ltl invariant positive: <>([]AP(i==0));

typedef enum { false, true } bool;

extern int __VERIFIER_nondet_int(void);

int i;


int main() {

    while (true) {
        if (i > 0) {
            i = i-1;
        }
        if (i < 0) {
            i = i+1;
        }
    }
    
    return 0;
}
