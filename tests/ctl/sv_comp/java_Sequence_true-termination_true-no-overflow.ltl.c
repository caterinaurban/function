//#Safe
//@ ltl invariant positive: <>(<>AP(j >= 21) && AP(i==100));

typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int j;
int i;
int c;

int main() {

    
    c = 0;
    i = 0;
    while (i < 100) {
        c = c + 1;
        i = i + 1;
    }
    j = 5;
    while (j < 21) {
        c = c + 1;
        j = j + 3;
    }
    return 0;
}
