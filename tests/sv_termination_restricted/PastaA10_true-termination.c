typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int x;
    int y;
    // x = __VERIFIER_nondet_int();
    // y = __VERIFIER_nondet_int();
    
    while (x != y) {
        if (x > y) {
            y = y+1;
        } else {
            x = x+1;
        }
    }
    
    return 0;
}
