typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int x;
    int y;
   // x = __VERIFIER_nondet_int();
    
    while (x >= 0) {
        y = 1;
        while (x > y) {
            y = 2*y;
        }
        x = x-1;
    }
    
    return 0;
}
