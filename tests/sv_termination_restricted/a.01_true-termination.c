typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int c;
    int x, y;
   // x = __VERIFIER_nondet_int();
   // y = __VERIFIER_nondet_int();
    c = 0;
    while (x > 0) {
        y = 0;
        while (y < x) {
            y = y + 1;
            c = c + 1;
        }
        x = x - 1;
    }
    return 0;
}
