typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int x, y;
    // x = __VERIFIER_nondet_int();
    // y = __VERIFIER_nondet_int();
    if (x > 0) {
        while (x > y) {
            y = y + x;
        }
    }
    return 0;
}
