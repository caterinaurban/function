typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int c;
    int x, y;
    // x = __VERIFIER_nondet_int();
    // y = __VERIFIER_nondet_int();
    c = 0;
    while ((x == y) && (x > 0)) {
        while (y > 0) {
            x = x - 1;
            y = y - 1;
            c = c + 1;
        }
    }
    return 0;
}
