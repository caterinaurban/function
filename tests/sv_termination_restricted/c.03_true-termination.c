typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int c, x, y, z;
    // x = __VERIFIER_nondet_int();
    // y = __VERIFIER_nondet_int();
    // z = __VERIFIER_nondet_int();
    c = 0;
    while (x < y) {
        if (x < z) {
            x = x + 1;
        } else {
            z = z + 1;
        }
        c = c + 1;
    }
    return 0;
}


