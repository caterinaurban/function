//#Safe
//@ ltl invariant positive: <>(AP(x == 4 || x == -4));

extern int __VERIFIER_nondet_int() __attribute__ ((__noreturn__));

int x;

int main() {

    if (__VERIFIER_nondet_int()) {
        x = 1;
    } else {
        x = -1;
    }

    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }

    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }

    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }


}
