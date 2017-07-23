//#Safe
//@ ltl invariant positive: []<>AP(n==1) && <>AP(n == 0);

int n;

void main() {
    while (n > 0) {
        n--;
    }

    while (n == 0) {
        n++;
        n--;
    }
}
