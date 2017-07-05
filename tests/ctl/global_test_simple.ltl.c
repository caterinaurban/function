//#Safe
//@ ltl invariant positive: []<>AP(x <= -10);

int x;

int main() {

    while (x < 0) {
        x = x + 1;
        while (x < -3) {
            x = x - 1;
        }
    }

    x = -20;
    while(1) {}
}
