//#Safe
//@ ltl invariant positive: <>[]AP(x < -100) || <>AP(x == 20);
int x;
int main() {
    if (x <= 0) {
        while(x <= 0) { x--; }
    } else {
        x = 20;
    }
}
