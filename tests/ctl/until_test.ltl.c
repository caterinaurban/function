//#Safe
//@ ltl invariant positive: (AP(x >= y) U AP(x == y));

int y;
int x;

int main() {
    
    // assume x > y
    x = y + 10000;
    
    while (x > y) {
        x = x - 1;
    }
    // now x == y
}
