// -ctl_str "EU{x >= y}{x == y}"
// -domain polyhedra
// -precondition "x > y"
int main() {
    // assume x > y
    int x;
    int y;

    if (?) {
        // loop invariant: x >= y
        while (x > y) {
            x = x - 1;
        }
        // now x == y
    } else {
        // on this trace be break the until property
        x = y - 1;
    }
}
