// -ctl_str "AU{x >= y}{x == y}"
// -domain polyhedra
// -precondition "x >= y"
int main() {
    // assume x > y
    int x;
    int y;
    while (x > y) {
        x = x - 1;
    }
    // now x == y
}
