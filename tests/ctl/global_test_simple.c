// FuncTion arguments:
// -ctl "AG{AF{x <= -10}}" 
// -joinbwd 4
int main() {
    int x;

    while (x < 0) {
        x = x + 1;
        while (x < -3) {
            x = x - 1;
        }
    }

    x = -20;
    while(1) {}
}
