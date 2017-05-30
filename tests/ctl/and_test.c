// -ctl_str "AND{AG{AF{n==1}}}{AF{n==0}}"
// -precondition n > 0
int main() {
    int n;  //assume n > 0
    while (n > 0) {
        n--;
    }

    while (n == 0) {
        n++;
        n--;
    }

    return;
}
