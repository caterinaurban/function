// -ctl_str EF{r == 1}
// -precondition "x>0"
// -ctl_existential_equivalence
//
//Here widening fails for the direct EF solver, but works fine with -ctl_existential_equivalence
// 
// alternatively this works:
// -joinbwd 5
// -precondition "x==2"
//
int main() {
    int r = 0;
    int x;
    while (x > 0) {
        x = x - 1;
        if (?) {
            r = 1;
        }
    }


}
