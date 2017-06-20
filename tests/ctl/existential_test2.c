// -ctl_str "EF{r == 1}"
// -domain polyhedra
// should return UNKNONW
int main() {
    int r = 0;
    int x;
    int y;
    if (x*x < y*y + 3*x*y) { 
        // here we use non-linear expression that can't be expressed 
        // in any of the domains to validate if FILTER underapproximates correctly
        if (?) {
            r = 1;
        } 
    }
}

