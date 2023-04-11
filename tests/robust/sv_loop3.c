
int main() {
    int i;
    int j;
    int k;
    i = 0 ;
    k = ?;
    j = 0;
    while (i <= 100) {
        i = i + 1;
        while (j < 20) {
            j = i + j;
        }
        k = 4;
        while (k <= 3) {
            k = k + 1;
        }
    }
/*  
    Prop (k == 4)
    Controlled: i
    uncontrolled: j
*/
    return 0;
}