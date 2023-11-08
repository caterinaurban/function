// Adjusted version of P3.c that works with FuncTion
//
//
// '[AG](varA != 1 || [EF](varR == 1))'
// -ctl "OR{varA != 1}{EF{varR==1}"
// -precondition "varA == 0 && varR == 0"
//

void main() {

    int n;
    int m;
    int n_init;
    int m_init;

    int varA = 0;
    int varR = 0;

    //loc1
    n = 0;
    while(n < n_init) {
        n++;
        varA = 1;
        // loc2
        varA = 0;
        // loc3
        m = 0;
        while(m < m_init) {
            m++;
        }
        varR = 1;
        //loc4
    }
    //loc5
}

