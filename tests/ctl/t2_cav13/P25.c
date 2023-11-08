// OK
// (varC <= 5) || ([AF](varR > 5))
// -ctl "OR{varC <= 5}{AF{varR > 5}}
// -joinbwd 6

void main() {

    int varC; // assume varC >= 1
    int varR = 0;
    int varCS = 8;

    while (varCS > 0) {
        if (varC >= varCS) {
            varC = varC - 1;
            varR = varR + 1;
            varCS = varCS - 1;
        } else {
            varC = varC - 1; 
            varR = varR + 1; 
            varCS = varCS - 1;
        }
    }

}
