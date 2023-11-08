// Can't be expressed with function because 
// of the whay AG/EG is defined
//
// (varC > 5) && [EG](varR <= 5)
//
// -ctl "OR{varC > 5}{EG{varR <= 5}}
void main() {

    int varC; // assume varC >= 1
    int varR = 0;
    int varCS = 4;


    while (varCS > 0) {
        if (varC >= varCS) {
            varC = varC - 1;
            varR = varR + 1;
            varCS = varCS - 1;
        } else {
            /* varC = varC - 1; */ 
            /* varR = varR + 1; */ 
            varCS = varCS - 1;
        }
    }

}
