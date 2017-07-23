// NOK
//
// -ctl_cfg "OR{varA != 1}{EF{varR==1}"
// -precondition "varA == 0"


void main() {

    int varA = 0;
    int varR = 0;

    //loc1
    while(?) {
        varA = 1;
        // loc2
        varA = 0;
        // loc3
        while(?) {}
        varR = 1;
        //loc4
    }
    //loc5
}
