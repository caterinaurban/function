// *************************************************************
//                  Original Source: 
//              Byron Cook * Eric Koskinen
//                     July 2010
//
//          Modified version of acqrel that can be proven by FuncTion
// *************************************************************

// FuncTion arguments:
// -ctl "OR{set==0}{AF{unset == 1}}"

int i; int Pdolen; int DName;
int status;

int set;
int unset;

void main() {

    set = 1; // aquire resource

    while (i < Pdolen) { 
        // DName = ?;  // leads to bad widening behaviour
        if (!DName) { break; } 
        status = ?; 
        if (1 != status) { 
            if (2 == status) { 
                goto loc_continue; 
            } 
            break; 
        } else { 
            i++; 
        } 
    } 

loc_continue:

    // release resource
    unset = 1; 
    
}

