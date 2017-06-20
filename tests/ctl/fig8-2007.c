// *************************************************************
//                  Original Source: 
//              Byron Cook * Eric Koskinen
//                     July 2010
//
//
// Adjusted for FuncTion by Samuel Ueltschi
// *************************************************************

// Benchmark: fig8-2007.c
//
// Property: (set == 1) => AF(unset == 1)
//
// -ctl_cfg "OR{set==0}{AF{unset == 1}}"

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

