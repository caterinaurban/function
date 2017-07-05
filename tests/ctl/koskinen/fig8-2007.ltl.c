//#Safe
//@ ltl invariant positive: AP(set == 0) || <>AP(unset == 1);

extern int __VERIFIER_nondet_int() __attribute__ ((__noreturn__));

int i; int Pdolen; int DName;
int status;

int set = 0;
int unset = 0;

void main() {
    set = 1;
    while (i < Pdolen) { 
        DName = __VERIFIER_nondet_int();  
        if (!DName) { break; } 
        status = __VERIFIER_nondet_int(); 
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
    unset = 1; 
}

