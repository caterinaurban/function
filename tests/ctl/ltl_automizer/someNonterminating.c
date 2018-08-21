// -precondition "x > 0" -ctl "EG{x > 0}"
//#Unsafe

//@ ltl invariant positive: []AP(x > 0);

extern int __VERIFIER_nondet_int();

int x,y;

int main(){
    x = __VERIFIER_nondet_int();
    y = __VERIFIER_nondet_int();
    while(x>0){
        x = x-y;
    }
}
