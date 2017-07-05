//#Safe
//@ ltl invariant positive: <>[]AP(WItemsNum >= 1);

extern int __VERIFIER_nondet_int() __attribute__ ((__noreturn__));
int WItemsNum;
void main() {
    
    while(1) {
        while(WItemsNum <= 5 || __VERIFIER_nondet_int() == 1) {
               if (WItemsNum <= 5) {
                   WItemsNum++;
               } else {
                   WItemsNum++;
               }
        }
        while(WItemsNum > 2) {
             WItemsNum--;
        }
    }

    while(1) {}
}
    
