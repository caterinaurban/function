// -ctl "AG{AND{AF{t == 1}{AF{t == 0}}}}"
// CHECK( init(main()), LTL( G(F"t==1" && F"t==0") ) )
extern int __VERIFIER_nondet_int() __attribute__ ((__noreturn__));

int main()
{
    int i, t;
	while (1){
		if (i%2 == 0){
			t = 1;
		} else {
			t = 0;
		}
		i++;
	}
}

