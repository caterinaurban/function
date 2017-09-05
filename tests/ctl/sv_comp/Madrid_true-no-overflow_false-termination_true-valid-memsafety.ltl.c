//#Safe
//@ ltl invariant positive: <>(AP(x==7) && <>[]AP(x==2));

/*
 * Date: 2013-05-02
 * Author: heizmann@informatik.uni-freiburg.de
 *
 */
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int x;
int main()
{
	x = 7;
	while (true) {
		x = 2;
	}
	return 0;
}
