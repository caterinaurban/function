// package TwoWay;

// public class TwoWay {
	// public static void main(String[] args) {
		// Random.args = args;
		// twoWay(true, Random.random());
	// }

	// public static int twoWay(boolean terminate, int n) {
		// if (n < 0) {
			// return 1;
		// } else {
			// int m = n;
			// if (terminate) {
				// m--;
			// } else {
				// m++;
			// }
			// return m*twoWay(terminate, m);
		// }
	// }
// }

extern int __VERIFIER_nondet_int(void);

int twoWay(int terminate, int n) {
		if (n < 0) {
			return 1;
		} else {
			int m = n;
			if (terminate) {
				m--;
			} else {
				m++;
			}
			return m*twoWay(terminate, m);
		}
	}

int main() {
	int x, y, z; 
	if(x < 0)
		return 0;
	if(y < 0) 
		return 0;
	twoWay(1,x);

}
