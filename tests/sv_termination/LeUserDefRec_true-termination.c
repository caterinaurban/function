// public class LeUserDefRec {
	// public static void main(String[] args) {
		// int x = args[0].length();
		// int y = args[1].length();
		// le(x, y);
	// }

	// public static boolean le(int x, int y) {
		// if (x > 0 && y > 0) {
			// return le(x-1, y-1);
		// } else {
			// return (x == 0);
		// }
	// }
// }

extern int __VERIFIER_nondet_int(void);
int le(int x, int y);
int random(void);

int random() {
	int x; 
	if (x < 0)
		return -x;
	else
		return x;
}

int le(int x, int y) {
		if (x > 0 && y > 0) {
			return le(x-1, y-1);
		} else {
		  if (x == 0)
		    return 1;
		  return 0;
		}
}

int main() {
	int x, y , z; 
	if(x < 0)
		return 0;
	if(y < 0) 
		return 0;
	le(x,y);

}
