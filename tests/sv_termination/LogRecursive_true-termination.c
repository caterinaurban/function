// public class LogRecursive {
  // public static void main(String[] args) {
    // Random.args = args;
    // log(Random.random(), Random.random());
  // }

  // public static int log(int x, int y) {
    // if (x >= y && y > 1) {
      // return 1 + log(x/y, y);
    // }
    // return 0;
  // }
// }

extern int __VERIFIER_nondet_int(void);
int log(int x, int y);
int random(void);

int main() {
  int x, y ,z;
	if(x < 0)
		return 0;
	if(y < 0) 
		return 0;
	log(x,y);

}

int random() {
  int x; 
	if (x < 0)
		return -x;
	else
		return x;
}

int log(int x, int y) {
    if (x >= y && y > 1) {
      return 1 + log(x/y, y);
    }
    return 0;
  }
