
int main() {
	int x1; int x2;
	x1 = 1; 
	x2 = 1;
	while (x2 >= 1) {
		if (x1 > 100) {
			x1 = x1 - 10;
			x2 = x2 - 1;
		} else {
			x1 = x1 + 11;
			x2 = x2 + 1;
		}
	}
	return x2;
}