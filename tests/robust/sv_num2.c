
int main() {
    int x;
    int y;
    int z;
    int w;
    
    int nd1; 
    int nd2; 
    int nd3;

    
    x = 0 ;
    y = 0;
    z = 0 ;
    w = 0;
    
    nd1 = ?;
    while (nd1 && y < 10000) {
    nd2=?;
    nd3=?;
	if (nd2) {
	    x = x + 1;
	    y = y + 100;
	} else if (nd3) {
	    if (x >= 4) {
		x = x + 1;
		y = y + 1;
	    }
	} else if (y > 10*w && z >= 100*x) {
	    y = -y;
	}
	w = w + 1;
	z = z + 10;
    nd1=?;
    }
    //__VERIFIER_assert(x >= 4 && y <= 2);
    return 0;
}