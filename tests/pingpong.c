/*
GUARANTEE (z == 1)

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

void main() {
	int x = 0, y = 0, z = 0;

	// x = 1;
	// await (y > 0);
	// z = 1;
	//
	// await (x > 0);
	// y = 1;
	if (x > 0) {
		// x = 1;
		// await (y > 0);
		// z = 1;
		//
		// y = 1;
		if (?) {
			x = 1;
			// await (y > 0);
			// z = 1;
			//
			// y = 1;
			if (y > 0) {
				// z = 1;
				//
				// y = 1;
				if (?) {
					z = 1;
					y = 1;
				} else {
					y = 1;
					z = 1;
				}
			} else {
				y = 1;
				// await (y > 0);
				// z = 1;
				while (y <= 0);
				z = 1;
			}
		} else {
			y = 1;
			// x = 1;
			// await (y > 0);
			// z = 1;
			x = 1;
			while (y <= 0);
			z = 1;
		}
	} else {
		x = 1;
		// await (y > 0);
		// z = 1;
		//
		// await (x > 0);
		// y = 1;
		if (x > 0 && y > 0) {
			if (?) {
				z = 1;
				// await (x > 0);
				// y = 1;
				while (x <= 0);
				y = 1;
			} else {
				y = 1;
				// await (y > 0);
				// z = 1;
				while (y <= 0);
				z = 1;
			}
		} else if (x > 0) {
			// await (y > 0);
			// z = 1;
			//
			// y = 1;
			if (y > 0) {
				// z = 1;
				//
				// y = 1;
				if (?) {
					z = 1;
					y = 1;
				} else {
					y = 1;
					z = 1;
				}
			} else {
				y = 1;
				// await (y > 0);
				// z = 1;
				while (y <= 0);
				z = 1;
			}
		} else if (y > 0) {
			// z = 1;
			//
			// await (x > 0);
			// y = 1;
			if (x > 0) {
				// z = 1;
				//
				// y = 1;
				if (?) {
					z = 1;
					y = 1;
				} else {
					y = 1;
					z = 1;
				}
			} else {
				z = 1;
				// await (x > 0);
				// y = 1;
				while (x <= 0);
				y = 1;
			}
		} else {
			//
		}
	}

}