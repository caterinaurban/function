/**
 * Samuel Ueltschi: multiple branches with initial non-det choice
 *
 * FuncTion arguments: 
 * -ctl AF{OR{x==4}{x==-4}}
 * -ctl EF{x==-4}
 *
 */

int main() {
    int x;

    if (?) {
        x = 1;
    } else {
        x = -1;
    }

    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }

    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }

    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }


}
