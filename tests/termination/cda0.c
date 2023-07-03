int step(int x ){
    if ( x > 20 ){
        return 3;
    }
    else if ( x > 10) {
        return 2;

    }else {
        return 1;

    }
}


int main () {
    int y; int i;
    if (y > 0 ){
        i = - step(y);
    }else {
        i = step(-y);
    }

    while( y < -3 || y > 3 ){ 
        y = y + i;
    }




    return 0 ; 
}