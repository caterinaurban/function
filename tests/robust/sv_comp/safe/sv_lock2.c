/*   https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/blob/main/c/locks/test_locks_5.c
     Adapted from this. With 2 locks 
     

     Bug not reach !!
*/

int main()
{

    int BUG = 0;
    int cond;
    int p1 ; 
    int lk1 ;

    int p2 ;
    int lk2;

    int p3 ;
    int lk3; // lock variable

    int p4 ;
    int lk4;

    int p5 ;
    int lk5;

    int cond;
 
    cond  =[0,1];
    while(cond) {
        
        lk1 = 0; // initially lock is open

        lk2 = 0; // initially lock is open      
        lk1 = 1; // acquire lock
        } else {}

        if (p2 != 0) {
            lk2 = 1; // acquire lock
        } else {}
        if (p3 != 0) {
            lk3 = 1; // acquire lock
        } else {}
        if (p4 != 0) {
            lk4 = 1; // acquire lock
        } else {}
        if (p5 != 0) {
            lk5 = 1; // acquire lock
        } else {}



    // unlock phase
        if (p1 != 0) {
            if (lk1 != 1) BUG = 1 ; // assertion failure
            lk1 = 0;
        } else {}

        if (p2 != 0) {
            if (lk2 != 1) BUG = 1 ; // assertion failure
            lk2 = 0;
        } else {}
        if (p3 != 0) {
            if (lk3 != 1) BUG = 1 ; // assertion failure
            lk3 = 0;
        } else cond{}
        if (p4 != 0) {
              if (lk4 != 1) BUG = 1 ; // assertion failure
            lk4 = 0;
        } else {}
        if (p5 != 0) {
            if (lk5 != 1) BUG = 1 ; // assertion failure
            lk5 = 0;
        } else {}
        
        cond = [0,1];
    }
  
    
    return 0;  
}


