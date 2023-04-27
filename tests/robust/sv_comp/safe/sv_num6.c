// https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/blob/main/c/loops/terminator_02-2.c
int main()
{   
    int x ; 
    int y ; 
    int z ;
    int tmp;
    int bugx;
    int bugz;
    bugx = 101;
    bugz = 99;

    if (!(x>-100)) return 0;
    if (!(x<200)) return 0;
    if (!(z>100)) return 0;
    if (!(z<200)) return 0;
    while(x<100 && z>100) 
    {
        tmp= [0,1];
        if (tmp) {
            x++;
        } else {
            x--;
            z--;
        }
    }                       
    bugx = x; 
    bugz = y;
    

    return 0;
}
