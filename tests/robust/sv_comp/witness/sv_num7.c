
// https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/blob/main/c/loops/nec20.c
int main(){
   int k ;
   int lab;
   int i; 
   int n; 
   int j;
 
   if (k){
      n=0;
   } 
   else{
      n=1023;
   }

   i=0;
 
   while ( i <= n){
      i++;
      j= j+2;
   }
 
   lab = j ;

   
   return 0 ;
}