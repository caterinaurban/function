int main() { 
  
  int sn ; 
  int x ; 
  int bug;
  int i ; 
  x = 0 ; 
  sn = 0; 
  bug = 0;

  for(i = 0 ; i< 1000;i++){
      if (x<10) {
          sn = sn + 2;
      }
      x++;
      if( !(sn==x*2 || sn == 0)) bug = 1;
  }
}
