void main(){
  int x,x0,x1,y0,y1,r;
  y0 = 1;
  y1 = 7; 
  
  int bug ; 



  
  bug = 0 ; 
  if (x0 <= x && x <= x1 && x0 < x1 && y0 <= y1) {
    r = y0 + ((uint64_t) (x-x0) * (y1-y0) / (x1-x0));
    if (!(y0 <= r && r <= y1)) bug = 1 ;
  }

}





