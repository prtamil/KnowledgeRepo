Display BitWise Num in Binary in C
==================================

  #define kDisplayWidth 32  
  char* pBinFill(long int x,char *so, char fillChar)  
  { // fill in array from right to left  
      char s[kDisplayWidth+1];   
      int  i=kDisplayWidth;  
      s[i--]=0x00;   // terminate string  
      do  
      { // fill in array from right to left  
          s[i--]=(x & 1) ? '1':'0';  
          x>>=1;  // shift right 1 bit  
      } while( x > 0);  
      while(i>=0) s[i--]=fillChar;    // fill with fillChar  
      sprintf(so,"%s",s);  
      return so;  
  }  

//To use it we can use it like 

   char so[32+1];  
   cout<<pBinFill(12,so,'0')<<endl;  
 
