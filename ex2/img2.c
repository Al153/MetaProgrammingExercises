#include <limits.h>
/*@
requires (((((0<x0) &&
(x0<100)) &&
((0<x1) &&
(x1<100))) &&
\valid(x2+(0..(x0*x1)-1))) &&
(\forall int x67; (0<=x67<(x0*x1)) ==> ((x2[x67]>=0) &&
(x2[x67]<256))));
ensures (\forall int x88; (0<=x88<(x0*x1)) ==> ((x2[x88]>=0) &&
(x2[x88]<256)));
*/
void p(int  x0, int  x1, int  * x2) {
  int x44 = 1 > 3;
  int x45;
  if (x44) {
    x45 = 4;
  } else {
    x45 = 5;
  }
  /*@
  loop invariant 0<=x5<=x0;
  loop assigns x5, x2[(0..(x0*x1)-1)];
  loop variant x0-x5;
  */
  for(int x5=0; x5 < x0; x5++) {
    int x11 = x5 * x1;
    /*@
    loop invariant 0<=x13<=x1;
    loop invariant (x11==(x5*x1));
    loop invariant (\forall int x19; (0<=x19<(x0*x1)) ==> ((x2[x19]>=0) &&
    (x2[x19]<256)));
    loop assigns x13, x2[(0..(x0*x1)-1)];
    loop variant x1-x13;
    */
    for(int x13=0; x13 < x1; x13++) {
      int x42 = x11 + x13;
      int x43 = x2[x42];
      x2[x42] = x45;
    }
  }
}
