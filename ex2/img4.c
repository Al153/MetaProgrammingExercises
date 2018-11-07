#include <limits.h>
/*@
requires ((((0<x0) &&
(x0<100)) &&
((0<x1) &&
(x1<100))) &&
\valid(x2+(0..(x0*x1)-1)));
*/
void p(int  x0, int  x1, int  * x2) {
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
    loop assigns x13, x2[(0..(x0*x1)-1)];
    loop variant x1-x13;
    */
    for(int x13=0; x13 < x1; x13++) {
      int x22 = x11 + x13;
      int x23 = x2[x22];
      int x24 = x23 > 120;
      int x25;
      if (x24) {
        x25 = x23;
      } else {
        x25 = 0;
      }
      x2[x22] = x25;
    }
  }
}
