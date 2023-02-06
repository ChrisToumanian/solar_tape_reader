      subroutine getell(ntheta, ellips, zell, ktheta)
C
C     Compute the location of an ellipse
C
C     input :
C       ntheta : nbr of angular location
C       ellips : ellipse params (A,B,Xc,Yc,Theta_o)
C
C     output :
C       zell(7,i) : (x,y,r,cos(theta),sin(theta),cos(alpha),sin(alpha))
C       ktheta : set to ntheta, once the {theta} are defined
C                if ktheta=ntheta, then cos(theta) and sin(theta) are kept
C
C     SGK/24-May-89/26-Jun-89                                 Ver2.1/0/F77
C
      parameter (twopi=6.283185308)
      dimension ellips(5), zell(7,1)
C
      ct0 = cos(ellips(5))
      st0 = sin(ellips(5))
C
      if(ktheta.ne.ntheta) then
C
        ktheta=ntheta
        dalpha = twopi/ntheta
        do 100 i=1,ntheta
          alpha = i*dalpha
          ca = cos(alpha)
          sa = sin(alpha)
          x  = ellips(1)*ca*ct0 - ellips(2)*sa*st0
          y  = ellips(1)*ca*st0 + ellips(2)*sa*ct0
          r  = sqrt(x*x + y*y)
          zell(1,i) = x + ellips(3)
          zell(2,i) = y + ellips(4)
          zell(3,i) = r
          zell(4,i) = x/r
          zell(5,i) = y/r
          zell(6,i) = ca
          zell(7,i) = sa
  100   continue
C
      else
C
        do 200 i=1,ntheta
          xnum = ellips(1)*(zell(5,i)*ct0-zell(4,i)*st0)
          xden = ellips(2)*(zell(4,i)*ct0+zell(5,i)*st0)
c--          alpha = atan2(xnum,xden)
c--          ca = cos(alpha)
c--          sa = sin(alpha)
          xsqr = sqrt(xnum*xnum+xden*xden)
          ca = xden/xsqr
          sa = xnum/xsqr
          x  = ellips(1)*ca*ct0 - ellips(2)*sa*st0
          y  = ellips(1)*ca*st0 + ellips(2)*sa*ct0
          zell(1,i) = x + ellips(3)
          zell(2,i) = y + ellips(4)
          zell(3,i) = sqrt(x*x+y*y)
          zell(6,i) = ca
          zell(7,i) = sa
  200   continue
C
      endif
C
      do 991 i=1,ntheta
  991 write(11,*) (zell(j,i),j=1,7)
C
      return
      end
