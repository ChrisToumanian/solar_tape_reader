      subroutine getlmb(ntheta, ellips, zell, img, ix, jx, dr
     &,                 imskn, imskx, jmskn, jmskx
     &,                 erm, ers, zlmb)
C
C     Find the location of the limb.
C
C     input :
C             ntheta     : No of points where to compute the limb
C             ellips(5)  : presumed location of the limb (A,B,Xc,Yc,Theta_o)
C                          (Theta_o in radians)
C             zell(1,i)  : x coord for ellipse
C             zell(2,i)  : y coord
C             zell(3,i)  : radius  (x*x + y*y)
C             zell(4,i)  : cos(theta)
C             zell(5,i)  : sin(theta)
C             zell(6,i)  : cos(alpha)
C             zell(7,i)  : sin(alpha)
C             img(ix,jx) : I*2 image of size ix by jx
C             dr         : width around assumed ellipse to find the limb
C             i/jmskn/x  : limiting mask
C     Output:
C             erm        : mean of radial error
C             ers        : st dev of radial error
C             zlmb(1,i)  : x_limb(Theta)
C             zlmb(2,i)  : y_limb(Theta)
C
C     SGK/25-Aug-88/24-May-89                                 Ver 2.1/0/F77
C
      parameter (twopi=6.283185308, idrx=256)
C
      dimension ellips(5), zell(7,1), zlmb(2,1)
     &,         didr(idrx)
      integer   x(idrx), y(idrx)
      integer*2 img(ix,jx)
C
      write(6,*) 'getlmb : ntheta, dr=', ntheta, dr
C
      xc = ellips(3)
      yc = ellips(4)
      erm = 0.0
      ers = 0.0
C
      do 100 itheta = 1, ntheta
        ct = zell(4,itheta)
        st = zell(5,itheta)
        rl = zell(3,itheta)
C
        r0 = rl - dr/2.
        idr = dr + 0.5
        idr = min(idr,idrx)
        do 50 ir = 1, idr
          r = r0 + ir
          i = r * ct + xc + 0.5
          i = max(imskn,min(i,imskx))
          x(ir) = i
          j = r * st + yc + 0.5
          j = max(jmskn,min(j,jmskx))
          y(ir) = j
          didr(ir) = (img(i+1,j)-img(i-1,j))*ct
     &              +(img(i+2,j)-img(i-2,j))*ct/2
     &              +(img(i,j+1)-img(i,j-1))*st
     &              +(img(i,j+2)-img(i,j-2))*st/2
          didr(ir) = abs(didr(ir))
   50   continue
        irx = locmax(didr,idr)
        rr  = sqrt( (x(irx)-xc)**2 + (y(irx)-yc)**2 )
        drr = rr - zell(3,itheta)
        erm = erm + drr
        ers = ers + drr*drr
        zlmb(1,itheta) = x(irx)
        zlmb(2,itheta) = y(irx)
       write(10,*) zell(1,itheta), zell(2,itheta), x(irx), y(irx), drr
  100 continue
      erm = erm/ntheta
      ers = sqrt(ers/ntheta-erm*erm)
      write(6,*) 'getlmb : <erl>, std(erl) =',erm,ers
      return
      end
      function locmax(array, npts)
C
C     return the location of the max in the array of npts points
C     (first one if more that one)
C
      dimension array(1)
C
      ax = array(1)
      ix = 1
      do 10 i = 1, npts
        if(array(i).le.ax) goto 10
        ax = array(i)
        ix = i
   10 continue
      locmax = ix
      return
      end
