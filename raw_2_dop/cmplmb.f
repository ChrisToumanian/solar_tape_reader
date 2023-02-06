      subroutine cmplmb(img, ix, jx
     &,                 ellips, xc, yc, xlmb, ylmb, nx)
c
c     extract the limb(s) from an images
c     interpolates the image to extract profiles in the X- and Y-directions
c     (angle of ellipse ignored), over +/- nx pixels around the image center
c
      integer*2 img(ix,jx)
      dimension ellips(5), xlmb(ix,-nx:nx), ylmb(jx,-nx:nx)
     &,         ux(4), uy(4)
c
      dx = ellips(3)-xc
      id = dx
      dx = dx - id
      if(dx.lt.0.0) then
        id = id-1
        dx = 1.0 + dx
      endif
c
      dy = ellips(4)-yc
      jd = dy
      dy = dy - jd
      if(dy.lt.0.0) then
        jd = jd-1
        dy = 1.0 + dy
      endif
c
c     compute the interpolation kernels
c
      dx2 = dx*dx
      dx3 = dx2*dx
      ux(1) = -0.5*dx +     dx2 - 0.5*dx3
      ux(2) =          - 2.5*dx2 + 1.5*dx3 + 1.0
      ux(3) =  0.5*dx + 2.0*dx2 - 1.5*dx3
      ux(4) =          - 0.5*dx2 +0.5 *dx3
c
      dy2 = dy*dy
      dy3 = dy2*dy
      uy(1) = -0.5*dy +     dy2 - 0.5*dy3
      uy(2) =          - 2.5*dy2 + 1.5*dy3 + 1.0
      uy(3) =  0.5*dy + 2.0*dy2 - 1.5*dy3
      uy(4) =          - 0.5*dy2 +0.5 *dy3
c
c     limits for translation
c 
      idm2 = id-2
      is = max( 1,  -idm2)
      ie = min(ix,ix-id-2)
      jdm2 = jd-2
      js = max( 1,  -jdm2)
      je = min(jx,jx-jd-2)
c
c     limits for the limbs
c
      ni0 = ix/2
      nis = ni0 - nx
      nie = ni0 + nx

      nj0 = jx/2
      njs = nj0 - nx
      nje = nj0 + nx
c
c     now translate and compute the x-limb
c
      do 100 j = njs, nje
        j0 = j + jdm2
        nj = j - nj0

        do 40 i=1,ix
 40     xlmb(i,nj) = 0.0

        do 50 kj = 1, 4
        jj = j0 + kj
          do 50 ki = 1, 4
          i0 = ki + idm2
          do 50 i = is, ie
            ii = i0 + i
            xlmb(i,nj) = xlmb(i,nj) + img(ii,jj)*ux(ki)*uy(kj)
   50   continue
  100 continue
c
c     now translate and compute the y-limb
c
      do 200 i = nis, nie
        i0 = i + idm2
        ni = i - ni0

        do 140 j=1,jx
 140    ylmb(j,ni) = 0.0

        do 150 ki = 1, 4
        ii = i0 + ki
          do 150 kj = 1, 4
          j0 = kj + jdm2
          do 150 j = js, je
            jj = j0 + j
            ylmb(j,ni) = ylmb(j,ni) + img(ii,jj)*ux(ki)*uy(kj)
  150   continue
  200 continue
c
      return
      end

