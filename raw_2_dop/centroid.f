      subroutine centroid(img, ix, jx, ellips, acurell, niterx, erx
     &,                   imskn, imskx, jmsln, jmskx
     &,                   ntheta, niter, xnstd)
C
C     fit an ellipse to the limb of an image
C
C     input :
C             img(ix,jx) : I*2, image of size ix by jx.
C             ellips(5)  : 5 coeff that define the ellipse (A,B,Xc,Yc,Theta_o)
C                          initial guess for the ellipse (Theta_o in rad.).
C             acurell(5) : absolute accuracy req'd for the 5 coeff that
C                          define the ellipse.
C             niterx     : max no of iterations.
C             erx        : largest error expected between actual limb position
C                          and first guess ellipse (in pixels).
C             i/jmskn/x  : limiting mask for the image.
C             ntheta     : no of angular location where to evaluate the limb.
C             nstd       : no of st. dev. for valid limb point.
C      output :
C             ellips(5)  : solution.
C             niter      : nbr of iterations performed, negatif if not converged
C
C      SGK/25-Aug-88/08-Aug-89                                Ver 2.4/0/F77
C
      parameter (nlmbx=3200,pidemi=1.570796327,pi=2.0*pidemi)
      dimension ellips(5), dellips(5), acurell(5)
     &,         zell(7,nlmbx), zlmb(2,nlmbx)
      integer*2 img(ix,jx)
C
      common /dbg/ idbg, ludbg
C
C     maximum angular correction per iteration
C
      data dtmax/0.5236/
C
      if(ntheta.gt.nlmbx) stop 'Centroid: ntheta too large'
C
      ktheta = 0
      call getell(ntheta, ellips, zell, ktheta)
      call getlmb(ntheta, ellips, zell, img, ix, jx, erx
     &,           imskn, imskx, jmsln, jmskx
     &,           erm, ers, zlmb)
      do 100 niter = 1, niterx
C
        zemax = xnstd*ers
        if(idbg.gt.0) then
          write(ludbg,*) 'Centroid: niter, ers, zemax'
          write(ludbg,*) niter, ers, zemax
        endif
        call fitell(ntheta, zell, zlmb, zemax, ellips, dellips)
C
C     limit Delta(Thetha_o), by dividing by 2 when larger than some given value
C
        if(abs(dellips(5)).gt.dtmax) then
          dellips(5) = dellips(5)/2.0
        endif
C
        do 40 i = 1, 5
   40   ellips(i) = ellips(i) + dellips(i)
C
C        keep A > B and -90 < Theta_o < +90
C
         if(ellips(2).gt.ellips(1)) then
           a = ellips(1)
           ellips(1) = ellips(2)
           ellips(2) = a
           ellips(5) = ellips(5) + pidemi
         endif
         kpi = ellips(5)/pi
         ellips(5) = ellips(5) - kpi*pi
         if(ellips(5).gt. pidemi) ellips(5)=ellips(5)-pi
         if(ellips(5).lt.-pidemi) ellips(5)=pi+ellips(5)
C
         if(idbg.gt.0) then
           write(ludbg,*) 'Centroid: ellips'
           write(ludbg,*) ellips
         endif
         do 50 i = 1, 5
           if(abs(dellips(i)).gt.acurell(i)) goto 90
   50    continue
         goto 120
C
   90    call getell(ntheta, ellips, zell, ktheta)
         ers = cmpers(ntheta, ellips, zell, zlmb)
  100 continue
C
      niter = -niterx
  120 return
      end
      function cmpers(ntheta, ellips, zell, zlmb)
c
c     compute the st dev of the radial error between the limb and the ellipse
c
      dimension ellips(5), zell(7,1), zlmb(2,1)
c
      erm = 0.0
      ers = 0.0
      do 100 itheta = 1, ntheta
        rr = sqrt( (zlmb(1,itheta)-ellips(3))**2 
     &            +(zlmb(2,itheta)-ellips(4))**2 )
        dr = rr - zell(3,itheta)
        erm = erm + dr
        ers = ers + dr*dr
 100  continue
      erm = erm/ntheta
      ers = sqrt(ers/ntheta-erm*erm)
      cmpers=ers
      write(6,*) 'cmpers: <dr>=',erm,', std(dr}=',ers
      return
      end
