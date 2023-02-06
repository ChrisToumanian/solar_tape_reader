      subroutine fitell(nlmb, zell, zlmb, zemax, ellips, dellips)
C
C     Differential LSQ fit to an ellipse
C
C     input :
C             nlmb         : nbr of points that define the limb
C             zell(7,ilmb) : (x_ell, y_ell, r_ell,
C                            cos(theta), sin(theta), cos(alpha), sin(alpha))
C             zlmb(2,ilmb) : (x_lmb, y_lmb)
C             zemax        : max allowed limb error to be used in the fit
C             ellips(5)    : (A,B,Xc,Yc,Theta_o)
C      output :
C             dellips(5)   : (d_A,d_B,d_Xc,d_Yc,d_Theta_o)
C                            correction to the fitting ellipse
C
C      SGK/25-Aug-88/26-Jun-89                                    Ver 2.3/0/F77
C
      parameter (nlmbx=3200)
      dimension errlmb(nlmbx)
      dimension zell(7,1), zlmb(2,1), ellips(5), dellips(5)
     &,         a(5,6), ellps6(6)
C
      common /dbg/ idbg, ludbg
C
      if(nlmb.gt.nlmbx) stop 'Fitell: nlmb too large'
C
      if(idbg.gt.1) then
        write(ludbg,*) 'fitell: ellips'
        write(ludbg,*) ellips
      endif
      ellps6(1) = ellips(1)
      ellps6(2) = ellips(2)
      ellps6(3) = ellips(3)
      ellps6(4) = ellips(4)
      ellps6(5) = cos(ellips(5))
      ellps6(6) = sin(ellips(5))
C
      do 10 j = 1, 6
        do 10 i = 1, 5
   10   a(i,j) = 0.0
C
      do 15 i = 1 ,5
 15   dellips(i) = 0.0
C
      nlmbkept = 0
      do 20 k = 1, nlmb
        errlmbx = zlmb(1,k)-zell(1,k)
        errlmby = zlmb(2,k)-zell(2,k)
        errlmb(k) = sqrt(errlmbx*errlmbx + errlmby*errlmby)
        if(errlmb(k).gt.zemax) goto 20
        nlmbkept = nlmbkept + 1
 20   continue
C
      if(idbg.gt.0) write(ludbg,*) 'fitell: nlmbkept=', nlmbkept
      if(nlmbkept.lt.0.25*nlmb)
     &  write(*,*) 'Fitell: less than 25% of #limb points valid.'
C
      do 50 j = 1, 5
        do 40 i = 1, j
          do 30 k = 1, nlmb
            if(errlmb(k).gt.zemax) goto 30
            a(i,j) = a(i,j)
     &       + gx(j,zell(4,k),zell(5,k),zell(6,k),zell(7,k),ellps6)
     &        *gx(i,zell(4,k),zell(5,k),zell(6,k),zell(7,k),ellps6)
     &       + gy(j,zell(4,k),zell(5,k),zell(6,k),zell(7,k),ellps6)
     &        *gy(i,zell(4,k),zell(5,k),zell(6,k),zell(7,k),ellps6)
   30     continue
   40   a(j,i) = a(i,j)
   50 continue
      do 70 i = 1, 5
        do 60 k = 1, nlmb
          if(errlmb(k).gt.zemax) goto 60
          errlmbx = zlmb(1,k)-zell(1,k)
          errlmby = zlmb(2,k)-zell(2,k)
          a(i,6) = a(i,6) + errlmbx
     &        *gx(i,zell(4,k),zell(5,k),zell(6,k),zell(7,k),ellps6)
     &                    + errlmby
     &        *gy(i,zell(4,k),zell(5,k),zell(6,k),zell(7,k),ellps6)
   60   continue
   70 continue
C
CD      write(6,*) 'a(i,j),i=1,5)'
CD     do 991 j=1,6
CD 991 write(6,*) (a(i,j),i=1,5)
C
      call diaggja(a,5,5,ierr)
C
CD      write(6,*) 'diaggja ierr =',ierr
CD      write(6,*) 'a(i,6),i=1,5)'
CD      write(6,*) (a(i,6),i=1,5)
      if(ierr.ne.0) stop 'fitell : diaggj error'
      do 100 i=1,5
  100 dellips(i) = a(i,6)
C
      if(idbg.gt.0) then
        write(ludbg,*) 'fitell: dellips'
        write(ludbg,*) dellips
      endif
      return
      end
      function gx(k,ct,st,ca,sa,ellps6)
C
C     return the derivative vs kth parameter of the x def of the ellipse
C
C     input :
C             k : parameter index (A,B,Xc,Yc,Theta_o)
C             ct : cos(theta)
C             st : sin(theta)
C             ca : cos(alpha)
C             sa : sin(alpha)
C             ellps6 : ellipse coefficients, in the form
C             (A,B,Xc,Yc,cos(Theta_o),sin(Theta_o))
C
C     SGK/25-Aug-88                                            Ver 2.0/0/F77
C
      dimension ellps6(6)
C
      goto(100,200,300,400,500),k
C
  100 gx = ca*ellps6(5)
      return

  200 gx = -sa*ellps6(6)
      return

  300 gx = 1.0
      return

  400 gx = 0.0
      return

  500 dadt0 =  ellps6(1)/ellps6(2)
      xden = ct*ellps6(5)+st*ellps6(6)
      if(xden.ne.0.0) dadt0=dadt0*ca*ca/xden/xden
      gx = -(ellps6(1)*ca*ellps6(6)+ellps6(2)*sa*ellps6(5))
     &     +(ellps6(1)*sa*ellps6(5)+ellps6(2)*ca*ellps6(6))*dadt0
      return
      end
      function gy(k,ct,st,ca,sa,ellps6)
C
C     return the derivative vs kth parameter of the y def of the ellipse
C
C     input :
C             k : parameter index (A,B,Xc,Yc,Theta_o)
C             ct : cos(theta)
C             st : sin(theta)
C             ca : cos(alpha)
C             sa : sin(alpha)
C             ellps6 : ellipse coefficients, in the form
C             (A,B,Xc,Yc,cos(Theta_o),sin(Theta_o))
C
C     SGK/25-Aug-88                                            Ver 2.0/0/F77
C
      dimension ellps6(6)
C
      goto(100,200,300,400,500),k
C
  100 gy = ca*ellps6(6)
      return

  200 gy = sa*ellps6(5)
      return

  300 gy = 0.0
      return

  400 gy = 1.0
      return

  500 dadt0 =  ellps6(1)/ellps6(2)
      xden = ct*ellps6(5)+st*ellps6(6)
      if(xden.ne.0.0) dadt0=dadt0*ca*ca/xden/xden
      gy =   ellps6(1)*ca*ellps6(5)-ellps6(2)*sa*ellps6(6)
     &     +(ellps6(1)*sa*ellps6(6)-ellps6(2)*ca*ellps6(5))*dadt0
      return
      end
