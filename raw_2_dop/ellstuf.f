      subroutine defib(xc,yc,xr,yr,dth,ib,nix,njx)
c
c     define the 'i' borders of an ellipse
c
c     the ellipse is defined by 
c     (xc,yc) : center
c     (xr,yr) : semi-axis
c     dth     : angle in degree
c     output
c     ib(1,1) = j1, ib(2,1) = j2
c     ib(1,j) = i1, ib(2,j) = i2 for j = j1, j2
c
      dimension ib(2,njx)
c
      th = dth*3.14159265/180.
c
c    find the higher and lower rows
c
      i0 = xc - xr*0.7071
      i1 = xc + xr*0.7071
      j0 = njx
      j1 = 1
      do 50 i=i0,i1
        xi = i
      	call yellips(y0,y1,xi,xc,yc,xr,yr,th,ierr)
      	if(ierr.ne.0) goto 50
        iy0 = y0 + 1.0
        j0 = min(j0,iy0)
        iy1 = y1
        j1 = max(j1,iy1)
   50 continue
c
c     save the values
c
      ib(1,1) = j0
      ib(2,1) = j1
c
c     set the borders
c   
      do 60 j=j0,j1
        yj = j
        call xellips(x0,x1,yj,xc,yc,xr,yr,th,ierr)
        if(ierr.ne.0) goto 60
        ib(1,j) = x0 + 1.0
        ib(2,j) = x1
   60 continue

      return
      end
      subroutine defellib(ellips,ib,nix,njx)
c
c     define the 'i' borders of an ellipse
c
c     the ellipse is defined by 
c     ellips(5) : (A,B,Xc,Yc,Theta_o)
c     output
c     ib(1,1) = j1, ib(2,1) = j2
c     ib(1,j) = i1, ib(2,j) = i2 for j = j1, j2
c
      dimension ib(2,njx), ellips(5)
c
      xr = ellips(1)
      yr = ellips(2)
      xc = ellips(3)
      yc = ellips(4)
      th = ellips(5)
c
c    find the higher and lower rows
c
      i0 = xc - xr*0.7071
      i1 = xc + xr*0.7071
      j0 = njx
      j1 = 1
      do 50 i=i0,i1
        xi = i
      	call yellips(y0,y1,xi,xc,yc,xr,yr,th,ierr)
      	if(ierr.ne.0) goto 50
        iy0 = y0 + 1.0
        j0 = min(j0,iy0)
        iy1 = y1
        j1 = max(j1,iy1)
   50 continue
c
c     save the values
c
      ib(1,1) = j0
      ib(2,1) = j1
c
c     set the borders
c   
      do 60 j=j0,j1
        yj = j
        call xellips(x0,x1,yj,xc,yc,xr,yr,th,ierr)
        if(ierr.ne.0) goto 60
        ib(1,j) = x0 + 1.0
        ib(2,j) = x1
   60 continue

      return
      end
      function xint(image,ib,nix,njx)
c
c     compute the integrated intensity of the image within the borders
c
      integer*2 image(nix,njx)
      dimension ib(2,njx)
      double precision x
c
      j0 = ib(1,1)
      j1 = ib(2,1)
c
      x = 0.0
      do 100 j = j0, j1
        do 100 i = ib(1,j), ib(2,j)
  100 x = x + image(i,j)
c
      xint = x
      return
      end
	subroutine yellips(y1,y2,x,xc,yc,a,b,t,ierr)
c
	ierr = 0

	xx = x-xc

	s2 = sin(t)**2
	c2 = cos(t)**2
	boa2 = (b/a)**2

	f20 = s2+boa2*c2
	f02 = c2+boa2*s2
	f11 =  (1.-boa2)*cos(t)*sin(t)
	f00 = - b*b

	aa = f02
	bb = xx*f11
	cc = f00 + xx*xx*f20
	if(aa.eq.0.0) then
	  if(bb.eq.0.0) then
	    ierr = -1
	  else
	    y1 = -cc/bb
	    y2 = y1
	  endif
	else
	  d2 = bb*bb - aa*cc
	  if(d2.lt.0.0) then
	    ierr = -1
	  else
	    y1 = (-bb-sqrt(d2))/aa+yc
	    y2 = (-bb+sqrt(d2))/aa+yc
	  endif
	endif
	return
	end
	subroutine xellips(x1,x2,y,xc,yc,a,b,t,ierr)
c
	ierr = 0

	yy = y-yc

	s2 = sin(t)**2
	c2 = cos(t)**2
	boa2 = (b/a)**2

	f20 = s2+boa2*c2
	f02 = c2+boa2*s2
	f11 =  (1.-boa2)*cos(t)*sin(t)
	f00 = - b*b

	aa = f20
	bb = yy*f11
	cc = f00 + yy*yy*f02

	if(aa.eq.0.0) then
	  if(bb.eq.0.0) then
	    ierr = -1
	  else
	    x1 = -cc/bb + xc
	    x2 = y1
	  endif
	else
	  d2 = bb*bb - aa*cc
	  if(d2.lt.0.0) then
	    ierr = -1
	  else
	    x1 = (-bb-sqrt(d2))/aa + xc
	    x2 = (-bb+sqrt(d2))/aa + xc
	  endif
	endif
	return
	end
