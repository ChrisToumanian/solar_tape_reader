      subroutine fitslop(img,ix,jx,xc,yc,ni,nj,slop,xntrcp,regrsn)
c
c     fit the slope vs x of an image, where x = i-xc
c
c     SGK/29-May-89                                        Ver1.0/0/f77
c
      integer*2 img(ix,jx)
      real*8    sx, sx2, sxy, sy, sy2
      save      sx, sx2, xcp, ycp, iwp, jwp
      data      iwp, jwp /0, 0/
c
      is = xc-ni/2
      ie = is + ni
      js = yc-nj/2
      je = js + nj
      write(6,*) 'fitslop: xc, yc, ni, nj', xc, yc, ni, nj
      write(6,*) 'fitslop: is, ie, js, je', is, ie, js, je
c     
      if(iwp.eq.ni .and. jwp.eq.nj .and. 
     &   xcp.eq.xc .and. ycp.eq.yc      ) goto 100
      sx  = 0.0
      sx2 = 0.0
      do 50 i = is, ie
        x = i - xc
        sx  = sx  + x
        sx2 = sx2 + x*x
 50   continue
      sx  = sx /ni
      sx2 = sx2/ni - sx*sx
      iwp = ni
      jwp = nj
      xcp = xc
      ycp = yc
c
 100  continue
      sy  = 0.0
      sy2 = 0.0
      sxy = 0.0
      do 150 i = is, ie
        y = 0.0
        do 120 j = js, je
 120    y = y + img(i,j)
        y = y / nj
        x = i - xc
        sy  = sy  + y
        sy2 = sy2 + y*y
        sxy = sxy + x*y
 150  continue
      sy  = sy/ni
      sy2 = sy2/ni
      sy2 = sy2 - sy*sy
      sxy = sxy/ni
      write(6,*) 'fitslop: sx, sx2, sy, sy2, sxy'
      write(6,*) sx, sx2, sy, sy2, sxy
c     
      slop = (sxy - sx*sy)/sx2
      xntrcp = sy - slop*sx
      regrsn = slop*sqrt(sx2/sy2)
c
      return
      end
      
