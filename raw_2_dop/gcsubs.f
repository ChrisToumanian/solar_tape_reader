      function xtivity(imag,i,j,nix,njx)
c
c     compute the activity of the image at (i,j) in the x dir
c
      common / dbg / idbg, ludb
      common / msk / imb, ime, jmb, jme

      integer*2 imag(nix,njx)

      if(idbg.gt.3) write(ludb,*) 'xtivity (',i,',',j,') :'

      if(i.ge.imb .and. i.le.ime .and.
     &   j.ge.jmb .and. j.le.jme ) then

        xtivity =abs(       imag(i+1,j)-imag(i-1,j) 
     &               + 0.5*(imag(i+2,j)-imag(i-2,j)) )

      else
        xtivity = 0.0
      endif

      if(idbg.gt.3) write(ludb,*) xtivity

      return
      end
      function ytivity(imag,i,j,nix,njx)
c
c     compute the activity of the image at (i,j) in the y dir
c
      common / dbg / idbg, ludb
      common / msk / imb, ime, jmb, jme

      integer*2 imag(nix,njx)
 
      if(idbg.gt.3) write(ludb,*) 'ytivity (',i,',',j,') :'

      if(i.ge.imb .and. i.le.ime .and.
     &   j.ge.jmb .and. j.le.jme ) then

        ytivity =abs(       imag(i,j+1)-imag(i,j-1) 
     &               + 0.5*(imag(i,j+2)-imag(i,j-2)) )

      else
        ytivity = 0.0
      endif

      if(idbg.gt.3) write(ludb,*) ytivity

      return
      end
      subroutine cntroid(dd,cc,kk,kkx)
c
      dimension cc(-kkx:kkx)

      kx = 0
      cx = cc(kx)

      do 10 k = -kk, kk
        if(cc(k).gt.cx) then
          cx = cc(k)
          kx = k
         endif
   10 continue

      dd = topcmp(cc(kx-1),cc(kx),cc(kx+1),kx)

      return
      end
      function topcmp(y0,y1,y2,kx)
c
      x0 = kx - 1
      x1 = kx
      x2 = kx + 1

      f1 = (y0-y1)/(x0-x1)
      f2 = (y0-y2)/(x0-x2)

      a = (f1-f2)/(x1-x2)
      b = f1/(x0+x1)

      if(a.ne.0.0) then
        topcmp = -b/2.0/a + kx
      else
        topcmp = kx
	print *, 'topcmp :', y0, y1, y2
      endif

      return
      end
