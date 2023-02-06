	subroutine gcentr(imag, xc, yc, rad, rpx, rpy, nix, njx, ierr)
c
c       crude location of center and radius
c
c	SGK/10-Feb-88					Ver 1.1/0/FX
c
	parameter (kkx=50)

	integer*2 imag(nix,njx)
	dimension x0(-kkx:kkx), x1(-kkx:kkx)
     &,	          y0(-kkx:kkx), y1(-kkx:kkx)

	common /dbg/ idbg, ludb
	common /msk/ imb,ime,jmb,jme
	common /ctr/ aex, aey, aer, cx, cy, nx

	n = 1
5	do 10 k=-kkx,kkx
		x0(k) = 0.0
		x1(k) = 0.0
		y0(k) = 0.0
		y1(k) = 0.0
10	continue

	i0 = xc - rad + 0.5
	i1 = xc + rad + 0.5
	ic = xc + 0.5
	j0 = yc - rad + 0.5
	j1 = yc + rad + 0.5
	jc = yc + 0.5

	kn = min(kkx,i0-1)
	kx = min(kkx,nix-i1)

	do 100 j=jc-10,jc+10
	  do 100 k=-kn,kx
	    x0(k) = x0(k) + xtivity(imag,i0+k,j,nix,njx)
	    x1(k) = x1(k) + xtivity(imag,i1+k,j,nix,njx)
100	continue

	kn = min(kkx,j0-1)
	kx = min(kkx,njx-j1)

	do 200 i=ic-10,ic+10
	  do 200 k=-kn,kx
	    y0(k) = y0(k) + ytivity(imag,i,j0+k,nix,njx)
	    y1(k) = y1(k) + ytivity(imag,i,j1+k,nix,njx)
200	continue

	call cntroid(ppx0,x0,kkx,kkx)
	call cntroid(ppx1,x1,kkx,kkx)
	call cntroid(ppy0,y0,kkx,kkx)
	call cntroid(ppy1,y1,kkx,kkx)

	px0 = ppx0 + i0
	px1 = ppx1 + i1
	py0 = ppy0 + j0
	py1 = ppy1 + j1

	xpc = (px1+px0)/2.
	ypc = (py1+py0)/2.
	rpx = (px1-px0)/2.
	rpy = (py1-py0)/2.
	rpp = (px1-px0+py1-py0)/4.

	n = n + 1

 	if(idbg.gt.0) write(ludb,*) 'Gcentr: iteration #',n
 	if(idbg.gt.1) then
 	  write(ludb,*) ' x0 : '
 	  write(ludb,7000) x0
 	  write(ludb,*) ' x1 : '
 	  write(ludb,7000) x1
 	  write(ludb,*) ' y0 : '
 	  write(ludb,7000) y0
 	  write(ludb,*) ' y1 : '
 	  write(ludb,7000) y1
 7000	  format((1P10E8.1))
 	else if(idbg.gt.0) then
 	  write(ludb,*) ppx0,ppx1,ppy0,ppy1
 	  write(ludb,*) px0,px1,py0,py1
 	  write(ludb,*) xpc,ypc,rpp,rpx,rpy
 	endif


	ex = abs(xc-xpc)
	ey = abs(yc-ypc)
	er = abs(rad-rpp)

	xc = xpc
	yc = ypc
	rad =rpp
	if(n.gt.nx) goto 500

	if(ex.gt.aex .or. ey.gt.aey .or. er.gt.aer .or.
     &	   abs(ppx0).gt.cx .or. abs(ppy0).gt.cy    .or.
     &	   abs(ppx1).gt.cx .or. abs(ppy1).gt.cy        ) goto 5

	ierr = 0
	return

500	ierr = -1
	return
	end



