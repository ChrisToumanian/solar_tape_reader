      subroutine cmpdop(imgred, imgblu, imgdop, ix, jx
     &,                 redellips, bluellips, xc, yc, fdop)
c
c     compute a dop from red & blu images
c
      integer*2 imgred(ix,jx), imgblu(ix,jx), imgdop(ix,jx)
      dimension redellips(5), bluellips(5)
     &,         uxr(4), uyr(4)
     &,         uxb(4), uyb(4)
     &,         xmgred(1024), xmgblu(1024)
c
      if(ix.gt.1024) stop '-STOP- : cmpdop buffer too small'
c
      dxr = redellips(3)-xc
      idr = dxr
      dxr = dxr - idr
      if(dxr.lt.0.0) then
        idr = idr-1
        dxr = 1.0 + dxr
      endif
c
      dyr = redellips(4)-yc
      jdr = dyr
      dyr = dyr - jdr
      if(dyr.lt.0.0) then
        jdr = jdr-1
        dyr = 1.0 + dyr
      endif
c
c     compute the interpolation kernels
c
      dxr2 = dxr*dxr
      dxr3 = dxr2*dxr
      uxr(1) = -0.5*dxr +     dxr2 - 0.5*dxr3
      uxr(2) =          - 2.5*dxr2 + 1.5*dxr3 + 1.0
      uxr(3) =  0.5*dxr + 2.0*dxr2 - 1.5*dxr3
      uxr(4) =          - 0.5*dxr2 +0.5 *dxr3
c
      dyr2 = dyr*dyr
      dyr3 = dyr2*dyr
      uyr(1) = -0.5*dyr +     dyr2 - 0.5*dyr3
      uyr(2) =          - 2.5*dyr2 + 1.5*dyr3 + 1.0
      uyr(3) =  0.5*dyr + 2.0*dyr2 - 1.5*dyr3
      uyr(4) =          - 0.5*dyr2 +0.5 *dyr3
c
c     limits for translation
c 
      idrm2 = idr-2
      is = max( 1,  -idrm2)
      ie = min(ix,ix-idr-2)
      jdrm2 = jdr-2
      js = max( 1,  -jdrm2)
      je = min(jx,jx-jdr-2)
c
c     ibidem for blue
c
      dxb = bluellips(3)-xc
      idb = dxb
      dxb = dxb - idb
      if(dxb.lt.0.0) then
        idb = idb-1
        dxb = 1.0 + dxb
      endif
c
      dyb = bluellips(4)-yc
      jdb = dyb
      dyb = dyb - jdb
      if(dyb.lt.0.0) then
        jdb = jdb-1
        dyb = 1.0 + dyb
      endif
c
c     compute the interpolation kernels
c
      dxb2 = dxb*dxb
      dxb3 = dxb2*dxb
      uxb(1) = -0.5*dxb +     dxb2 - 0.5*dxb3
      uxb(2) =          - 2.5*dxb2 + 1.5*dxb3 + 1.0
      uxb(3) =  0.5*dxb + 2.0*dxb2 - 1.5*dxb3
      uxb(4) =          - 0.5*dxb2 +0.5 *dxb3
c
      dyb2 = dyb*dyb
      dyb3 = dyb2*dyb
      uyb(1) = -0.5*dyb +     dyb2 - 0.5*dyb3
      uyb(2) =          - 2.5*dyb2 + 1.5*dyb3 + 1.0
      uyb(3) =  0.5*dyb + 2.0*dyb2 - 1.5*dyb3
      uyb(4) =          - 0.5*dyb2 +0.5 *dyb3
c
c     limits for translation
c 
      idbm2 = idb-2
      is = max(is,  -idbm2)
      ie = min(ie,ix-idb-2)
      jdbm2 = jdb-2
      js = max(js,  -jdbm2)
      je = min(je,jx-jdb-2)
c
c     now translate and compute the dop
c
      do 100 j = js, je
        jr0 = j + jdrm2
        jb0 = j + jdbm2

        do 40 i = is, ie
   	xmgred(i) = 0.0
   40   xmgblu(i) = 0.0

        do 50 kj = 1, 4
        jjr = jr0 + kj
        jjb = jb0 + kj
          do 50 ki = 1, 4
          ir0 = ki + idrm2
          ib0 = ki + idbm2
          do 50 i = is, ie
            iir = ir0 + i
            iib = ib0 + i
            xmgred(i) = xmgred(i) + imgred(iir,jjr)*uxr(ki)*uyr(kj)
            xmgblu(i) = xmgblu(i) + imgblu(iib,jjb)*uxb(ki)*uyb(kj)
   50   continue

        do 80 i = is , ie
        dif = xmgred(i) - xmgblu(i)
C        sum = 1.9
        sum = xmgred(i) + xmgblu(i)
c--??--        if (sum.eq.0.0) sum = 1.9
        dop = max(-1.0,min(+1.0,dif/sum))
   80   imgdop(i,j) = dop*fdop

  100 continue
c
      return
      end

