      subroutine cmpmag(imgredp, imgredm, imgblup, imgblum, imgmag
     &,                 ix, jx, iform
     &,                 redpellips, redmellips, blupellips, blumellips
     &,                 xc,yc, fmag)
c
c     compute a mag from red+, red-, blu+ & blu- images
c iform# 0:
c     mag = {red+ + blu- - (red- + blu+)}/sum
c iform# 1:
c     mag = (red+ - blu+)/(red+ + blu+) - (red- - blu-)/(red- + blu-)
c
      integer*2 imgredp(ix,jx), imgredm(ix,jx)
     &,         imgblum(ix,jx), imgblup(ix,jx)
     &,         imgmag(ix,jx)
      dimension redpellips(5), redmellips(5)
     &,         blupellips(5), blumellips(5)
     &,         uxrp(4), uyrp(4)
     &,         uxrm(4), uyrm(4)
     &,         uxbp(4), uybp(4)
     &,         uxbm(4), uybm(4)
     &,         xmgplu(1024), xmgmin(1024)
     &,         xmgsmp(1024), xmgsmm(1024)
c
      if(ix.gt.1024) stop '-STOP- : cmpdop buffer too small'
c
      dxrp = redpellips(3)-xc
      idrp = dxrp
      dxrp = dxrp - idrp
      if(dxrp.lt.0.0) then
        idrp = idrp-1
        dxrp = 1.0 + dxrp
      endif
c
      dyrp = redpellips(4)-yc
      jdrp = dyrp
      dyrp = dyrp - jdrp
      if(dyrp.lt.0.0) then
        jdrp = jdrp-1
        dyrp = 1.0 + dyrp
      endif
c
      dxrm = redmellips(3)-xc
      idrm = dxrm
      dxrm = dxrm - idrm
      if(dxrm.lt.0.0) then
        idrm = idrm-1
        dxrm = 1.0 + dxrm
      endif
c
      dyrm = redmellips(4)-yc
      jdrm = dyrm
      dyrm = dyrm - jdrm
      if(dyrm.lt.0.0) then
        jdrm = jdrm-1
        dyrm = 1.0 + dyrm
      endif
c
c     compute the interpolation kernels
c
      dxr  = dxrp
      dxr2 = dxr*dxr
      dxr3 = dxr2*dxr
      uxrp(1) = -0.5*dxr +     dxr2 - 0.5*dxr3
      uxrp(2) =          - 2.5*dxr2 + 1.5*dxr3 + 1.0
      uxrp(3) =  0.5*dxr + 2.0*dxr2 - 1.5*dxr3
      uxrp(4) =          - 0.5*dxr2 +0.5 *dxr3
c
      dxr  = dxrm
      dxr2 = dxr*dxr
      dxr3 = dxr2*dxr
      uxrm(1) = -0.5*dxr +     dxr2 - 0.5*dxr3
      uxrm(2) =          - 2.5*dxr2 + 1.5*dxr3 + 1.0
      uxrm(3) =  0.5*dxr + 2.0*dxr2 - 1.5*dxr3
      uxrm(4) =          - 0.5*dxr2 +0.5 *dxr3
c
      dyr  = dyrp
      dyr2 = dyr*dyr
      dyr3 = dyr2*dyr
      uyrp(1) = -0.5*dyr +     dyr2 - 0.5*dyr3
      uyrp(2) =          - 2.5*dyr2 + 1.5*dyr3 + 1.0
      uyrp(3) =  0.5*dyr + 2.0*dyr2 - 1.5*dyr3
      uyrp(4) =          - 0.5*dyr2 +0.5 *dyr3
c
      dyr  = dyrm
      dyr2 = dyr*dyr
      dyr3 = dyr2*dyr
      uyrm(1) = -0.5*dyr +     dyr2 - 0.5*dyr3
      uyrm(2) =          - 2.5*dyr2 + 1.5*dyr3 + 1.0
      uyrm(3) =  0.5*dyr + 2.0*dyr2 - 1.5*dyr3
      uyrm(4) =          - 0.5*dyr2 +0.5 *dyr3
c
c     limits for translation
c 
      idrpm2 = idrp-2
      is = max( 1,  -idrpm2)
      ie = min(ix,ix-idrp-2)
      jdrpm2 = jdrp-2
      js = max( 1,  -jdrpm2)
      je = min(jx,jx-jdrp-2)
c
      idrmm2 = idrm-2
      is = max(is,  -idrmm2)
      ie = min(ie,ix-idrm-2)
      jdrmm2 = jdrm-2
      js = max(js,  -jdrmm2)
      je = min(je,jx-jdrm-2)
c
c     ibidem for blue
c
      dxbp = blupellips(3)-xc
      idbp = dxbp
      dxbp = dxbp - idbp
      if(dxbp.lt.0.0) then
        idbp = idbp-1
        dxbp = 1.0 + dxbp
      endif
c
      dxbm = blumellips(3)-xc
      idbm = dxbm
      dxbm = dxbm - idbm
      if(dxbm.lt.0.0) then
        idbm = idbm-1
        dxbm = 1.0 + dxbm
      endif
c
      dybp = blupellips(4)-yc
      jdbp = dybp
      dybp = dybp - jdbp
      if(dybp.lt.0.0) then
        jdbp = jdbp-1
        dybp = 1.0 + dybp
      endif
c
      dybm = blumellips(4)-yc
      jdbm = dybm
      dybm = dybm - jdbm
      if(dybm.lt.0.0) then
        jdbm = jdbm-1
        dybm = 1.0 + dybm
      endif
c
c     compute the interpolation kernels
c     
      dxb  = dxbp
      dxb2 = dxb*dxb
      dxb3 = dxb2*dxb
      uxbp(1) = -0.5*dxb +     dxb2 - 0.5*dxb3
      uxbp(2) =          - 2.5*dxb2 + 1.5*dxb3 + 1.0
      uxbp(3) =  0.5*dxb + 2.0*dxb2 - 1.5*dxb3
      uxbp(4) =          - 0.5*dxb2 +0.5 *dxb3
c
      dxb  = dxbm
      dxb2 = dxb*dxb
      dxb3 = dxb2*dxb
      uxbm(1) = -0.5*dxb +     dxb2 - 0.5*dxb3
      uxbm(2) =          - 2.5*dxb2 + 1.5*dxb3 + 1.0
      uxbm(3) =  0.5*dxb + 2.0*dxb2 - 1.5*dxb3
      uxbm(4) =          - 0.5*dxb2 +0.5 *dxb3
c
      dyb  = dybp
      dyb2 = dyb*dyb
      dyb3 = dyb2*dyb
      uybp(1) = -0.5*dyb +     dyb2 - 0.5*dyb3
      uybp(2) =          - 2.5*dyb2 + 1.5*dyb3 + 1.0
      uybp(3) =  0.5*dyb + 2.0*dyb2 - 1.5*dyb3
      uybp(4) =          - 0.5*dyb2 +0.5 *dyb3
c
      dyb  = dybm
      dyb2 = dyb*dyb
      dyb3 = dyb2*dyb
      uybm(1) = -0.5*dyb +     dyb2 - 0.5*dyb3
      uybm(2) =          - 2.5*dyb2 + 1.5*dyb3 + 1.0
      uybm(3) =  0.5*dyb + 2.0*dyb2 - 1.5*dyb3
      uybm(4) =          - 0.5*dyb2 +0.5 *dyb3
c
c     limits for translation
c 
      idbpm2 = idbp-2
      is = max(is,  -idbpm2)
      ie = min(ie,ix-idbp-2)
      jdbpm2 = jdbp-2
      js = max(js,  -jdbpm2)
      je = min(je,jx-jdbp-2)
c
      idbmm2 = idbm-2
      is = max(is,  -idbmm2)
      ie = min(ie,ix-idbm-2)
      jdbmm2 = jdbm-2
      js = max(js,  -jdbmm2)
      je = min(je,jx-jdbm-2)
c
c     now translate and compute the mag
c
      do 100 j = js, je
        jrp0 = j + jdrpm2
        jrm0 = j + jdrmm2
        jbp0 = j + jdbpm2
        jbm0 = j + jdbmm2

        do 40 i = is, ie
          xmgplu(i) = 0.0
          xmgmin(i) = 0.0
 40     continue

        if (iform.ne.0) then
          do 45 i = is, ie
            xmgsmp(i) = 0.0
            xmgsmm(i) = 0.0
 45       continue
        endif

        do 50 kj = 1, 4
        jjrp = jrp0 + kj
        jjrm = jrm0 + kj
        jjbp = jbp0 + kj
        jjbm = jbm0 + kj
          do 50 ki = 1, 4
          irp0 = ki + idrpm2
          irm0 = ki + idrmm2
          ibp0 = ki + idbpm2
          ibm0 = ki + idbmm2
          do 50 i = is, ie
            iirp = irp0 + i
            iirm = irm0 + i
            iibp = ibp0 + i
            iibm = ibm0 + i

            xrp = imgredp(iirp,jjrp)*uxrp(ki)*uyrp(kj)
            xrm = imgredm(iirm,jjrm)*uxrm(ki)*uyrm(kj)
            xbm = imgblum(iibm,jjbm)*uxbm(ki)*uybm(kj)
            xbp = imgblup(iibp,jjbp)*uxbp(ki)*uybp(kj)

            if(iform.eq.0) then
              xmgplu(i) = xmgplu(i) + xrp + xbm
              xmgmin(i) = xmgmin(i) + xrm + xbp
            else
              xmgplu(i) = xmgplu(i) + xrp - xbp
              xmgsmp(i) = xmgsmp(i) + xrp + xbp
              xmgmin(i) = xmgmin(i) + xrm - xbm
              xmgsmm(i) = xmgsmm(i) + xrm + xbm
            endif
   50   continue

        if(iform.eq.0) then
          do 80 i = is , ie
            dif = xmgplu(i) - xmgmin(i)
            sum = xmgplu(i) + xmgmin(i)
            xmg = max(-1.0,min(+1.0,dif/sum))
 80       imgmag(i,j) = xmg*fmag
        else
          do 90 i = is , ie
            xmg = xmgplu(i)/xmgsmp(i) - xmgmin(i)/xmgsmm(i)
            xmg = max(-1.0,min(+1.0,xmg))
 90       imgmag(i,j) = xmg*fmag
        endif

  100 continue
c
      return
      end

