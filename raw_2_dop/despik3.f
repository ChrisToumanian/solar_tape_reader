      subroutine despike(img, ix, jx, ni, nii, npasx, xstd, stdn
     &,                  nspiks)
c
c     despike an image
c     input:
c       img(ix,jx) : image to despike (I*2)
c       ni         : 1/2 length of the weighted running mean filter
c       nii        : offset weighting factor for the running mean
c       npasx      : max no of passes
c       xstd       : st dev multiplicatif factor that defines a spike
c       stdn       : min value for st dev
c     output:
c       nspiks     : no of spikes removed
c
c     SGK/01-Jun-89/14-Jan-91                                    Ver3.1/0/F77
c     `despike failed at' printed to ludbg is idbg > 0
c
      parameter (ixx=1024, luerr=6)
      integer*2 img(ix,jx)
      integer*2 ispikes(ixx,ixx)
      real      x1(ixx), x2(ixx), x3(ixx)
c
      common    /dbg/ idbg, ludbg
      common    /msk/ ims, ime, jms, jme
      common    /spikes/ ispikes
c
      if(ix.gt.ixx) stop '-STOP DESPIKE: buffer too small-'
      is = ims+ni
      js = jms
      ie = ime-ni
      je = jme
c
      xni = 0.0
      do 20 ii = -ni, ni
   20 xni = xni + 1./(nii+ii*ii)
c
      nspiks = 0
      do 500 j = js, je
c
c     compute the 1st derivative
c
      x1(1) = 0.0
      do 40 i = max(ims,2), ime
   40 x1(i) = img(i-1,j) - img(i,j)
c
c     compute a weighted running mean of 1st derivative
c
      do 120 i = is, ie
        x2(i) = 0.0
        do 110 ii = -ni, ni
  110   x2(i) = x2(i) + x1(i+ii)/(nii+ii*ii)
        x2(i) = x2(i)/xni
  120 continue
c
c     subtract the weighted running mean from the 1st derivative
c
      do 123 i = is, ie
  123 x3(i) = x1(i)-x2(i)
c
      ipass = 0
c
c     loop on several passes
c
  125 ipass = ipass + 1
c
c     compute the st dev of that difference
c
      std = 0.0
      do 127 i = is, ie
 127  std = std + x3(i)*x3(i)
      std = sqrt(std/(ie-is+1))
c
c      ignore a constant line (std=0.0)
c
      if(std.eq.0.0) goto 500
      if(idbg.gt.2) write(ludbg,*) 'Despike: ipass, j, std='
     &,                            ipass, j, std
c
c     use the min value of stdev if larger than real one
c
      std = max(std, stdn)
c
c     double the std at the first pass (==> always 2 passes)
c
      if(ipass.eq.1) std = 2.0*std
      std = std*xstd
c
c     reentry point if no spikes at 1rst pass
c
 135  continue
c
c     now a spike is defined as:
c        abs(x3(i)) > std, but w/ i-i_lastspike >= ni
c                          and 'top/bottom' of the spike
c
      nspik=0
      ip = is - ni
      
      do 150 i = is, ie
        if(abs(x3(i)).le.std) goto 150
        if(i-ip.lt.ni) goto 150
        if(x3(i).gt.0.0 .and. x3(i+1).gt.x3(i)) goto 150
        if(x3(i).lt.0.0 .and. x3(i+1).lt.x3(i)) goto 150
c
c       we have a spike at (i,j)
c       we know that (i-1,j) is ok, but is (i+1,j)?
c       we can use the same criterion,
c       but we need to recompute the derivative
c
        if(idbg.gt.1) write(ludbg,*) 40+ipass, i, j, x1(i)
        nspik = nspik+1
        ip = i
        ii = 0
c
c       step forward until the derivative is ok
c
        if(idbg.gt.3) write(ludbg,*) 'img(i,j)=',img(i,j)
  140   ii = ii + 1
        if(ii.gt.npasx) then
          if (idbg.gt.0) 
     &         write(ludbg,*) 'Despike: despiking failed at',i,j
          nspik=nspik-1
          x1(i) = 0
          x1(i+1) = 0
          goto 150
        endif
        xx = (img(i-1,j) - img(i+ii,j))/(1.+ii)
        xx = xx - x2(i+ii)
        if(idbg.gt.3) then
          write(ludbg,*) 'ii, img(i-1,j), img(i+ii,j)'
     &,                   ii, img(i-1,j), img(i+ii,j)
          write(ludbg,*) 'xx, std:', xx ,std
        endif
        if(abs(xx).gt.std) goto 140
c
c       now linearly interpolate
c
        img(i,j) = ii/(1.+ii)*img(i-1,j) + 1./(1.+ii)*img(i+ii,j)
c
c       and recompute the 1st der at that point and the next one
c       as well as the 2nd derivative, and the diff x3
c
        x1(i) = img(i-1,j) - img(i,j)
        x1(i+1) = img(i,j) - img(i+1,j)
        do 145 iii=i-ni,i+ni+1
          x2(iii) = 0.0
          do 141 ii = -ni, ni
  141     x2(iii) = x2(iii) + x1(iii+ii)/(nii+ii*ii)
          x2(iii) = x2(iii)/xni
 145    x3(iii) = x1(iii) - x2(iii)
c
        ispikes(i,j) = ispikes(i,j)+1
  150 continue
c
c     repeat the process until no more spikes are detected
c
      if(idbg.gt.2.and.nspik.gt.0) write(ludbg,*) 
     &    'despike: ipass, j, nspik', ipass, j, nspik
      nspiks = nspiks + nspik
      if(nspik.gt.0.and.ipass.lt.npasx) goto 125
c
c     check again if 1rst pass(but don't need to recmp x2)
c
      if(ipass.eq.1) then
        std = std/2.0
        ipass = 2
        goto 135
      endif
      if(nspik.gt.0.and.idbg.gt.0) write(luerr,*) 'Despike: line '
     &,              j, ' not finished in', npasx, 'passes.'
c
      if(idbg.gt.2) then
        ns = nspiks-np
        if(ns.gt.0) write(ludbg,*) 'Despike: ==> #spikes=', ns
        np=nspiks
      endif
  500 continue
c
      return
      end
