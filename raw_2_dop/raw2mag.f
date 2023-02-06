      program raw2mag
C
C     reduce raw images to magnetograms by:
C
C       subtracting a darkframe (same for red/blu)
C       despiking the images
C       computing the location of the limb (max of 1rst derivative)
C       fitting an ellipse (gen. orient., diff least square)
C       computing integrated intensity (red+/-, blu+/-, & mag)
C       translating all 4 images when computing the mag (bicubic interp)
C       note: mag(1,j) = j-1
C
C     SGK/27-Mar-91/Dec 28 1994                             Ver 2.0/0/F77
C     from raw2dop  SGK/26-May-89/24-Jan-91                 Ver 1.5/1/F77
C
C  WARNING: 1024x1024 built-in in the PARAMETER statment
C
      parameter (ix=1024, jx=1024, kx=ix*jx, npasx=5)
      parameter (nlmbx=3)
      integer*2 imgdrk(ix,jx)
     &,         imgredp(ix,jx), imgblup(ix,jx)
     &,         imgredm(ix,jx), imgblum(ix,jx)
     &,         imgmag(ix,jx)
      real*8 ellavg, ellstd
      dimension ellips0(5)
     &,         redpellips(5), blupellips(5)
     &,         redmellips(5), blumellips(5)
     &,         xmgellips(5)
     &,         ellavg(5), ellstd(5), ib(2,ix)
     &,         niterx(npasx), ntheta(npasx), dr(npasx), xnstd(npasx)
     &,         acurels(5,npasx)
      character*60 dfn, ifn, lfn
      parameter (luf=1, lured=2, lublu=3, lumag=4, lui=5, luo=6
     &,          lulmb=9)
C
      common /dbg/ idbg, ludbg
      common /msk/ imskn, imskx, jmskn, jmskx
      common /ctr/ aex, aey, aer, cx, cy, nx
C
C WARNING : 1024x1024 built-in in the DATA statments:
C
      data idbg,ludbg     /0,6/
      data xc0, yc0, rad0 /512., 512., 450./
      data aex, aey, aer  /3*.5/
      data cx, cy, nx     /2*5.,10/
      data ellavg, ellstd /10*0.0/
c-??-      data imskn, imskx, jmskn, jmskx /30,994,30,994/
c-??-      data imskn, imskx, jmskn, jmskx /40,994,30,994/
      data imskn, imskx, jmskn, jmskx /100,1014,60,994/
      data xcd, ycd, fmag  /512., 512., 30000.0/
C
      read (lui,*) idbg, ludbg
C
C     Read the dark frame file name, and read the dark frame
C
      read(lui,'(A)') dfn
      if(dfn.ne.'-') then
      open(luf,file=dfn,status='old',form='unformatted'
     &,    access='direct', recl= 2*kx, err=901)
        read(luf,rec=1) imgdrk
        close(luf)
      endif
C
C     Read the despiking parameters
C
      read (lui,*) nidspk, niidspk, npasxdspk, xstdspk, stdndspk
C
C     Read the fitting parameters
C
      read (lui,*) nx, xc0, yc0, rad0
      read (lui,*) aex, aey, aer
      read (lui,*) ellips0
      read (lui,*) npass
      if(npass.gt.npasx) goto 902
      do 10 ipass = 1, npass
        read(lui,*) niterx(ipass), ntheta(ipass), dr(ipass)
     &,             xnstd(ipass)
        read(lui,*) (acurels(i,ipass),i=1,5)
   10 continue
C
C     read the translation center location, the mag scaling factor and
C     which formula to use
C
      read (lui,*) xcd, ycd, fmag, iform
C
C     Echo on output the input coeff
C
      write(luo,*) 'Raw to MAG'
      write(luo,*) '---------- Ver 2.0/0'
      write(luo,*)
      if(dfn.eq.'-') then
        write(luo,*) 'No dark frame subtracted'
      else
        write(luo,*) 'Dark frame from file :',dfn
      endif
      if(npasxdspk.gt.0.0) then
        write(luo,*) 'Despiking parameters:'
        write(luo,*) nidspk, niidspk, npasxdspk, xstdspk, stdndspk
      else
        write(luo,*) 'Images NOT despiked'
      endif
      if(nx.gt.0) then
        write(luo,*) 'First guess for  Gcentr :', xc0, yc0, rad0, nx
        write(luo,*) 'Gcentr fitting acuracy  :', aex, aey, aer
      endif
      write(luo,*) 'First guess for ellipse :',ellips0
      write(luo,*) npass,' passes :'
      do 15 ipass =1 ,npass
        write(luo,*) 'pass #',ipass,niterx(ipass),' iterations,'
     &,              ntheta(ipass),' points, and radial error of'
     &,              dr(ipass),'pixels (',xnstd(ipass),' std)'
        write(luo,*) 'asbolute accuracy :',(acurels(i,ipass),i=1,5)
   15 continue
      write(luo,*) 'Centering, after translation at :',xcd,ycd
      write(luo,*) 'Mag scaling factor :', fmag
      write(luo,*) 'Mag computation formulation:', iform
C
C     Endless loop on file names
C
      nimg = 0
      ntot = 0
      nmag = 0
   20 redpxtens = 0.0
      redmxtens = 0.0
C
C     read the 'red' images (+ -)
C
      read (lui,'(A)',end=500) ifn
      open(luf,file=ifn,status='old',form='unformatted'
     &,    access='direct', recl= 2*kx, err=903)
C      read(luf,rec=1,err=904,end=904) imgredp
      read(luf,err=904,end=904) imgredp
      close(luf)
      write(luo,*) ifn
      write(lured,*) ifn
      read (lui,'(A)',end=500) ifn
      open(luf,file=ifn,status='old',form='unformatted'
     &,    access='direct', recl= 2*kx, err=903)
C      read(luf,rec=1,err=904,end=904) imgredm
      read(luf,err=904,end=904) imgredm
      close(luf)
      write(luo,*) ifn
      write(lured,*) ifn
      ntot = ntot + 2
C
C     Prepare the 'red' images
C
C     Subtract the dark frame
C
      if(dfn.ne.'-') then
        do 30 j=1,jx
        do 30 i=1,ix
 30     imgredp(i,j)=imgredp(i,j)-imgdrk(i,j)
        do 31 j=1,jx
        do 31 i=1,ix
 31     imgredm(i,j)=imgredm(i,j)-imgdrk(i,j)
      endif
C
C     despike the red images
C
      if(npasxdspk.gt.0) 
     &  call despike(imgredp, ix, jx, nidspk, niidspk, npasxdspk
     &,              xstdspk, stdndspk, nredpspiks)
      if(npasxdspk.gt.0) 
     &  call despike(imgredm, ix, jx, nidspk, niidspk, npasxdspk
     &,              xstdspk, stdndspk, nredmspiks)
C
C     get ellipse location
C
      call gelloc(imgredp, redpellips
     &,           xc0, yc0, rad0
     &,           ellips0, acurels, niterx, dr, ntheta, xnstd
     &,           npass, niterp, ix, jx)
      call gelloc(imgredm, redmellips
     &,           xc0, yc0, rad0
     &,           ellips0, acurels, niterx, dr, ntheta, xnstd
     &,           npass, niterm, ix, jx)

C
C     compute intensity and stat only if converged
C
      if(niterp.gt.0) then
        call defellib(redpellips, ib, ix, jx)
        redpxtens = xint(imgredp, ib, ix, jx)
        nimg = nimg + 1
      endif
      if(niterm.gt.0) then
        call defellib(redmellips, ib, ix, jx)
        redmxtens = xint(imgredm, ib, ix, jx)
        nimg = nimg + 1
      endif
 61   continue
C
C     output the result
C
      write(lured,*) niterp, redpellips, redpxtens, nredpspiks
      write(lured,*) niterm, redmellips, redmxtens, nredmspiks
C
C     now ibidem for the 'blu' (+ -)
C
  120 blupxtens = 0.0
      blumxtens = 0.0
      read (lui,'(A)',end=500) ifn
      open(luf,file=ifn,status='old',form='unformatted'
     &,    access='direct', recl= 2*kx, err=913)
C      read(luf,rec=1,err=914,end=914) imgblup
      read(luf,err=914,end=914) imgblup
      close(luf)
      write(luo,*) ifn
      write(lublu,*) ifn
      read (lui,'(A)',end=500) ifn
      open(luf,file=ifn,status='old',form='unformatted'
     &,    access='direct', recl= 2*kx, err=913)
C      read(luf,rec=1,err=914,end=914) imgblum
      read(luf,err=914,end=914) imgblum
      close(luf)
      write(luo,*) ifn
      write(lublu,*) ifn
      ntot = ntot + 2
C
C     Subtract the dark frame
C
      if(dfn.ne.'-') then
        do 130 j=1,jx
        do 130 i=1,ix
 130    imgblup(i,j)=imgblup(i,j)-imgdrk(i,j)
        do 131 j=1,jx
        do 131 i=1,ix
 131    imgblum(i,j)=imgblum(i,j)-imgdrk(i,j)
      endif
C
C     despike the images
C
      if(npasxdspk.gt.0) 
     &  call despike(imgblup, ix, jx, nidspk, niidspk, npasxdspk
     &,              xstdspk, stdndspk, nblupspiks)
      if(npasxdspk.gt.0) 
     &  call despike(imgblum, ix, jx, nidspk, niidspk, npasxdspk
     &,              xstdspk, stdndspk, nblumspiks)
C
C     get ellipse location
C
      call gelloc(imgblup, blupellips
     &,           xc0, yc0, rad0
     &,           ellips0, acurels, niterx, dr, ntheta, xnstd
     &,           npass, niterp, ix, jx)
      call gelloc(imgblum, blumellips
     &,           xc0, yc0, rad0
     &,           ellips0, acurels, niterx, dr, ntheta, xnstd
     &,           npass, niterm, ix, jx)
C
C     compute intensity, stat and limb only if converged
C
      if(niterp.gt.0) then
        call defellib(blupellips, ib, ix, jx)
        blupxtens = xint(imgblup, ib, ix, jx)
        nimg = nimg + 1
      endif
      if(niterm.gt.0) then
        call defellib(blumellips, ib, ix, jx)
        blumxtens = xint(imgblum, ib, ix, jx)
        nimg = nimg + 1
      endif
 161  continue
C
C     output the result
C
      write(lublu,*) niterp, blupellips, blupxtens, nblupspiks
      write(lublu,*) niterm, blumellips, blumxtens, nblumspiks
C
C     read the mag file name
C
 220  continue
      read (lui,'(A)',end=500) ifn
C
C     go to the next set of images if centering failed
C
      if (redpxtens.eq.0.0 .or. blupxtens.eq.0.0 .or.
     &    redmxtens.eq.0.0 .or. blumxtens.eq.0.0) goto 20
      write(luo,*) ifn
C
C     now lets compute the magnetogram
C
      call cmpmag(imgredp, imgredm, imgblup, imgblum, imgmag, ix, jx
     &,           iform
     &,           redpellips, redmellips, blupellips, blumellips
     &,           xcd, ycd, fmag)
C
C     compute mag's integrated intensity
C
      xmgellips(1) = (redpellips(1) + blupellips(1) 
     &              + redmellips(1) + blumellips(1))/4.
      xmgellips(2) = (redpellips(2) + blupellips(2)
     &              + redmellips(2) + blumellips(2))/4.
      xmgellips(3) = xcd
      xmgellips(4) = ycd
      xmgellips(5) = (redpellips(5) + blupellips(5)
     &              + redmellips(5) + blumellips(5))/4.
      call defellib(xmgellips, ib, ix, jx)
      xmgxtens = xint(imgmag, ib, ix, jx)
C
C     add mag(1,j) = j
C
      do 210 j=1,jx
 210  imgmag(1,j) = j-1
C
C     and save it
C
      open(luf,file=ifn,status='new',form='unformatted'
     &,    access='direct', recl= 2*kx, err=923)
      write(luf,rec=1,err=924) imgmag
      close(luf)
      nmag = nmag + 1
C
      write(lumag,*) ifn
      write(lumag,*) niter, xmgellips, xmgxtens
C
C     go back to the next pair of images
C
      goto 20
C
  500 continue
C
C     print the stats
C
      write(luo,*)
      write(luo,*) ' Center ',nimg,' out of ',ntot,' images(s)'
C
      write(luo,*) nmag, ' magnetogram(s) computed'
C     [can't pass nomag>127 --> leave it to 0, or 1]
      nmag = min(1,nmag)
      call exit(nmag)
C
  901 write(luo,*) 'Could not open the dark frame file :',dfn
      write(luo,*) '-STOP : can not open dark frame-'
      call exit(-1)
C
  902 write(luo,*) ' No of passes too large, max :',npasx
      write(luo,*) '-STOP : No of passes too large-'
      call exit(-2)
C
  903 write(luo,*) 'Could not open :',ifn
      goto 120
C
  904 write(luo,*) 'Reading error, while reading from file :',ifn
      close(luf)
      goto 120
C
  913 write(luo,*) 'Could not open :',ifn
      goto 220
C
  914 write(luo,*) 'Reading error, while reading from file :',ifn
      close(luf)
      goto 220
C
  923 write(luo,*) 'Could not open :',ifn
      goto 20
C
  924 write(luo,*) 'Writing error, while writing file :',ifn
      close(luf)
      goto 20
C
 992  write(luo,*) 'Cannot open red limb file :', lfn
      goto 61
C
 993  write(luo,*) 'Cannot open blu limb file :', lfn
      goto 161
      end
      subroutine gelloc(img, ellips
     &,                 xc0, yc0, rad0
     &,                 ellips0, acurels, niterx, dr, ntheta, xnstd
     &,                 npass, niter,  ix, jx)
C
      integer*2 img(ix,jx)
      dimension ellips(5), ellips0(5), acurell(5)
     &,         niterx(*), ntheta(*), dr(*), xnstd(*), acurels(5,*)
C
      common /dbg/ idbg, ludbg
      common /msk/ imskn, imskx, jmskn, jmskx
      common /ctr/ aex, aey, aer, cx, cy, nx
C
C     get crude center and limb location
C
      if(nx.gt.0) then
        xc = xc0
        yc = yc0
        rad = rad0
        call gcentr(img, xc, yc, rad, rpx, rpy, ix, jx, ierr)
CD        if(idbg.gt.0) then
CD          write(ludbg,*) 'Gcentr result:'
CD          write(ludbg,*) rpx, rpy, xc, yc, rad, ierr
CD        endif
        ellips0(1) = rpx
        ellips0(2) = rpy
        ellips0(3) = xc
        ellips0(4) = yc
        if(ierr.lt.0) then
          niter = -99
          return
        endif
      endif
C
C     fit an ellipse to the limb, in npass passes
C
      do 40 i=1,5
   40 ellips(i) = ellips0(i)
C
      do 50 ipass = 1, npass
        do 45 i=1,5
   45   acurell(i)=acurels(i,ipass)
        call centroid(img, ix, jx, ellips, acurell
     &,               niterx(ipass), dr(ipass)
     &,               imskn, imskx, jmskn, jmskx
     &,               ntheta(ipass), niter, xnstd(ipass))
CD        if(idbg.gt.0) then
CD          write(ludbg,*) 'Centroid result at pass#', ipass
CD          write(ludbg,*) ellips
CD        endif
   50 continue
      return
      end
