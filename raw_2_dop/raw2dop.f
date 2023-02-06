      program raw2dop
C
C     reduce raw images to dopplergrams by:
C
C       subtracting a darkframe (same for red/blu)
C       despiking the images
C       computing the location of the limb (max of 1rst derivative)
C       fitting an ellipse (gen. orient., diff least square)
C       computing integrated intensity (red, blu, & dop)
C       translating both images when computing the dop (bicubic interp)
C       compute the X-dir and Y-dir limb profile of each image
C       and compute slope vs 'x' at image center (512 by 8)
C       note: dop(1,j) = j-1
C       reading in: mask spec, limb dir and stat file names
C       lmbdir='-' --> do not compute limbs
C
C     SGK/26-May-89/24-Jan-91/26-Sep-95                         Ver 1.5/2/F77
C
C  WARNING: 1024x1024 built-in in the PARAMETER statment
C
      parameter (ix=1024, jx=1024, kx=ix*jx, npasx=5)
      parameter (nlmbx=3)
      integer*2 imgdrk(ix,jx), imgred(ix,jx), imgblu(ix,jx)
     &,         imgdop(ix,jx)
      real*8 ellavg, ellstd
      dimension ellips0(5)
     &,         redellips(5), bluellips(5), dopellips(5)
     &,         ellavg(5), ellstd(5), ib(2,ix)
     &,         niterx(npasx), ntheta(npasx), dr(npasx), xnstd(npasx)
     &,         acurels(5,npasx)
     &,         xlmb(ix,-nlmbx:nlmbx), ylmb(jx,-nlmbx:nlmbx)
      character*80 dfn, ifn, lfn, redstatfn, blustatfn, dopstatfn
      character*80 xdir, lmbdir
      external xdir
      parameter (luf=1, lured=2, lublu=3, ludop=4, lui=5, luo=6
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
C---- data imskn, imskx, jmskn, jmskx /30,994,30,994/   --??--
C---- data imskn, imskx, jmskn, jmskx /40,994,30,994/ --18Nov93--
      data imskn, imskx, jmskn, jmskx /55,994,30,994/
      data xcd, ycd,fdop  /512., 512., 30000.0/
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
C     read `mask' to avoid detector edges
C
      read (lui,*) imskn, imskx, jmskn, jmskx
C
C     read the translation center location, and the doppler scaling factor
C
      read (lui,*) xcd, ycd, fdop
C
      read (lui,'(a)') lmbdir
      read (lui,'(a)') redstatfn
      read (lui,'(a)') blustatfn
      read (lui,'(a)') dopstatfn
C
      open(lured, file=redstatfn, err=930)
      open(lublu, file=blustatfn, err=931)
      open(ludop, file=dopstatfn, err=932)
C
C     Echo on output the input coeff
C
      write(luo,*) 'Raw to Dop'
      write(luo,*) '---------- Ver 1.5/2'
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
      write(luo,*) 'Mask: ', imskn, imskx, jmskn, jmskx
      write(luo,*) 'Centering, after translation at :',xcd,ycd
      write(luo,*) 'Doppler scaling factor :',fdop
      if (lmbdir.ne.'-')
     &     write(luo,999) ' Limb dir: ',lmbdir
C
C     Endless loop on file names
C
      nimg = 0
      ntot = 0
      ndop = 0
   20 redxtens = 0.0
      read (lui,'(A)',end=500) ifn
      open(luf,file=ifn,status='old',form='unformatted'
     &,    access='direct', recl= 2*kx, err=903)
C     read(luf,rec=1,err=904,end=904) imgred
      read(luf,err=904,end=904) imgred
      close(luf)
      ntot = ntot + 1
C
C     Prepare the 'red' image
C
C     Subtract the dark frame
C
      if(dfn.ne.'-') then
        do 30 j=1,jx
        do 30 i=1,ix
   30   imgred(i,j)=imgred(i,j)-imgdrk(i,j)
      endif
C
C     despike the red image
C
      if(npasxdspk.gt.0) 
     &  call despike(imgred, ix, jx, nidspk, niidspk, npasxdspk
     &,              xstdspk, stdndspk, nredspiks)
C
C     get ellipse location
C
      call gelloc(imgred, redellips
     &,           xc0, yc0, rad0
     &,           ellips0, acurels, niterx, dr, ntheta, xnstd
     &,           npass, niter, ix, jx)
C
C     compute intensity and stat only if converged
C
      if(niter.gt.0) then
        nimg = nimg + 1
        do 60 i=1,5
          ellavg(i) = ellavg(i) + redellips(i)
          ellstd(i) = ellstd(i) + redellips(i)*redellips(i)
   60   continue
        call defellib(redellips, ib, ix, jx)
        redxtens = xint(imgred, ib, ix, jx)
C
        if (lmbdir.ne.'-') then
C
C     compute the limb and save it
C
          call cmplmb(imgred, ix, jx
     &,               redellips, xcd, ycd, xlmb, ylmb, nlmbx)
          lfn = xdir(ifn,lmbdir)
          open (lulmb, file=lfn, status='new', form='unformatted'
     &,         err=992)
          write(lulmb) xlmb, ylmb
          close(lulmb)
        endif
      endif
 61   continue
C
C     output the result
C
      write(lured,*) ifn
      write(lured,*) niter, redellips, redxtens, nredspiks
C
      write(luo,*) ifn
      write(luo,*) niter, redellips, redxtens, nredspiks
C
C     now ibidem for the 'blu'
C
  120 bluxtens = 0.0
      read (lui,'(A)',end=500) ifn
      open(luf,file=ifn,status='old',form='unformatted'
     &,    access='direct', recl= 2*kx, err=913)
C     read(luf,rec=1,err=914,end=914) imgblu
      read(luf,err=914,end=914) imgblu
      close(luf)
      ntot = ntot + 1
C
C     Subtract the dark frame
C
      if(dfn.ne.'-') then
        do 130 j=1,jx
        do 130 i=1,ix
  130   imgblu(i,j)=imgblu(i,j)-imgdrk(i,j)
      endif
C
C     despike the image
C
      if(npasxdspk.gt.0) 
     &  call despike(imgblu, ix, jx, nidspk, niidspk, npasxdspk
     &,              xstdspk, stdndspk, nbluspiks)
C
C     get ellipse location
C
      call gelloc(imgblu, bluellips
     &,           xc0, yc0, rad0
     &,           ellips0, acurels, niterx, dr, ntheta, xnstd
     &,           npass, niter, ix, jx)
C
C     compute intensity, stat and limb only if converged
C
      if(niter.gt.0) then
        nimg = nimg + 1
        do 160 i=1,5
          ellavg(i) = ellavg(i) + bluellips(i)
          ellstd(i) = ellstd(i) + bluellips(i)*bluellips(i)
  160   continue
        call defellib(bluellips, ib, ix, jx)
        bluxtens = xint(imgblu, ib, ix, jx)
C
C
        if (lmbdir.ne.'-') then
C     compute the limb and save it
C
          call cmplmb(imgblu, ix, jx
     &,               bluellips, xcd, ycd, xlmb, ylmb, nlmbx)
          lfn = xdir(ifn, lmbdir)
          open (lulmb, file=lfn, status='new', form='unformatted'
     &,         err=993)
          write(lulmb) xlmb, ylmb
          close(lulmb)
        endif
      endif
 161  continue
C
C     output the result
C
      write(lublu,*) ifn
      write(lublu,*) niter, bluellips, bluxtens, nbluspiks
C
      write(luo,*) ifn
      write(luo,*) niter, bluellips, bluxtens, nbluspiks
C
C     read the dop file name
C
 220  continue
      read (lui,'(A)',end=500) ifn
C
C     go to the next pair of images if centering failed
C
      if (redxtens.eq.0.0 .or. bluxtens.eq.0.0) goto 20
      write(luo,*) ifn
C
C     now lets compute the dopplergram
C
      call cmpdop(imgred, imgblu, imgdop, ix, jx
     &,           redellips, bluellips, xcd, ycd, fdop)
C
C     compute dop's integrated intensity
C
      dopellips(1) = (redellips(1) + bluellips(1))/2.
      dopellips(2) = (redellips(2) + bluellips(2))/2.
      dopellips(3) = xcd
      dopellips(4) = ycd
      dopellips(5) = (redellips(5) + bluellips(5))/2.
      call defellib(dopellips, ib, ix, jx)
      dopxtens = xint(imgdop, ib, ix, jx)
      call fitslop(imgdop, ix, jx, xcd, ycd, 512, 8
     &,            slop, xntrcp, regrsn)
C
C     change sign of dop if req'd
C
      if(slop.gt.0.0) then
        write(luo,*) 'WARNING: slope > 0 ==> swap red/blu'
        do 200 j=1,jx
        do 200 i=1,ix
 200    imgdop(i,j)=-imgdop(i,j)
        dopxtens = -dopxtens
        slop = -slop
        xntrcp = -xntrcp
        regrsn = -regrsn
      endif
C
C     add dop(1,j) = j
C
      do 210 j=1,jx
 210  imgdop(1,j) = j-1
C
C     and save it
C
      open(luf,file=ifn,status='new',form='unformatted'
     &,    access='direct', recl= 2*kx, err=923)
      write(luf,rec=1,err=924) imgdop
      close(luf)
      ndop = ndop + 1
C
      write(ludop,*) ifn
      write(ludop,*) niter, dopellips, dopxtens, slop, xntrcp, regrsn
C
      write(luo,*) niter, dopellips, dopxtens, slop, xntrcp, regrsn
C
C     go back to the next pair of images
C
      goto 20
C
  500 continue
C
C     print the stats
C
      if(nimg.gt.1) then
CVD$  NOVECTOR
      do 510 i=1,5
        ellavg(i)=ellavg(i)/nimg
        ellstd(i)=sqrt(ellstd(i)/nimg-ellavg(i)*ellavg(i))
  510 continue
      write(luo,*)
      write(luo,*) ' Stats on ',nimg,' out of ',ntot,' images(s)'
      write(luo,'(a,5f12.4)') 'avg :',ellavg
      write(luo,'(a,5f12.4)') 'std :',ellstd
      endif
C
      r = 200.*ndop/ntot
      write(luo,*) ndop, ' dopplergram(s) computed, ',r,'%'
C     [can't pass nodop>127 --> leave it to 0, or 1]
      ndop = min(1,ndop)
      call exit(ndop)
C
  901 write(luo,999) 'Could not open the dark frame file: ',dfn
      write(luo,999) '-STOP : can not open dark frame-'
      call exit(-1)
C
  902 write(luo,999) ' No of passes too large, max: ',npasx
      write(luo,999) '-STOP : No of passes too large-'
      call exit(-2)
C
  903 write(luo,999) 'Could not open: ',ifn
      goto 120
C
  904 write(luo,999) 'Reading error, while reading from file: ',ifn
      close(luf)
      goto 120
C
  913 write(luo,999) 'Could not open: ',ifn
      goto 220
C
  914 write(luo,999) 'Reading error, while reading from file: ',ifn
      close(luf)
      goto 220
C
  923 write(luo,999) 'Could not open: ',ifn
      goto 20
C
  924 write(luo,999) 'Writing error, while writing file: ',ifn
      close(luf)
      goto 20
C
 930  write(luo,999) 'Could not open: ', redstatfn
      stop '-STOP: could not open red stat file'
C
 931  write(luo,999) 'Could not open: ', blustatfn
      stop '-STOP: could not open blu stat file'
C
 932  write(luo,999) 'Could not open: ', dopstatfn
      stop '-STOP: could not open dop stat file'
C
 992  write(luo,999) 'Could not open red limb file: ', lfn
      goto 61
C
 993  write(luo,999) 'Could not open blu limb file: ', lfn
      goto 161
C
 999  format(2a)
C
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
