      program darkchk
c
c     check from ronchi/dark/full, and compute avg'd dark
c
c     SGK 26 Sep 95: added nfulx (instead of "3" hard-wired)
c         Oct  5 95: added rfn: ronchi list filename
c
      parameter (nix=512, njx=512, nkx=nix*njx)
      integer*2 img(nkx)
      integer   imgavg(nkx)
      logical   notsaved
      character *70 dfn, rfn, fn
      parameter (lui=5, luo=6, luf=1, lud=2, lur=3)
c
      read (lui,*) ni,nj
      nk = ni*nj
      if (nk.gt.nkx) stop '-STOP: ni x nj too large-'
c
      read (lui,*) nis, nie, njs, nje
      if(nis.lt.1   .or.nis.ge.nie) stop '-STOP: invalid nis-'
      if(nie.le.nis .or.nie.gt.ni ) stop '-STOP: invalid nie-'
      if(njs.lt.1   .or.njs.ge.nje) stop '-STOP: invalid njs-'
      if(nje.le.njs .or.nje.gt.nj ) stop '-STOP: invalid nje-'
c
      read (lui,*) avgmin, stdmin, nfulx
      read (lui,'(a)') dfn
      read (lui,'(a)') rfn
c
      open(lur, file=rfn, status='unknown', err=991)
c
      notsaved = .true.
      do 5 k=1,nk
 5    imgavg(k) = 0
      nrch = 0
      ndrk = 0
      nful = 0
c
 10   read (lui,'(a)',end=100) fn
      open (luf, file=fn, status='old'
     &,     access='direct', recl=2*nk, err=900)
      read (luf, rec=1) img
      close(luf)
c
      call stats(img, ni, nj, nis, nie, njs, nje, avg, std)
      write(luo,*) fn
      write(luo,*) avg, std, nis, nie, njs, nje
c
      if(avg.lt.avgmin) then
        ndrk = ndrk + 1
        do 50 k = 1, nk
 50     imgavg(k) = imgavg(k) + img(k)
        write(luo,*) '--> dark   frame #', ndrk
      else if(std.gt.stdmin) then
        nrch = nrch + 1
        write(luo,*) '--> ronchi frame #', nrch
        write(lur,'(a)') fn
      else
        nful = nful + 1
        write(luo,*) '--> full   frame #', nful
      endif
c
      if(ndrk.eq.2.and.notsaved) then
        do 60 k = 1, nk
 60     img(k) = imgavg(k)/ndrk
        write(luo,*) 'saving ', dfn
        open (lud, file=dfn, status='new'
     &,       access='direct', recl=2*nk, err=910)
        write(lud, rec=1) img
        close(lud)
        notsaved = .false.
      endif
c
      if(nful.lt.nfulx) goto 10
      close(lur)
      stop
c
 100  write(luo,*) 'no more file to process'
      stop
c
 900  write(luo,*) 'Could not open ', fn
      stop '-STOP: open image file error-'
c
 910  write(luo,*) 'Could not open ', dfn
      stop '-STOP: open darkframe file error-'
c
 991  write(luo,*) 'Could not open ', rfn
      stop '-STOP: open ronchi list file error-'
      end

      subroutine stats(img, ni, nj, nis, nie, njs, nje, avg, std)
c
      integer*2 img(ni,nj)
c
      avg = 0.0
      do 10 j = njs, nje
      do 10 i = nis, nie
 10   avg = avg + img(i,j)
c
      n   = (nie-nis+1)*(nje-njs+1)
      avg = avg /n
c
      std = 0.0
      do 20 j = njs, nje
      do 20 i = nis, nie
 20   std = std + (img(i,j)-avg)**2
c
      std = sqrt(std/n)
      return
      end
