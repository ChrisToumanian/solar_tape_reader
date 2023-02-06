      subroutine modfn(ifn, lfn)
c
c     modify ifn-->lfn for limb file name
c     uses the environment LmbDIR, or './limbs' if LmbDIR is not set
c
c     SGK/17-Dec-90/26-Jan-91                              Ver 1.0/1/f77/Unix
c
      character*(*) ifn, lfn
      character*80 lmbdir
      save         lmbdir
      data nlmbdir /-99/
c
      if(nlmbdir.eq.-99) call initlmbdir(lmbdir, nlmbdir)
c
      i = lastchar(ifn,'/')
      if(i.le.1) stop '-STOP in modfn: invalid fn-'
c
      lfn = lmbdir(1:nlmbdir)//ifn(i:)
c
      return
      end
      subroutine initlmbdir(lmbdir, nlmbdir)
c
      character*(*) lmbdir
c-sunOS:
      call getenv('LmbDIR', lmbdir)
      nlmbdir = nstrsiz(lmbdir)
c-unicos:
c-      integer GETENV
c-      external GETENV
c-c
c-      nlmbdir = GETENV('LmbDIR', lmbdir, len(lmbdir))
c-      if(nlmbdir.ne.0) nlmbdir = nstrsiz(lmbdir)
c
      if(nlmbdir.gt.0) return
c
c     default value:
c
      lmbdir = './limbs'
      nlmbdir = nstrsiz(lmbdir)
      return
      end
      function nstrsiz(strng)
c
      character*(*) strng
c
      do 10 i = len(strng), 1, -1
        if(strng(i:i).ne.' ') goto 20
 10   continue
      i = 0
 20   nstrsiz = i
      return
      end
      function lastchar(strng,c)
c
      character*(*) strng
      character*1 c
c
      do 10 i = len(strng), 1, -1
        if(strng(i:i).eq.c) goto 20
 10   continue
      i = 0
 20   lastchar = i
      return
      end
