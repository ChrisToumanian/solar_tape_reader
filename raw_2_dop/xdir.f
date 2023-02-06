      character*80 function xdir(fn, newdir)
c
      character*(*) fn, newdir
c
      do 10 i = len(fn), 1, -1
        if (fn(i:i).eq.'/') goto 15
 10   continue
      i = 0
 15   ibase = i + 1
c
      xdir = newdir(1:inblank(newdir))//'/'//fn(ibase:inblank(fn))
      return
      end
c
c ---------------------------------------------------------------------------
c
      function inblank(fn)
c
      character*(*) fn
c
      do 10 i=len(fn),1,-1
        if(fn(i:i).ne.' ') goto 20
 10   continue
      i = 1
 20   inblank = i
      return
      end
c
c ---------------------------------------------------------------------------
c
      function ilast(fn, c)
c
      character*(*) fn, c*1
c
      do 10 i=len(fn),1,-1
        if(fn(i:i).eq.c) goto 20
 10   continue
      i = -1

 20   ilast = i
      return
      end
