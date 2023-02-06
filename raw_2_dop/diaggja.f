      SUBROUTINE DIAGGJA (A,ND,N,IERR)
C
C     Gauss-Jordan diagonalisation : A.X=B ==> X=diag(A,B)
C
C     Ref : H Epps, Astro 204A, 1986
C
C     SK/27-JAN-86/5-14-86                           Ver1.2/VAX
C
C     A(ND,*) : Matrix to be solved, where A(i,N+1) are the bi
C     N       : the actual part used
C     IERR    : Error flag, 0 -> ok
C                          -1 -> N > ND
C                          -k -> no non-zero pivot for the k-th equation

C     A(ND,*) is destroyed (scrambled), the solution is return via A(i,N+1)

      DIMENSION A(ND,*)

      NP1=N+1
      IERR=0
      IF(N.LE.ND) GOTO 10
      IERR=-1
      RETURN

   10 DO 100 K=1,N
      KP1=K+1

C     select the best pivot a(k',k) for the k-th equation

      KK=0
      XMIN=+1.E+38
C                                  ! +infinity
      DO 20 I=K,N
      IF(A(I,K).EQ.0.) GOTO 20
      AA=ABS(A(I,NP1)/A(I,K))
C                                  ! pivot selection criterion
      IF(AA.GE.XMIN) GOTO 20
      XMIN=AA
      KK=I
   20 CONTINUE

      IF(KK.NE.0) GOTO 25
      IERR=-K
C                                  ! no a(i,k) >< 0. for i=k,N
      RETURN

   25 IF(KK.EQ.K) GOTO 40

C     exchange equations k & k'

      DO 30 J=1,NP1
      AA=A(KK,J)
      A(KK,J)=A(K,J)
   30 A(K,J)=AA

   40 CONTINUE

C     reduce the pivoting equation

      AA=A(K,K)
C                                 ! pivot value

      DO 50 J=1,NP1
   50 A(K,J)=A(K,J)/AA

C     Reduce the matrix

      DO 80 I=1,N
      IF(I.EQ.K) GOTO 80
C                                 ! not the pivot equation
      DO 70 J=KP1,NP1
C                                 ! only on the right side of the pivot
   70 A(I,J)=A(I,J)-A(K,J)*A(I,K)
   80 CONTINUE

  100 CONTINUE

      RETURN
      END
