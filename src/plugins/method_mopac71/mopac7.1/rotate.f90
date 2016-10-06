      subroutine rotate(ni, nj, xi, xj, w, kr, e1b, e2a, enuc, cutof2) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numcal, keywrd, method_am1, method_pm3
      use parameters_C, only: alp, natorb, tore, guess1, guess2, guess3
      use funcon_C, only : ev, a0
      USE rotate_C, only : css1, csp1, cpps1, cppp1, css2, csp2, cpps2, cppp2, ccore
      use chanel_C, only : irot
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:01  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use repp_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: ni 
      integer  :: nj 
      integer , intent(inout) :: kr 
      real(double) , intent(out) :: enuc 
      real(double)  :: cutof2 
      real(double) , intent(in) :: xi(3) 
      real(double) , intent(in) :: xj(3) 
      real(double) , intent(out) :: w(100) 
      real(double) , intent(out) :: e1b(10) 
      real(double) , intent(out) :: e2a(10) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, i, ki, nt, nk, nl, ig 
      real(double), dimension(22) :: ri  
      real(double), dimension(3,4) :: boron1, boron2, boron3 
      real(double) ::  ev1, ev2, ev3, ev4,  rij, &
        scale, rijx, gam, a, xx11, xx21, xx22, xx31, xx32, xx33, yy11, yy21, &
        yy22, zz11, zz21, zz22, zz31, zz32, zz33, yyzz11, yyzz21, yyzz22, xy11&
        , xy21, xy22, xy31, xy32, xz11, xz21, xz22, xz31, xz32, xz33, yz11, &
        yz21, yz22, yz31, yz32, ax 
      real(double), dimension(3) :: x, y, z 
      logical :: analyt, si, sj, opend 

      save analyt, icalcn, boron1, boron2, boron3 
!-----------------------------------------------
!***********************************************************************
!
!..IMPROVED SCALAR VERSION
!..WRITTEN BY ERNEST R. DAVIDSON, INDIANA UNIVERSITY.
!
!
!   ROTATE CALCULATES THE TWO-PARTICLE INTERACTIONS.
!
!   ON INPUT  NI     = ATOMIC NUMBER OF FIRST ATOM.
!             NJ     = ATOMIC NUMBER OF SECOND ATOM.
!             XI     = COORDINATE OF FIRST ATOM.
!             XJ     = COORDINATE OF SECOND ATOM.
!
! ON OUTPUT W      = ARRAY OF TWO-ELECTRON REPULSION INTEGRALS.
!           E1B,E2A= ARRAY OF ELECTRON-NUCLEAR ATTRACTION INTEGRALS,
!                    E1B = ELECTRON ON ATOM NI ATTRACTING NUCLEUS OF NJ.
!           ENUC   = NUCLEAR-NUCLEAR REPULSION TERM.
!
!
! *** THIS ROUTINE COMPUTES THE REPULSION AND NUCLEAR ATTRACTION
!     INTEGRALS OVER MOLECULAR-FRAME COORDINATES.  THE INTEGRALS OVER
!     LOCAL FRAME COORDINATES ARE EVALUATED BY SUBROUTINE REPP AND
!     STORED AS FOLLOWS (WHERE P-SIGMA = O,   AND P-PI = P AND P* )
!     IN RI
!     (SS/SS)=1,   (SO/SS)=2,   (OO/SS)=3,   (PP/SS)=4,   (SS/OS)=5,
!     (SO/SO)=6,   (SP/SP)=7,   (OO/SO)=8,   (PP/SO)=9,   (PO/SP)=10,
!     (SS/OO)=11,  (SS/PP)=12,  (SO/OO)=13,  (SO/PP)=14,  (SP/OP)=15,
!     (OO/OO)=16,  (PP/OO)=17,  (OO/PP)=18,  (PP/PP)=19,  (PO/PO)=20,
!     (PP/P*P*)=21,   (P*P/P*P)=22.
!
!*********************************************************************** 
      data icalcn/ 0/  
      data boron1/ 0.182613D0, 0.118587D0, -0.073280D0, 0.412253D0, -0.149917D0&
        , 0.000000D0, 0.261751D0, 0.050275D0, 0.000000D0, 0.359244D0, &
        0.074729D0, 0.000000D0/  
      data boron2/ 6.D0, 6.D0, 5.D0, 10.D0, 6.D0, 0.D0, 8.D0, 5.D0, 0.D0, 9.D0&
        , 9.D0, 0.D0/  
      data boron3/ 0.727592D0, 1.466639D0, 1.570975D0, 0.832586D0, 1.186220D0, &
        0.000000D0, 1.063995D0, 1.936492D0, 0.000000D0, 0.819351D0, 1.574414D0&
        , 0.000000D0/  
!
      if (icalcn /= numcal) then   
!
!   EV = 27.21
!
        ev1 = ev*0.5D0 
        ev2 = ev1*0.5D0 
        ev3 = ev2*0.5D0 
        ev4 = ev3*0.5D0 
        icalcn = numcal 
        analyt = index(keywrd,'ANALYT') /= 0 
        if (analyt) then 
          inquire(unit=irot, opened=opend) 
          if (opend) close(unit=irot, status='DELETE') 
          open(unit=irot, status='SCRATCH', form='UNFORMATTED', position='asis'&
            ) 
          rewind irot 
        endif 
      endif 
!
      x(1) = xi(1) - xj(1) 
      x(2) = xi(2) - xj(2) 
      x(3) = xi(3) - xj(3) 
      rij = x(1)*x(1) + x(2)*x(2) + x(3)*x(3) 
      if (rij < 0.00002D0) then 
!
!     SMALL RIJ CASE
!
        e1b = 0.D0 
        e2a = 0.D0 
        w = 0.D0 
        enuc = 0.D0 
!
      else 
!
!     MNDO AND AM1 CASES
!
! *** THE REPULSION INTEGRALS OVER MOLECULAR FRAME (W) ARE STORED IN THE
!     ORDER IN WHICH THEY WILL LATER BE USED.  IE.  (I,J/K,L) WHERE
!     J.LE.I  AND  L.LE.K     AND L VARIES MOST RAPIDLY AND I LEAST
!     RAPIDLY.  (ANTI-NORMAL COMPUTER STORAGE)
!
!
        rijx = sqrt(rij) 
        rij = rijx 
!
! *** COMPUTE INTEGRALS IN DIATOMIC FRAME
!
        call repp (ni, nj, rij, ri, ccore, cutof2, a0, ev, ev1, ev2, ev3, ev4) 
        if (analyt) write (irot) (ri(i),i=1,22) 
!
        gam = ri(1) 
        a = 1.D0/rijx 
        x(1) = x(1)*a 
        x(2) = x(2)*a 
        x(3) = x(3)*a 
        if (abs(x(3)) > 0.9999999999D0) then 
          x(3) = sign(1.D0,x(3)) 
          y(1) = 0.D0 
          y(2) = 1.D0 
          y(3) = 0.D0 
          z(1) = 1.D0 
          z(2) = 0.D0 
          z(3) = 0.D0 
        else 
          z(3) = sqrt(1.D0 - x(3)*x(3)) 
          a = 1.D0/z(3) 
          y(1) = -a*x(2)*sign(1.D0,x(1)) 
          y(2) = abs(a*x(1)) 
          y(3) = 0.D0 
          z(1) = -a*x(1)*x(3) 
          z(2) = -a*x(2)*x(3) 
        endif 
        si = natorb(ni) > 1 
        sj = natorb(nj) > 1 
        if (si .or. sj) then 
          xx11 = x(1)*x(1) 
          xx21 = x(2)*x(1) 
          xx22 = x(2)*x(2) 
          xx31 = x(3)*x(1) 
          xx32 = x(3)*x(2) 
          xx33 = x(3)*x(3) 
          yy11 = y(1)*y(1) 
          yy21 = y(2)*y(1) 
          yy22 = y(2)*y(2) 
          zz11 = z(1)*z(1) 
          zz21 = z(2)*z(1) 
          zz22 = z(2)*z(2) 
          zz31 = z(3)*z(1) 
          zz32 = z(3)*z(2) 
          zz33 = z(3)*z(3) 
          yyzz11 = yy11 + zz11 
          yyzz21 = yy21 + zz21 
          yyzz22 = yy22 + zz22 
          xy11 = 2.D0*x(1)*y(1) 
          xy21 = x(1)*y(2) + x(2)*y(1) 
          xy22 = 2.D0*x(2)*y(2) 
          xy31 = x(3)*y(1) 
          xy32 = x(3)*y(2) 
          xz11 = 2.D0*x(1)*z(1) 
          xz21 = x(1)*z(2) + x(2)*z(1) 
          xz22 = 2.D0*x(2)*z(2) 
          xz31 = x(1)*z(3) + x(3)*z(1) 
          xz32 = x(2)*z(3) + x(3)*z(2) 
          xz33 = 2.D0*x(3)*z(3) 
          yz11 = 2.D0*y(1)*z(1) 
          yz21 = y(1)*z(2) + y(2)*z(1) 
          yz22 = 2.D0*y(2)*z(2) 
          yz31 = y(1)*z(3) 
          yz32 = y(2)*z(3) 
        endif 
!
!     (S S/S S)
        w(1) = ri(1) 
        ki = 1 
        if (sj) then 
!     (S S/PX S)
          w(2) = ri(5)*x(1) 
!     (S S/PX PX)
          w(3) = ri(11)*xx11 + ri(12)*yyzz11 
!     (S S/PY S)
          w(4) = ri(5)*x(2) 
!     (S S/PY PX)
          w(5) = ri(11)*xx21 + ri(12)*yyzz21 
!     (S S/PY PY)
          w(6) = ri(11)*xx22 + ri(12)*yyzz22 
!     (S S/PZ S)
          w(7) = ri(5)*x(3) 
!     (S S/PZ PX)
          w(8) = ri(11)*xx31 + ri(12)*zz31 
!     (S S/PZ PY)
          w(9) = ri(11)*xx32 + ri(12)*zz32 
!     (S S/PZ PZ)
          w(10) = ri(11)*xx33 + ri(12)*zz33 
          ki = 10 
        endif 
!
        if (si) then 
          if (sj) then 
!     (PX S/S S)
            w(11) = ri(2)*x(1) 
!     (PX S/PX S)
            w(12) = ri(6)*xx11 + ri(7)*yyzz11 
!     (PX S/PX PX)
            w(13) = x(1)*(ri(13)*xx11+ri(14)*yyzz11) + ri(15)*(y(1)*xy11+z(1)*&
              xz11) 
!     (PX S/PY S)
            w(14) = ri(6)*xx21 + ri(7)*yyzz21 
!     (PX S/PY PX)
            w(15) = x(1)*(ri(13)*xx21+ri(14)*yyzz21) + ri(15)*(y(1)*xy21+z(1)*&
              xz21) 
!     (PX S/PY PY)
            w(16) = x(1)*(ri(13)*xx22+ri(14)*yyzz22) + ri(15)*(y(1)*xy22+z(1)*&
              xz22) 
!     (PX S/PZ S)
            w(17) = ri(6)*xx31 + ri(7)*zz31 
!     (PX S/PZ PX)
            w(18) = x(1)*(ri(13)*xx31+ri(14)*zz31) + ri(15)*(y(1)*xy31+z(1)*&
              xz31) 
!     (PX S/PZ PY)
            w(19) = x(1)*(ri(13)*xx32+ri(14)*zz32) + ri(15)*(y(1)*xy32+z(1)*&
              xz32) 
!     (PX S/PZ PZ)
            w(20) = x(1)*(ri(13)*xx33+ri(14)*zz33) + ri(15)*(z(1)*xz33) 
!     (PX PX/S S)
            w(21) = ri(3)*xx11 + ri(4)*yyzz11 
!     (PX PX/PX S)
            w(22) = x(1)*(ri(8)*xx11+ri(9)*yyzz11) + ri(10)*(y(1)*xy11+z(1)*&
              xz11) 
!     (PX PX/PX PX)
            w(23) = (ri(16)*xx11+ri(17)*yyzz11)*xx11 + ri(18)*xx11*yyzz11 + ri(&
              19)*(yy11*yy11 + zz11*zz11) + ri(20)*(xy11*xy11 + xz11*xz11) + ri&
              (21)*(yy11*zz11 + zz11*yy11) + ri(22)*yz11*yz11 
!     (PX PX/PY S)
            w(24) = x(2)*(ri(8)*xx11+ri(9)*yyzz11) + ri(10)*(y(2)*xy11+z(2)*&
              xz11) 
!     (PX PX/PY PX)
            w(25) = (ri(16)*xx11+ri(17)*yyzz11)*xx21 + ri(18)*xx11*yyzz21 + ri(&
              19)*(yy11*yy21 + zz11*zz21) + ri(20)*(xy11*xy21 + xz11*xz21) + ri&
              (21)*(yy11*zz21 + zz11*yy21) + ri(22)*yz11*yz21 
!     (PX PX/PY PY)
            w(26) = (ri(16)*xx11+ri(17)*yyzz11)*xx22 + ri(18)*xx11*yyzz22 + ri(&
              19)*(yy11*yy22 + zz11*zz22) + ri(20)*(xy11*xy22 + xz11*xz22) + ri&
              (21)*(yy11*zz22 + zz11*yy22) + ri(22)*yz11*yz22 
!     (PX PX/PZ S)
            w(27) = x(3)*(ri(8)*xx11+ri(9)*yyzz11) + ri(10)*z(3)*xz11 
!     (PX PX/PZ PX)
            w(28) = (ri(16)*xx11+ri(17)*yyzz11)*xx31 + (ri(18)*xx11+ri(19)*zz11&
              +ri(21)*yy11)*zz31 + ri(20)*(xy11*xy31 + xz11*xz31) + ri(22)*yz11&
              *yz31 
!     (PX PX/PZ PY)
            w(29) = (ri(16)*xx11+ri(17)*yyzz11)*xx32 + (ri(18)*xx11+ri(19)*zz11&
              +ri(21)*yy11)*zz32 + ri(20)*(xy11*xy32 + xz11*xz32) + ri(22)*yz11&
              *yz32 
!     (PX PX/PZ PZ)
            w(30) = (ri(16)*xx11+ri(17)*yyzz11)*xx33 + (ri(18)*xx11+ri(19)*zz11&
              +ri(21)*yy11)*zz33 + ri(20)*xz11*xz33 
!     (PY S/S S)
            w(31) = ri(2)*x(2) 
!     (PY S/PX S)
            w(32) = ri(6)*xx21 + ri(7)*yyzz21 
!     (PY S/PX PX)
            w(33) = x(2)*(ri(13)*xx11+ri(14)*yyzz11) + ri(15)*(y(2)*xy11+z(2)*&
              xz11) 
!     (PY S/PY S)
            w(34) = ri(6)*xx22 + ri(7)*yyzz22 
!     (PY S/PY PX)
            w(35) = x(2)*(ri(13)*xx21+ri(14)*yyzz21) + ri(15)*(y(2)*xy21+z(2)*&
              xz21) 
!     (PY S/PY PY)
            w(36) = x(2)*(ri(13)*xx22+ri(14)*yyzz22) + ri(15)*(y(2)*xy22+z(2)*&
              xz22) 
!     (PY S/PZ S)
            w(37) = ri(6)*xx32 + ri(7)*zz32 
!     (PY S/PZ PX)
            w(38) = x(2)*(ri(13)*xx31+ri(14)*zz31) + ri(15)*(y(2)*xy31+z(2)*&
              xz31) 
!     (PY S/PZ PY)
            w(39) = x(2)*(ri(13)*xx32+ri(14)*zz32) + ri(15)*(y(2)*xy32+z(2)*&
              xz32) 
!     (PY S/PZ PZ)
            w(40) = x(2)*(ri(13)*xx33+ri(14)*zz33) + ri(15)*z(2)*xz33 
!     (PY PX/S S)
            w(41) = ri(3)*xx21 + ri(4)*yyzz21 
!     (PY PX/PX S)
            w(42) = x(1)*(ri(8)*xx21+ri(9)*yyzz21) + ri(10)*(y(1)*xy21+z(1)*&
              xz21) 
!     (PY PX/PX PX)
            w(43) = (ri(16)*xx21+ri(17)*yyzz21)*xx11 + ri(18)*xx21*yyzz11 + ri(&
              19)*(yy21*yy11 + zz21*zz11) + ri(20)*(xy21*xy11 + xz21*xz11) + ri&
              (21)*(yy21*zz11 + zz21*yy11) + ri(22)*yz21*yz11 
!     (PY PX/PY S)
            w(44) = x(2)*(ri(8)*xx21+ri(9)*yyzz21) + ri(10)*(y(2)*xy21+z(2)*&
              xz21) 
!     (PY PX/PY PX)
            w(45) = (ri(16)*xx21+ri(17)*yyzz21)*xx21 + ri(18)*xx21*yyzz21 + ri(&
              19)*(yy21*yy21 + zz21*zz21) + ri(20)*(xy21*xy21 + xz21*xz21) + ri&
              (21)*(yy21*zz21 + zz21*yy21) + ri(22)*yz21*yz21 
!     (PY PX/PY PY)
            w(46) = (ri(16)*xx21+ri(17)*yyzz21)*xx22 + ri(18)*xx21*yyzz22 + ri(&
              19)*(yy21*yy22 + zz21*zz22) + ri(20)*(xy21*xy22 + xz21*xz22) + ri&
              (21)*(yy21*zz22 + zz21*yy22) + ri(22)*yz21*yz22 
!     (PY PX/PZ S)
            w(47) = x(3)*(ri(8)*xx21+ri(9)*yyzz21) + ri(10)*z(3)*xz21 
!      (PY PX/PZ PX)
            w(48) = (ri(16)*xx21+ri(17)*yyzz21)*xx31 + (ri(18)*xx21+ri(19)*zz21&
              +ri(21)*yy21)*zz31 + ri(20)*(xy21*xy31 + xz21*xz31) + ri(22)*yz21&
              *yz31 
!      (PY PX/PZ PY)
            w(49) = (ri(16)*xx21+ri(17)*yyzz21)*xx32 + (ri(18)*xx21+ri(19)*zz21&
              +ri(21)*yy21)*zz32 + ri(20)*(xy21*xy32 + xz21*xz32) + ri(22)*yz21&
              *yz32 
!      (PY PX/PZ PZ)
            w(50) = (ri(16)*xx21+ri(17)*yyzz21)*xx33 + (ri(18)*xx21+ri(19)*zz21&
              +ri(21)*yy21)*zz33 + ri(20)*xz21*xz33 
!     (PY PY/S S)
            w(51) = ri(3)*xx22 + ri(4)*yyzz22 
!     (PY PY/PX S)
            w(52) = x(1)*(ri(8)*xx22+ri(9)*yyzz22) + ri(10)*(y(1)*xy22+z(1)*&
              xz22) 
!      (PY PY/PX PX)
            w(53) = (ri(16)*xx22+ri(17)*yyzz22)*xx11 + ri(18)*xx22*yyzz11 + ri(&
              19)*(yy22*yy11 + zz22*zz11) + ri(20)*(xy22*xy11 + xz22*xz11) + ri&
              (21)*(yy22*zz11 + zz22*yy11) + ri(22)*yz22*yz11 
!     (PY PY/PY S)
            w(54) = x(2)*(ri(8)*xx22+ri(9)*yyzz22) + ri(10)*(y(2)*xy22+z(2)*&
              xz22) 
!      (PY PY/PY PX)
            w(55) = (ri(16)*xx22+ri(17)*yyzz22)*xx21 + ri(18)*xx22*yyzz21 + ri(&
              19)*(yy22*yy21 + zz22*zz21) + ri(20)*(xy22*xy21 + xz22*xz21) + ri&
              (21)*(yy22*zz21 + zz22*yy21) + ri(22)*yz22*yz21 
!      (PY PY/PY PY)
            w(56) = (ri(16)*xx22+ri(17)*yyzz22)*xx22 + ri(18)*xx22*yyzz22 + ri(&
              19)*(yy22*yy22 + zz22*zz22) + ri(20)*(xy22*xy22 + xz22*xz22) + ri&
              (21)*(yy22*zz22 + zz22*yy22) + ri(22)*yz22*yz22 
!     (PY PY/PZ S)
            w(57) = x(3)*(ri(8)*xx22+ri(9)*yyzz22) + ri(10)*z(3)*xz22 
!      (PY PY/PZ PX)
            w(58) = (ri(16)*xx22+ri(17)*yyzz22)*xx31 + (ri(18)*xx22+ri(19)*zz22&
              +ri(21)*yy22)*zz31 + ri(20)*(xy22*xy31 + xz22*xz31) + ri(22)*yz22&
              *yz31 
!      (PY PY/PZ PY)
            w(59) = (ri(16)*xx22+ri(17)*yyzz22)*xx32 + (ri(18)*xx22+ri(19)*zz22&
              +ri(21)*yy22)*zz32 + ri(20)*(xy22*xy32 + xz22*xz32) + ri(22)*yz22&
              *yz32 
!      (PY PY/PZ PZ)
            w(60) = (ri(16)*xx22+ri(17)*yyzz22)*xx33 + (ri(18)*xx22+ri(19)*zz22&
              +ri(21)*yy22)*zz33 + ri(20)*xz22*xz33 
!     (PZ S/SS)
            w(61) = ri(2)*x(3) 
!     (PZ S/PX S)
            w(62) = ri(6)*xx31 + ri(7)*zz31 
!     (PZ S/PX PX)
            w(63) = x(3)*(ri(13)*xx11+ri(14)*yyzz11) + ri(15)*z(3)*xz11 
!     (PZ S/PY S)
            w(64) = ri(6)*xx32 + ri(7)*zz32 
!     (PZ S/PY PX)
            w(65) = x(3)*(ri(13)*xx21+ri(14)*yyzz21) + ri(15)*z(3)*xz21 
!     (PZ S/PY PY)
            w(66) = x(3)*(ri(13)*xx22+ri(14)*yyzz22) + ri(15)*z(3)*xz22 
!     (PZ S/PZ S)
            w(67) = ri(6)*xx33 + ri(7)*zz33 
!     (PZ S/PZ PX)
            w(68) = x(3)*(ri(13)*xx31+ri(14)*zz31) + ri(15)*z(3)*xz31 
!     (PZ S/PZ PY)
            w(69) = x(3)*(ri(13)*xx32+ri(14)*zz32) + ri(15)*z(3)*xz32 
!     (PZ S/PZ PZ)
            w(70) = x(3)*(ri(13)*xx33+ri(14)*zz33) + ri(15)*z(3)*xz33 
!     (PZ PX/S S)
            w(71) = ri(3)*xx31 + ri(4)*zz31 
!     (PZ PX/PX S)
            w(72) = x(1)*(ri(8)*xx31+ri(9)*zz31) + ri(10)*(y(1)*xy31+z(1)*xz31) 
!      (PZ PX/PX PX)
            w(73) = (ri(16)*xx31+ri(17)*zz31)*xx11 + ri(18)*xx31*yyzz11 + ri(19&
              )*zz31*zz11 + ri(20)*(xy31*xy11 + xz31*xz11) + ri(21)*zz31*yy11&
               + ri(22)*yz31*yz11 
!     (PZ PX/PY S)
            w(74) = x(2)*(ri(8)*xx31+ri(9)*zz31) + ri(10)*(y(2)*xy31+z(2)*xz31) 
!      (PZ PX/PY PX)
            w(75) = (ri(16)*xx31+ri(17)*zz31)*xx21 + ri(18)*xx31*yyzz21 + ri(19&
              )*zz31*zz21 + ri(20)*(xy31*xy21 + xz31*xz21) + ri(21)*zz31*yy21&
               + ri(22)*yz31*yz21 
!      (PZ PX/PY PY)
            w(76) = (ri(16)*xx31+ri(17)*zz31)*xx22 + ri(18)*xx31*yyzz22 + ri(19&
              )*zz31*zz22 + ri(20)*(xy31*xy22 + xz31*xz22) + ri(21)*zz31*yy22&
               + ri(22)*yz31*yz22 
!     (PZ PX/PZ S)
            w(77) = x(3)*(ri(8)*xx31+ri(9)*zz31) + ri(10)*z(3)*xz31 
!     (PZ PX/PZ PX)
            w(78) = (ri(16)*xx31+ri(17)*zz31)*xx31 + (ri(18)*xx31+ri(19)*zz31)*&
              zz31 + ri(20)*(xy31*xy31 + xz31*xz31) + ri(22)*yz31*yz31 
!      (PZ PX/PZ PY)
            w(79) = (ri(16)*xx31+ri(17)*zz31)*xx32 + (ri(18)*xx31+ri(19)*zz31)*&
              zz32 + ri(20)*(xy31*xy32 + xz31*xz32) + ri(22)*yz31*yz32 
!      (PZ PX/PZ PZ)
            w(80) = (ri(16)*xx31+ri(17)*zz31)*xx33 + (ri(18)*xx31+ri(19)*zz31)*&
              zz33 + ri(20)*xz31*xz33 
!     (PZ PY/S S)
            w(81) = ri(3)*xx32 + ri(4)*zz32 
!     (PZ PY/PX S)
            w(82) = x(1)*(ri(8)*xx32+ri(9)*zz32) + ri(10)*(y(1)*xy32+z(1)*xz32) 
!      (PZ PY/PX PX)
            w(83) = (ri(16)*xx32+ri(17)*zz32)*xx11 + ri(18)*xx32*yyzz11 + ri(19&
              )*zz32*zz11 + ri(20)*(xy32*xy11 + xz32*xz11) + ri(21)*zz32*yy11&
               + ri(22)*yz32*yz11 
!     (PZ PY/PY S)
            w(84) = x(2)*(ri(8)*xx32+ri(9)*zz32) + ri(10)*(y(2)*xy32+z(2)*xz32) 
!      (PZ PY/PY PX)
            w(85) = (ri(16)*xx32+ri(17)*zz32)*xx21 + ri(18)*xx32*yyzz21 + ri(19&
              )*zz32*zz21 + ri(20)*(xy32*xy21 + xz32*xz21) + ri(21)*zz32*yy21&
               + ri(22)*yz32*yz21 
!      (PZ PY/PY PY)
            w(86) = (ri(16)*xx32+ri(17)*zz32)*xx22 + ri(18)*xx32*yyzz22 + ri(19&
              )*zz32*zz22 + ri(20)*(xy32*xy22 + xz32*xz22) + ri(21)*zz32*yy22&
               + ri(22)*yz32*yz22 
!     (PZ PY/PZ S)
            w(87) = x(3)*(ri(8)*xx32+ri(9)*zz32) + ri(10)*z(3)*xz32 
!      (PZ PY/PZ PX)
            w(88) = (ri(16)*xx32+ri(17)*zz32)*xx31 + (ri(18)*xx32+ri(19)*zz32)*&
              zz31 + ri(20)*(xy32*xy31 + xz32*xz31) + ri(22)*yz32*yz31 
!      (PZ PY/PZ PY)
            w(89) = (ri(16)*xx32+ri(17)*zz32)*xx32 + (ri(18)*xx32+ri(19)*zz32)*&
              zz32 + ri(20)*(xy32*xy32 + xz32*xz32) + ri(22)*yz32*yz32 
!       (PZ PY/PZ PZ)
            w(90) = (ri(16)*xx32+ri(17)*zz32)*xx33 + (ri(18)*xx32+ri(19)*zz32)*&
              zz33 + ri(20)*xz32*xz33 
!     (PZ PZ/S S)
            w(91) = ri(3)*xx33 + ri(4)*zz33 
!     (PZ PZ/PX S)
            w(92) = x(1)*(ri(8)*xx33+ri(9)*zz33) + ri(10)*(z(1)*xz33) 
!       (PZ PZ/PX PX)
            w(93) = (ri(16)*xx33+ri(17)*zz33)*xx11 + ri(18)*xx33*yyzz11 + ri(19&
              )*zz33*zz11 + ri(20)*xz33*xz11 + ri(21)*zz33*yy11 
!     (PZ PZ/PY S)
            w(94) = x(2)*(ri(8)*xx33+ri(9)*zz33) + ri(10)*z(2)*xz33 
!       (PZ PZ/PY PX)
            w(95) = (ri(16)*xx33+ri(17)*zz33)*xx21 + ri(18)*xx33*yyzz21 + ri(19&
              )*zz33*zz21 + ri(20)*xz33*xz21 + ri(21)*zz33*yy21 
!       (PZ PZ/PY PY)
            w(96) = (ri(16)*xx33+ri(17)*zz33)*xx22 + ri(18)*xx33*yyzz22 + ri(19&
              )*zz33*zz22 + ri(20)*xz33*xz22 + ri(21)*zz33*yy22 
!     (PZ PZ/PZ S)
            w(97) = x(3)*(ri(8)*xx33+ri(9)*zz33) + ri(10)*z(3)*xz33 
!       (PZ PZ/PZ PX)
            w(98) = (ri(16)*xx33+ri(17)*zz33)*xx31 + (ri(18)*xx33+ri(19)*zz33)*&
              zz31 + ri(20)*xz33*xz31 
!       (PZ PZ/PZ PY)
            w(99) = (ri(16)*xx33+ri(17)*zz33)*xx32 + (ri(18)*xx33+ri(19)*zz33)*&
              zz32 + ri(20)*xz33*xz32 
!       (PZ PZ/PZ PZ)
            w(100) = (ri(16)*xx33+ri(17)*zz33)*xx33 + (ri(18)*xx33+ri(19)*zz33)&
              *zz33 + ri(20)*xz33*xz33 
            ki = 100 
          else 
!     (PX S/S S)
            w(2) = ri(2)*x(1) 
!     (PX PX/S S)
            w(3) = ri(3)*xx11 + ri(4)*yyzz11 
!     (PY S/S S)
            w(4) = ri(2)*x(2) 
!     (PY PX/S S)
            w(5) = ri(3)*xx21 + ri(4)*yyzz21 
!     (PY PY/S S)
            w(6) = ri(3)*xx22 + ri(4)*yyzz22 
!     (PZ S/SS)
            w(7) = ri(2)*x(3) 
!     (PZ PX/S S)
            w(8) = ri(3)*xx31 + ri(4)*zz31 
!     (PZ PY/S S)
            w(9) = ri(3)*xx32 + ri(4)*zz32 
!     (PZ PZ/S S)
            w(10) = ri(3)*xx33 + ri(4)*zz33 
            ki = 10 
          endif 
        endif 
!
! *** NOW ROTATE THE NUCLEAR ATTRACTION INTEGRALS.
! *** THE STORAGE OF THE NUCLEAR ATTRACTION INTEGRALS  CORE(KL/IJ) IS
!     (SS/)=1,   (SO/)=2,   (OO/)=3,   (PP/)=4
!
        e1b(1) = -css1 
        if (natorb(ni) > 3) then 
          e1b(2) = -csp1*x(1) 
          e1b(3) = (-cpps1*xx11) - cppp1*yyzz11 
          e1b(4) = -csp1*x(2) 
          e1b(5) = (-cpps1*xx21) - cppp1*yyzz21 
          e1b(6) = (-cpps1*xx22) - cppp1*yyzz22 
          e1b(7) = -csp1*x(3) 
          e1b(8) = (-cpps1*xx31) - cppp1*zz31 
          e1b(9) = (-cpps1*xx32) - cppp1*zz32 
          e1b(10) = (-cpps1*xx33) - cppp1*zz33 
        endif 
        e2a(1) = -css2 
        if (natorb(nj) > 3) then 
          e2a(2) = -csp2*x(1) 
          e2a(3) = (-cpps2*xx11) - cppp2*yyzz11 
          e2a(4) = -csp2*x(2) 
          e2a(5) = (-cpps2*xx21) - cppp2*yyzz21 
          e2a(6) = (-cpps2*xx22) - cppp2*yyzz22 
          e2a(7) = -csp2*x(3) 
          e2a(8) = (-cpps2*xx31) - cppp2*zz31 
          e2a(9) = (-cpps2*xx32) - cppp2*zz32 
          e2a(10) = (-cpps2*xx33) - cppp2*zz33 
        endif 
        if (abs(tore(ni))>20.D0 .and. abs(tore(nj))>20.D0) then 
! SPARKLE-SPARKLE INTERACTION
          enuc = 0.D0 
          return  
        else if (rij<1.D0 .and. natorb(ni)*natorb(nj)==0) then 
          enuc = 0.D0 
          return  
        endif 
        scale = exp((-alp(ni)*rij)) + exp((-alp(nj)*rij)) 
!
        nt = ni + nj 
        if (nt==8 .or. nt==9) then 
          if (ni==7 .or. ni==8) scale = scale + (rij - 1.D0)*exp((-alp(ni)*rij)&
            ) 
          if (nj==7 .or. nj==8) scale = scale + (rij - 1.D0)*exp((-alp(nj)*rij)&
            ) 
        endif 
        enuc = tore(ni)*tore(nj)*gam 
        scale = abs(scale*enuc) 
        if (method_am1 .and. (ni==5 .or. nj==5)) then 
!
!   LOAD IN AM1 BORON GAUSSIANS
!
          nk = ni + nj - 5 
!   NK IS THE ATOMIC NUMBER OF THE NON-BORON ATOM
          nl = 1 
          if (nk == 1) nl = 2 
          if (nk == 6) nl = 3 
          if (nk==9 .or. nk==17 .or. nk==35 .or. nk==53) nl = 4 
          guess1(5,:3) = boron1(:,nl) 
          guess2(5,:3) = boron2(:,nl) 
          guess3(5,:3) = boron3(:,nl) 
        endif 
        if (method_am1 .or. method_pm3) then 
          do ig = 1, 4 
            if (abs(guess1(ni,ig)) > 0.D0) then 
              ax = guess2(ni,ig)*(rij - guess3(ni,ig))**2 
              if (ax <= 25.D0) scale = scale + tore(ni)*tore(nj)/rij*guess1(ni,ig)&
                *exp((-ax)) 
            endif 
            if (abs(guess1(nj,ig)) <= 0.D0) cycle  
            ax = guess2(nj,ig)*(rij - guess3(nj,ig))**2 
            if (ax > 25.D0) cycle  
            scale = scale + tore(ni)*tore(nj)/rij*guess1(nj,ig)*exp((-ax)) 
          end do 
        endif 
        enuc = enuc + scale 
!
        if (natorb(ni)*natorb(nj) == 0) ki = 0 
        kr = kr + ki 
!
!
      endif 
      return  
      end subroutine rotate 
