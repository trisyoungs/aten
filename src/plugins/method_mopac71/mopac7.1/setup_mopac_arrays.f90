  subroutine setup_mopac_arrays(n, mode)
  use permanent_arrays, only : geo, coord, na, nb, nc, simbol, atmass, &
     & labels, loc, xparam, nat, nfirst, nlast, uspd, pdiag, xparef, &
     & h, w, p, pa, pb, f, c, eigs, dxyz, grad, eigb, txtatm, fb, cb, &
     & wj, wk, errfn, aicorr
  use iter_C, only : pold, pbold, pold2, pbold2, pold3, pbold3
  use symmetry_C, only : depmul, jelem, locpar, idepfn, locdep
  use molkst_C, only : mpack, n2elec, norbs, numat, nvar, method_mndod
  implicit none

    integer :: n, mode
    if (n > 0) then
      if (mode == 1) then
!
! Create essential arrays only at this point
!
        allocate(geo(3,n), coord(3,n), na(n), nb(n), nc(n))
        allocate(simbol(n), atmass(n), labels(n), loc(2, 3*n))
        allocate(xparam(3*n), nat(n), nfirst(n), nlast(n), uspd(9*n))
        allocate(pdiag(9*n), xparef(3*n), depmul(n), jelem(20, n))
        allocate(txtatm(n), locpar(3*n), idepfn(3*n), locdep(3*n))
        nfirst = -9999
        nlast = -9999
!
      else
!
!  Create main arrays - at this point, the array sizes are all known
!
        if (allocated(h)) deallocate(h)
        allocate(h(mpack), p(mpack), pa(mpack), pb(mpack))
        allocate(pold(6*mpack),  pold2(6*mpack), f(mpack))

        allocate(c(norbs, norbs), wj(1), wk(1), eigs(norbs), dxyz(3*numat), grad(nvar))
        allocate(eigb(norbs), pold3(max(mpack, 400)))
        allocate(errfn(nvar), aicorr(nvar))
        if (method_mndod) then
          allocate(w(n2elec**2))
        else
          allocate(w(n2elec))
        end if
        dxyz = 0.d0
        errfn = 0.d0
        aicorr = 0.d0
!--TGAY 08/2016- grad was zeroed before allocation on first call
        grad = 0.d0
!---------------
   !       If UHF then:
!
        allocate(fb(mpack), cb(norbs, norbs), pbold(6*mpack), pbold2(6*mpack), &
        & pbold3(max(mpack, 400)))
      end if
    else
!
!  Delete all arrays
!
    if (allocated(h))      deallocate (h)
    if (allocated(p))      deallocate (p)
    if (allocated(pa))     deallocate (pa)
    if (allocated(pb))     deallocate (pb)
    if (allocated(pold))   deallocate (pold)
    if (allocated(pold2))  deallocate (pold2)
    if (allocated(pold3))  deallocate (pold3)
    if (allocated(f))      deallocate (f)
    if (allocated(c))      deallocate (c)
    if (allocated(eigs))   deallocate (eigs)
    if (allocated(dxyz))   deallocate (dxyz)
    if (allocated(grad))   deallocate (grad)
    if (allocated(eigb))   deallocate (eigb)
    if (allocated(wj))     deallocate (wj)
    if (allocated(w))      deallocate (w)
    if (allocated(wk))     deallocate (wk)
    if (allocated(fb))     deallocate (fb)
    if (allocated(cb))     deallocate (cb)
    if (allocated(pbold))  deallocate (pbold)
    if (allocated(pbold2)) deallocate (pbold2)
    if (allocated(pbold3)) deallocate (pbold3)
    if (allocated(pbold3)) deallocate (pbold3)
    if (allocated(pbold3)) deallocate (pbold3)
    if (allocated(errfn))  deallocate (errfn)
    if (allocated(aicorr)) deallocate (aicorr)
    end if

  end subroutine setup_mopac_arrays
