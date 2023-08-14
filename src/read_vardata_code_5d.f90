  !> @file
  !! @brief Read 5D var data.
  !! optional return errcode
  !! @author Jeff Whitaker, Cory Martin
  type(Dataset), intent(in) :: dset
  character(len=*), intent(in) :: varname
  integer, allocatable, dimension(:) :: start, icount
  integer, intent(out), optional :: errcode
  integer, intent(in), optional :: nslice
  integer, intent(in), optional :: slicedim
  integer, intent(in), optional :: ncstart(5)
  integer, intent(in), optional :: nccount(5)
  integer ncerr, nvar, n1,n2,n3,n4,n5,nd,ncount
  logical return_errcode
  ! check if use the errcode
  if(present(errcode)) then
     return_errcode=.true.
     errcode = 0
  else
     return_errcode=.false.
  endif
  if (present(nslice)) then
     ncount = nslice
  else
     ncount = 1
  endif
  nvar = get_nvar(dset,varname)
  if (dset%variables(nvar)%ndims /= 5) then
     if (return_errcode) then
        errcode=nf90_ebaddim
        return
     else
        print *,'rank of data array != variable ndims (or ndims-1)'
        stop 99
     endif
  endif
  if (present(slicedim)) then
     nd = slicedim
  else
     nd = dset%variables(nvar)%ndims
  end if
  n1 = dset%variables(nvar)%dimlens(1)
  n2 = dset%variables(nvar)%dimlens(2)
  n3 = dset%variables(nvar)%dimlens(3)
  n4 = dset%variables(nvar)%dimlens(4)
  n5 = dset%variables(nvar)%dimlens(5)
  ! allocate/deallocate values
  allocate(start(dset%variables(nvar)%ndims),icount(dset%variables(nvar)%ndims))
  start(:) = 1; icount(1)=n1; icount(2)=n2; icount(3)=n3; icount(4)=n4; icount(5)=n5
  if (present(ncstart) .and. present(nccount)) then
     start(1)=ncstart(1); icount(1)=nccount(1)
     start(2)=ncstart(2); icount(2)=nccount(2)
     start(3)=ncstart(3); icount(3)=nccount(3)
     start(4)=ncstart(4); icount(4)=nccount(4)
     start(5)=ncstart(5); icount(5)=nccount(5)
  else if (present(nslice)) then
     start(nd)=ncount; icount(nd)=1
  endif
  allocate(values(icount(1),icount(2),icount(3),icount(4),icount(5)))
  ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values,&
                       start,icount)
  deallocate(start,icount)
  ! err check
  if (return_errcode) then
     call nccheck(ncerr,halt=.false.)
     errcode=ncerr
  else
     call nccheck(ncerr)
  endif
