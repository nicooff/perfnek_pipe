!=======================================================================
!     Adam Peplinski; 2015.10.20
!     Set of subroutines to read user and module parameters using 
!     namelists.
!     
!=======================================================================
!***********************************************************************
!     read parameters from the file
      subroutine uprm_read
      implicit none

      include 'SIZE_DEF'
      include 'SIZE'            ! NID
      include 'INPUT_DEF'
      include 'INPUT'           ! REAFLE

!     local variables
      integer len, ierr
      integer iunit

      character*132 fname 

!     functions
      integer ltrunc
!-----------------------------------------------------------------------
!     Open parameter file and read contents
      ierr=0
      if (NID.eq.0) then
!     find free unit
         call IO_freeid(iunit, ierr)
!     get file name and open file
         if(ierr.eq.0) then
            call blank(fname,132)
            len = ltrunc(REAFLE,132) - 4
            call chcopy(fname,REAFLE,len)
            fname(len+1:len+6)='.upar'
            write(6,*) 'Openning parameter file: ',trim(fname)
            open (unit=iunit,file=fname,status='old',action='read',
     $           iostat=ierr)
         endif
      endif
      call err_chk(ierr,'Error opening .upar file.$')

!     place to call module _param_in routines
      call uprm_in(iunit)

!     close the file
      ierr=0
      if (NID.eq.0) close(unit=iunit,iostat=ierr)
      call err_chk(ierr,'Error closing .upar file.$')

!     stamp logs
      if (NIO.eq.0) write(*,*) 'User parameter list'
      call uprm_out(6)
      if (NIO.eq.0) write(*,*) 

      return
      end
!***********************************************************************
!     read parameters
      subroutine uprm_in(iunit)
      implicit none

      include 'SIZE_DEF'
      include 'SIZE'

!     argument list
      integer iunit
!-----------------------------------------------------------------------
!     place to call module _param_in routines

!     user parameters
!      rewind(iunit)
!      call user_param_in(iunit)

      if (NID.eq.0) rewind(iunit)
!     restart
      call chkpt_param_in(iunit)

      return
      end
!***********************************************************************
!     output parameters
      subroutine uprm_out(iunit)
      implicit none

!     argument list
      integer iunit
!-----------------------------------------------------------------------
!     place to call module _param_in routines

!     user parameters
!      call user_param_out(iunit)

!     restart
      call chkpt_param_out(iunit)

      return
      end
!***********************************************************************
!     read user parameters
      subroutine user_param_in(fid)
      implicit none

      include 'SIZE_DEF'
      include 'SIZE'            ! NID
      include 'PARALLEL_DEF' 
      include 'PARALLEL'        ! ISIZE, WDSIZE, LSIZE,CSIZE
      include 'USERPAR'         !

!     argument list
      integer fid

!     local variables
      integer ierr, len, lisent, lrsent
      parameter (lisent = 2, lrsent = 3)
      integer itmp(lisent)
      real rtmp(lrsent)


      return
      end
!***********************************************************************
!     write user parameters
      subroutine user_param_out(fid)
      implicit none

      include 'SIZE_DEF'
      include 'SIZE'            !
      include 'USERPAR'         !

!     argument list
      integer fid               ! file id

!     local variables
      integer ierr


      return
      end
!***********************************************************************
