program test_WKT_parser
    !
    use wkt

    implicit none

    character(len=200) :: WKT_str_point=''
    character(len=200) :: WKT_str_line =''
    character(len=200) :: WKT_str_poly =''
    character(len=200) :: WKT_str_mpoly =''
    character(len=10 ) :: WKT_typ=''
    real, allocatable :: coords(:,:)
    integer :: i!, !n, ierr, i, p
    type tier
      character(len=10) :: tname
      integer :: id
      real, allocatable :: xy(:,:)                         !x-y coordinates     (from INP, NO CAMBIA)
      real :: h,z0                                                    !height,base hgt     (from INP, NO CAMBIA)
    endtype
    type(tier), allocatable :: T(:)

    ! Example WKT string
    WKT_str_point = 'POINT  (10 20)'
    WKT_str_line  = 'LINESTRING (10 20, 30 40, 50 60, 10 20)'
    WKT_str_poly  = 'POLYGON((10 20, 30 40, 50 60, 10 20))'
    WKT_str_mpoly = 'MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)),((20 35, 10 30, 10 10, 30 5, 45 20, 20 35),(30 20, 20 15, 20 25, 30 20)))'

    !testing:
    print '("---",/,"Test 1: ",A)',wkt_str_point
    call parse_wkt(WKT_str_point, wkt_typ, coords)
    print '(A10)',       wkt_typ
    print '(2(F12.3))', transpose(coords)

    print '("---",/,"Test 2: ",A)',wkt_str_line 
    call parse_wkt(WKT_str_line , wkt_typ, coords)
    print '(A10)',       wkt_typ
    print '(2(F12.3))', transpose(coords)

    print '("---",/,"Test 3: ",A)',wkt_str_poly 
    call parse_wkt(WKT_str_poly , wkt_typ, coords)
    print '(A10)',       wkt_typ
    print '(2(F12.3))', transpose(coords)

    print '("---",/,"Test 3: ",A)',wkt_str_mpoly 
   call parse_wkt(WKT_str_mpoly , wkt_typ, coords)
   print '(A10)',       wkt_typ
   print '(2(F12.3))', transpose(coords)

   !!test (harder) read csv and extract wkt strings and parse them:

   call read_csv(fileName='edificios.csv', header=.true., sep=";", quote='"',T=T)
 
   do i=1,size(T)
      print '("name:",A10," id:",I3," z0=",F4.1," h:",F4.1)',T(i)%tname,T(i)%id,T(i)%z0,T(i)%h
      !print '("tier coordinates:",2(F12.3))',T%xy
      print*, T(i)%xy! '("tier coordinates:",2(F12.3))',T%xy
   end do

contains

subroutine read_csv(fileName,header,sep,quote,T)
   implicit none
   character(len=*) ,intent(in)   :: fileName
   logical          ,intent(in)   :: header!=.true.
   character(len=1) ,intent(in)   :: sep,quote !=',',quote='"'
   type(tier)       ,allocatable, intent(inout):: T(:)
   !dummy vars:
   integer             :: iostat,i,j,nrec,ncols,isep,iquote,iquote2
   character(len=1000) :: row
   character(len=990)  :: cell
   character(len=12)   :: wkt_type

   !get number of records on file
   open(1,file=fileName,status="OLD",action="READ",iostat=iostat)

     !get # of records:
     nrec=0
     do while(iostat==0)
        read(1,*,iostat=iostat)
        if (iostat/=0) exit
        nrec=nrec+1
     enddo
     print*," nrec: ",nrec
     if (header) nrec=nrec-1
     !allocate variables according with "nrec"
     if (.not. allocated(T))  allocate(T(nrec))
     close(1)
     !rewind(1)
     
     !READ CONTENT
     open(1,file=fileName,status="OLD",action="READ",iostat=iostat)

     !  HEADER
     if (header) read(1,'(A)') row                                  !skip header
     ncols=count([ ( row(i:i) == sep, i=1, len(row) ) ])+1
     print*,"ncols:",ncols

     !  BODY
     j=1
     do while(iostat==0)
        read(1,'(A)',iostat=iostat) row
        if (iostat/=0) exit
        print*,"ROW:",row
        do i=1,ncols
          if ( i /= ncols) then
             isep  =index(row, sep  )
             iquote=index(row, quote)
             if ( iquote == 0) iquote=99999999
             !print*,iquote,isep

             if ( isep < iquote ) then
                cell=trim(row(1:isep-1))
                row(1:isep)=""
             else
                row(iquote:iquote)=""
                iquote2=index(row, quote)
                cell=trim(row(iquote:iquote2-1))
                row(1:iquote2)=""
             end if

          else
                cell=trim(row(isep:))
          end if

          print*,"*cell:",trim(adjustl(cell)) ! debug
          !wkt;z;name;tier;altura
          if (i==1) T(j)%id   = i!cell
          if (i==1) call parse_wkt(cell,wkt_type,T(j)%xy)
          if (i==3) T(j)%tname=      trim(adjustl(cell))
          if (i==2) T(j)%z0   = ator(trim(adjustl(cell)))
          if (i==5) T(j)%h    = ator(trim(adjustl(cell)))

        end do
        j=j+1
     end do                                        

   close(1)
end subroutine
 
real function ator(str)       !string-> real
  implicit none
  character(len=*), intent(in) :: str
  read(str,*) ator
end function

end program
