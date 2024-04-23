program test_WKT_parser
    !
    !Quiero una función que lea un wkt_string y me diga:
    ! - que tipo es (point, line, polygon)
    ! - me de sus coordenadas como array xy(n,:)
    !
    ! uso:  call read_wkt(string, xy, type)
    !
    implicit none

    character(len=200) :: WKT_str_point=''
    character(len=200) :: WKT_str_line =''
    character(len=200) :: WKT_str_poly =''
    character(len=10 ) :: WKT_typ=''
    real, allocatable :: coords(:,:)
    !integer :: n, ierr, i, p

    ! Example WKT string
    !POINT (30 10)
    !LINESTRING (30 10, 10 30, 40 40)
    !POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))
    !POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10),(20 30, 35 35, 30 20, 20 30))
    !MULTIPOINT ((10 40), (40 30), (20 20), (30 10))
    !MULTIPOINT (10 40, 40 30, 20 20, 30 10)
    !MULTILINESTRING ((10 10, 20 20, 10 40),(40 40, 30 30, 40 20, 30 10))
    !MULTIPOLYGON (((30 20, 45 40, 10 40, 30 20)),((15 5, 40 10, 10 20, 5 10, 15 5)))
    !MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)),((20 35, 10 30, 10 10, 30 5, 45 20, 20 35),(30 20, 20 15, 20 25, 30 20)))
    WKT_str_point = 'POINT  (10 20)'
    WKT_str_line  = 'LINESTRING ((10 20, 30 40, 50 60, 10 20))'
    WKT_str_poly  = 'POLYGON((10 20, 30 40, 50 60, 10 20))'
    !WKT_str = '10 20, 30 40, 50 60, 10 20'


    !testing:
    print '("---",/,"Test 1: ",A)',wkt_str_point
    call parse_wkt(WKT_str_point, coords, wkt_typ)
    print '(A10)',       wkt_typ
    print '(2(F12.3))', transpose(coords)

    print '("---",/,"Test 2: ",A)',wkt_str_line 
    call parse_wkt(WKT_str_line , coords, wkt_typ)
    print '(A10)',       wkt_typ
    print '(2(F12.3))', transpose(coords)

    print '("---",/,"Test 3: ",A)',wkt_str_poly 
    call parse_wkt(WKT_str_poly , coords, wkt_typ)
    print '(A10)',       wkt_typ
    print '(2(F12.3))', transpose(coords)
contains

subroutine parse_wkt(wkt_str, xy, typ)!, sep)
   !parse "Single Feature" wkt string. Output type and xy-coordinates array.
   implicit none
   character(len=*) , intent(in)    :: wkt_str  !in 
   real, allocatable, intent(inout) :: xy(:,:)  !out
   character(len=*) , intent(inout) :: typ      !out
   !character          , intent(in)    :: sep=','  !in (optional)
   !
   integer           :: op,cp,comma               !positions of "open" and "close" parentesis, and coordinate "comma
   integer           :: n,m,nbp                   !# of coordinates, # of shapes, # of boundary parenthesis
   integer           :: i!,j,k
   character(len=500) :: str
   character(len=300) :: substr
   character(len=100) :: xystr         

   str=wkt_str

   op = index(str,"(")     !start coordinates  TYPE (( ... ))
   cp = index(str,")")     !  end coordinates  TYPE (( ... ))

   typ=trim(str(:op-1))    !get type: point, polygon, linestring      
   str(:op)=''             !clean type from buffer string

   !print*,"_",trim(typ),"_"
   !SINGLE-features
   if ( typ(1:4) == "POIN"      ) nbp=0 
   if ( typ(1:4) == "LINE"      ) nbp=1 
   if ( typ(1:4) == "POLY"      ) nbp=1 
   !MULTI-features
   if ( typ(1:4) == "MULTIPOIN" ) nbp=1 
   if ( typ(1:4) == "MULTILINE" ) nbp=2 
   if ( typ(1:4) == "MULTIPOLY" ) nbp=2 

   m=count( [ ( str(i:i) == '(',i=1, len(str) ) ] ) - nbp

   if (m>1) print '("Warning: WKT String has multiple features! Only first will be processed")'

   !do i=1,m   !for each feature

   substr=str( op+1+nbp : cp-1 )                               !feature's coordinates string (without parenthesis)
   n=count( [ ( substr(i:i) == ',',i=1, len(substr) ) ] ) + 1  !count feature's # points

   if ( allocated(xy) ) deallocate(xy)                         !allocate coordinates array
   allocate(xy(n,2))

   !print*,"n=",n          !debug 
   !print*,"str=",str      !debug
   !print*,"substr=",substr!debug 
   do i=1, n !for each point
      if ( n>1 .and. i < n ) then
         comma =index(substr,',')
         xystr=trim(substr(1:comma))
      else
         xystr=trim(substr)           !for points or last coordinate
      end if
      !print*,xystr         !debug
      read(xystr,*) xy(i,:)
      if (n>1 .and. i < n ) substr(1:comma)=''

   enddo!point
   !ENDDO !feature
end subroutine

end program
