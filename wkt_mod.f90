module wkt
  !
  !Subrutina que lee "wkt string" devuelve tipo (point, line, poly, etc.)
  ! y array con coordenadas xy(n,2)
  !
  ! uso:  call parse_wkt(string, type, xy)
  implicit none

contains

subroutine parse_wkt(wkt_str, typ, xy)
   !parse "Single Feature" wkt string. Output type and xy-coordinates array.
   implicit none
   character(len=*) , intent(in)    :: wkt_str                          !in 
   character(len=*) , intent(inout) :: typ                              !out
   real, allocatable, intent(inout) :: xy(:,:)                          !out
   !
   integer            :: op,cp,sep                                      !position of "open"/"close" parentesis, and coordinate separator
   integer            :: n,m,nbp                                        !# of coordinates in a feature, # of features, # of boundary parenthesis
   integer            :: i!,j,k
   character(len=900) :: str                                            ! total      string buffer
   character(len=880) :: fstr                                           ! feature    string buffer
   character(len=50)  :: xystr                                          ! coordinate string buffer

   op = index(wkt_str,"("            )                                  !begin coordinates  <TYPE> ( <COORDINATES> )
   cp = index(wkt_str,")",back=.true.)                                  !  end coordinates  <TYPE> ( <COORDINATES> )

   !GET TYPE:
   typ=trim(wkt_str(:op-1))                                             !get type: point, polygon, linestring, etc.

   str=wkt_str(op+1:cp-1)                                               !transfer input to buffer (only coordinates)

   !Depending on type, get # of "boundary parenthesis" (taken as feature separator)
   !SINGLE-features
   if ( typ(1:4) == "POIN"      ) nbp=0                                 !POINT ( x y ) 
   if ( typ(1:4) == "LINE"      ) nbp=0                                 !LINE  (x1 y1, x2 y2,...,xn yn)
   if ( typ(1:4) == "POLY"      ) nbp=1                                 !POLY ((x1 y1, x2 y2,...), (...))
   !MULTI-features
   if ( typ(1:9) == "MULTIPOIN" ) nbp=1                                 !MULTIPOINT ((x1 y1), (x2 y2),..., (xn yn))
   if ( typ(1:9) == "MULTILINE" ) nbp=1                                 !MULTILINE  ((x1 y1, x2 y2,..., xn yn), (...))
   if ( typ(1:9) == "MULTIPOLY" ) nbp=2                                 !MULTIPOLY (((x1 y1, x2 y2,..., xn yn),( ...)),(( ...)))

   !GET 1ST FEATURE:
   if (nbp > 0) then
      m=count( [ ( str(i:i+nbp-1) == repeat(")",nbp),i=1, len(str)-nbp ) ] )
      if (m>1) print '("(!) WKT: ",A)',trim(str)
      if (m>1) print '("Warning: WKT with multiple (",I1,") features! Only 1st will be considered.")',m
      op  = index(str,"(")                                              !begin 1st feature coordinates  TYPE (( ... ))
      cp  = index(str,")")                                              !  end 1st feature coordinates  TYPE (( ... ))
      fstr=str( op+nbp : cp-1 )                                         !feature's coordinates string (without parenthesis)
   else
      fstr=str
   end if

   !DO i=1,m                                                            !for each feature
   n=count( [ ( fstr(i:i) == ',',i=1, len(fstr) ) ] ) + 1               !get # of points on feature
   if ( allocated(xy) ) deallocate(xy); allocate(xy(n,2))               !allocate coordinates array
   print*,"fstr=",fstr    !debug 
   print*,"n=",n          !debug 
   do i=1, n                                                            !for each point
   if ( n > 1 .and. i < n ) then                                        ! if more than 1 point, and point not the last one
         sep=index(fstr,',')                                            !get location of separator ","
         xystr=trim(adjustl(fstr(1:sep)))                                        !take "x y" point coordinates
         fstr(1:sep)=''                                                 !remove this point from feature string to keep reading
      else
         xystr=trim(adjustl(fstr))                                               !for points or last coordinate
      end if
      print*,xystr        !debug
      read(xystr,*) xy(i,:)                                             !transfer xy to out array
   enddo!points
   !ENDDO!features
end subroutine

end module 
