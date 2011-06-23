!A module for automatically opening files
!Pass in a file path as a 100 letter character variable and it returns the unit number 
!assigned to it
!Now with new magic to open a file for readin as well
!openin opens a file as input, openout opens a file as output
!Overwrites a preexisting file without confirmation, this is by design so change it if you
!don't like it

!Jason Hite
!Originally Written: 4 Apr 2009
!Last Updated: 9 Oct 2010

module io
implicit none

contains

function openin (filepath) !Opens a file as input, returns the unitnum of the file
integer ioerr, unum, openin
character(100) filepath

unum = get_unit()

open(unit = unum, file=filepath, action="read", iostat=ioerr)
 if ( ioerr /= 0 ) then
  stop "File read error"
 end if

openin = unum

end function

function openout (filepath) !Opens a file for output, returns the unitnum of the file
integer ioerr, unum, openout
character(100) filepath

unum = get_unit()

open(unit = unum, file=filepath, action="write", status="new", iostat=ioerr)
 if ( ioerr /= 0 ) then
  write(*,*) "File exists, replacing " , filepath !Hope you didn't need whatever was in it
  open(unit = unum, file=filepath, action="write", status="replace", iostat=ioerr)  
    if ( ioerr /= 0 ) then
     stop "File output error"
 end if
end if
openout = unum

end function

function openappend (filepath) !Opens a file for appending, returns unitnum
integer ioerr, unum, openappend !NOTE: DOES NOT START A NEW LINE
character(100) filepath

unum = get_unit()

open(unit = unum, file=filepath, action='write', status='new', iostat=ioerr)
 if ( ioerr /= 0 ) then
  write(*,*) "File exists, appending ", filepath
   open(unit = unum, file=filepath, action='write', access='append', status="old", iostat=ioerr)
    if ( ioerr /= 0 ) then
     stop "File output error"
    end if
 end if
openappend = unum

end function


!The following code is used with permission under the LGPL license
!The code here exists as a heavily modified version of the original because I like functions
!a lot more than subroutines
!Author: John Burkardt
!Last Updated (by the original author): 26 Oct 2008

function get_unit ()
integer i, get_unit, ioerr
logical lopen

do i = 1, 99
 if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then
  inquire ( unit = i, opened = lopen, iostat = ioerr )
  if ( ioerr == 0 ) then
   if ( .not. lopen ) then
    get_unit = i
    return
   end if
  end if
 end if
end do

end function

end module io

!Legal license stuff. Since LGPL code was used, this is LGPL too:

!Copyright 2009 Jason Hite

!This library is free software: you can redistribute it and/or
!modify it under the terms of the GNU Lesser General Public
!License as published by the Free Software Foundation, either
!version 3 of the License, or (at your option) any later version.
 
!This library is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!Lesser General Public License for more details.

!See <http://www.gnu.org/licenses/> to obtain a copy of the license
