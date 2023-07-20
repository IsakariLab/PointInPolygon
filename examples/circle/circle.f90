program main
  implicit none

  integer,parameter :: n=500
  real(8),parameter :: cx=0.d0, cy=0.d0, rad=1.d0
  real(8),parameter :: pi=acos(-1.d0)
  character(len=256) :: filename="circle_c00_r1_n500"

  integer :: i, itmp
  real(8) :: th, xx, yy
  integer :: np, ne
  integer :: nd(2,n)
  real(8) :: x(2,n)

  
  
  open(1,file=trim(trim(filename)//".el2"))
  write(1,*) n
  do i=1,n
     th=2.d0*pi*dble(i-1)/dble(n)
     xx=cx+rad*cos(th)
     yy=cy+rad*sin(th)
     write(1,*) i, xx, yy
  end do
  write(1,*) n
  do i=1,n-1
     write(1,*) i, i+1, i, 1, 2
  end do
  write(1,*) n, 1, n, 1, 2
  close(1)

  open(1,file=trim(trim(filename)//".el2"))
  read(1,*) np
  do i=1,np
     read(1,*) itmp, x(:,i)
  end do
  read(1,*) ne
  do i=1,ne
     read(1,*) itmp, nd(:,i)
  end do
  close(1)

  open(1,file=trim(trim(filename)//".gp"))
  do i=1,ne
     write(1,*) x(:,nd(1,i))
     write(1,*) x(:,nd(2,i))
     write(1,*)
     write(1,*)
  end do
  close(1)

end program main
