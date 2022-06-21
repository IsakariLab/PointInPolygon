program gen_points
  implicit none
  integer,parameter :: nx=100, ny=100
  real(8),parameter :: xlim(2)=[-0.99d0,0.99d0]
  real(8),parameter :: ylim(2)=[-0.99d0,0.99d0]
  
  integer,parameter :: n=(nx+1)*(ny+1)
  real(8) :: x(2,n)
  
  integer :: iy, ix, i
  real(8) :: xx, yy
  
  i=0
  do iy=0,ny
     yy=ylim(1)+(ylim(2)-ylim(1))*dble(iy)/dble(ny)
     do ix=0,nx
        xx=xlim(1)+(xlim(2)-xlim(1))*dble(ix)/dble(nx)
        i=i+1
        x(1,i)=xx
        x(2,i)=yy
     end do
  end do

  open(1,file="points.dat")
  write(1,*) n
  do i=1,n
     write(1,*) i, x(:,i)
  end do
  close(1)

end program gen_points
