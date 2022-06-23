program main
  implicit none

  integer,parameter :: n = 250
  real(8),parameter :: pi = acos(-1.d0)
  real(8) :: x_unit(2,n)
  real(8) :: x(2,4*n)
  integer :: nd(2,4*n)

  integer :: i, j, k
  real(8) :: th
  real(8) :: rot

  ! 単位構造を生成
  do i=1,n
     th = 2.d0*pi*dble(i-1)/dble(n)
     x_unit(1,i) = 0.25d0*(cos(th)+cos(2.d0*th))+0.5d0
     x_unit(2,i) = 0.2d0*sin(th)
  end do

  ! メッシュを書き出し
  open(1,file="resonator_demo.el2")

  ! 座標を設定
  write(1,*) 4*n
  do j=1,4
     do i=1,n
        k=i+(j-1)*n
        rot = 0.25d0*(2.d0*dble(j)-1.d0)*pi
        x(1,k)=+x_unit(1,i)*cos(rot)+x_unit(2,i)*sin(rot)
        x(2,k)=-x_unit(1,i)*sin(rot)+x_unit(2,i)*cos(rot)
        write(1,*) k, x(:,k)
     end do
  end do

  ! connectivity を設定
  write(1,*) 4*n
  do j=1,4
     do i=1,n-1
        k=i+(j-1)*n
        nd(1,k)=i+1+(j-1)*n
        nd(2,k)=i+(j-1)*n
        write(1,*) k, nd(:,k), 1, j+1
     end do
     k=n+(j-1)*n
     nd(1,k)=1+(j-1)*n
     nd(2,k)=n+(j-1)*n
     write(1,*) k, nd(:,k), 1, j+1
  end do

  close(1)
  

  ! meshの書き出し
  open(1,file="resonator_demo.gp")
  do i=1,n*4
     write(1,*) x(:,nd(1,i))
     write(1,*) x(:,nd(2,i))
     write(1,*)
     write(1,*)
  end do
  close(1)

  
end program main
