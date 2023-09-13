program comp
  implicit none

  real(8),parameter :: pi=acos(-1.d0)
  
  integer :: n, i, j
  real(8) :: th, x, y

  n = 2000

  open(1,file="medama.el2")
  write(1,*) n !節点数
  !上側ブーメランを書く
  do i=1,n/2
     th=2.d0*pi*dble(i)/dble(n/2)
     x= -cos(th)
     y= 0.42 - 0.2*sin(th)-0.4*cos(2*th)
     write(1,*) i, x, y !節点座標
  end do
  !下側ブーメランを書く
  do i=1,n/2
     th=2.d0*pi*dble(i)/dble(n/2)
     x= -cos(th)
     y= -0.42 - 0.2*sin(th) +0.4*cos(2*th)
     write(1,*) i+n/2, x, y !節点座標
  end do

  write(1,*) n !要素数
  !! 上側ブーメラン
  do i =1, n/2-1
    write(1,*) i,i+1,i,1,2
  enddo
  write(1,*) n/2,1,n/2,1,2
  
  !! 下側ブーメラン
  do i =n/2+1,n-1
    write(1,*) i, i+1, i, 1, 2
  enddo
  write(1,*) n, n/2+1, n, 1, 2

end program comp
