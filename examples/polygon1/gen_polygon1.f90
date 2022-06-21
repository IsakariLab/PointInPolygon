program gen_polygon1
  implicit none
  integer,parameter :: nunit=100
  real(8),parameter :: pi=acos(-1.d0)
  
  integer :: i, ic, n, icrv(2,7)
  real(8) :: rad, lngth, th, xx, yy, cx, cy
  
  ! 境界要素の数を数える
  ic=0
  ! 大外の円 (1-->2, 内向き)
  rad=1.d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  ic=ic+n
  
  ! 真ん中の円 (3-->2, 外向き)
  rad=0.4d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  ic=ic+n
  
  ! 内側の円 (3-->4, 内向き)
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  ic=ic+n
  
  ! 右の円 (2-->5, 内向き)
  cx=0.7d0; cy=0.d0
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  ic=ic+n

  ! 上の円 (2-->6, 内向き)
  cx=0.0d0; cy=0.7d0
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  ic=ic+n

  ! 左の円 (2-->7, 内向き)
  cx=-0.7d0; cy=0.0d0
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  ic=ic+n

  ! 下の円 (2-->8, 内向き)
  cx=-0.0d0; cy=-0.7d0
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  ic=ic+n

  
  ! 境界要素を作る
  open(1,file="polygon1.dat")
  write(1,*) ic

  ic=0
  ! 大外の円 (1-->2, 内向き)
  rad=1.d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  icrv(1,1)=ic+1
  do i=1,n
     th=2.d0*pi*dble(i)/dble(n)
     xx=rad*cos(th)
     yy=rad*sin(th)
     write(1,*) i+icrv(1,1)-1, xx, yy
     ic=ic+1
  end do
  icrv(2,1)=ic

  ! 真ん中の円 (3-->2, 外向き)
  rad=0.4d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  icrv(1,2)=ic+1
  do i=1,n
     th=2.d0*pi*dble(i)/dble(n)
     xx=rad*cos(th)
     yy=rad*sin(th)
     write(1,*) i+icrv(1,2)-1, xx, yy
     ic=ic+1
  end do
  icrv(2,2)=ic

  ! 内側の円 (3-->4, 内向き)
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  icrv(1,3)=ic+1
  do i=1,n
     th=2.d0*pi*dble(i)/dble(n)
     xx=rad*cos(th)
     yy=rad*sin(th)
     write(1,*) i+icrv(1,3)-1, xx, yy
     ic=ic+1
  end do
  icrv(2,3)=ic

  ! 右の円 (2-->5, 内向き)
  cx=0.7d0; cy=0.d0
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  icrv(1,4)=ic+1
  do i=1,n
     th=2.d0*pi*dble(i)/dble(n)
     xx=cx+rad*cos(th)
     yy=cy+rad*sin(th)
     write(1,*) i+icrv(1,4)-1, xx, yy
     ic=ic+1
  end do
  icrv(2,4)=ic

  ! 上の円 (2-->6, 内向き)
  cx=0.0d0; cy=0.7d0
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  icrv(1,5)=ic+1
  do i=1,n
     th=2.d0*pi*dble(i)/dble(n)
     xx=cx+rad*cos(th)
     yy=cy+rad*sin(th)
     write(1,*) i+icrv(1,5)-1, xx, yy
     ic=ic+1
  end do
  icrv(2,5)=ic

  ! 左の円 (2-->7, 内向き)
  cx=-0.7d0; cy=0.0d0
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  icrv(1,6)=ic+1
  do i=1,n
     th=2.d0*pi*dble(i)/dble(n)
     xx=cx+rad*cos(th)
     yy=cy+rad*sin(th)
     write(1,*) i+icrv(1,6)-1, xx, yy
     ic=ic+1
  end do
  icrv(2,6)=ic

  ! 下の円 (2-->8, 内向き)
  cx=-0.0d0; cy=-0.7d0
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  icrv(1,7)=ic+1
  do i=1,n
     th=2.d0*pi*dble(i)/dble(n)
     xx=cx+rad*cos(th)
     yy=cy+rad*sin(th)
     write(1,*) i+icrv(1,7)-1, xx, yy
     ic=ic+1
  end do
  icrv(2,7)=ic

  write(1,*) ic

  ic=0
  ! 大外の円 (1-->2, 内向き)
  rad=1.d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  do i=1,n-1
     write(1,*) i+icrv(1,1)-1, i+1+icrv(1,1)-1, i+icrv(1,1)-1, 1, 2
  end do
  write(1,*) icrv(2,1), icrv(1,1), icrv(2,1), 1, 2

  ! 真ん中の円 (3-->2, 外向き)
  rad=0.4d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  do i=1,n-1
     write(1,*) i+icrv(1,2)-1, i+icrv(1,2)-1, i+1+icrv(1,2)-1, 3, 2
  end do
  write(1,*) icrv(2,2), icrv(2,2), icrv(1,2), 3, 2

  ! 内側の円 (3-->4, 内向き)
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  do i=1,n-1
     write(1,*) i+icrv(1,3)-1, i+1+icrv(1,3)-1, i+icrv(1,3)-1, 3, 4
  end do
  write(1,*) icrv(2,3), icrv(1,3), icrv(2,3), 3, 4

  ! 右の円 (2-->5, 内向き)
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  do i=1,n-1
     write(1,*) i+icrv(1,4)-1, i+1+icrv(1,4)-1, i+icrv(1,4)-1, 2, 5
  end do
  write(1,*) icrv(2,4), icrv(1,4), icrv(2,4), 2, 5

  ! 上の円 (2-->6, 内向き)
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  do i=1,n-1
     write(1,*) i+icrv(1,5)-1, i+1+icrv(1,5)-1, i+icrv(1,5)-1, 2, 6
  end do
  write(1,*) icrv(2,5), icrv(1,5), icrv(2,5), 2, 6

  ! 左の円 (2-->7, 内向き)
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  do i=1,n-1
     write(1,*) i+icrv(1,6)-1, i+1+icrv(1,6)-1, i+icrv(1,6)-1, 2, 7
  end do
  write(1,*) icrv(2,6), icrv(1,6), icrv(2,6), 2, 7

  ! 下の円 (2-->8, 内向き)
  rad=0.2d0
  lngth=2.d0*rad*pi
  n=ceiling(nunit*lngth)
  do i=1,n-1
     write(1,*) i+icrv(1,7)-1, i+1+icrv(1,7)-1, i+icrv(1,7)-1, 2, 8
  end do
  write(1,*) icrv(2,7), icrv(1,7), icrv(2,7), 2, 8

  close(1)
    
end program gen_polygon1
