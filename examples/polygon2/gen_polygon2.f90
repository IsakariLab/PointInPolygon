program gen_polygon2
  implicit none
  integer,parameter :: nunit=100
  
  integer :: i, ic, n, icrv(2,9)
  real(8) :: xlim(2), ylim(2), xlim2(2), ylim2(2), lngth, xx, yy
  
  ! 境界要素の数を数える
  ic=0
  ! 小さい四角形
  xlim(1)=-0.5d0; xlim(2)=0.5d0
  ylim(1)=0.d0; ylim(2)=0.5d0
  ! 大きい四角形
  xlim2(1)=-0.75d0; xlim2(2)=0.75d0
  ylim2(1)=-0.5d0; ylim2(2)=0.5d0

  lngth=xlim(2)-xlim(1)
  n=ceiling(nunit*lngth)
  ic=ic+n
  lngth=ylim(2)-ylim(1)
  n=ceiling(nunit*lngth)
  ic=ic+n
  lngth=xlim(2)-xlim(1)
  n=ceiling(nunit*lngth)
  ic=ic+n
  lngth=ylim(2)-ylim(1)
  n=ceiling(nunit*lngth)
  ic=ic+n
  lngth=xlim2(2)-xlim(2)
  n=ceiling(nunit*lngth)
  ic=ic+n
  lngth=ylim2(2)-ylim2(1)
  n=ceiling(nunit*lngth)
  ic=ic+n
  lngth=xlim2(2)-xlim2(1)
  n=ceiling(nunit*lngth)
  ic=ic+n
  lngth=ylim2(2)-ylim2(1)
  n=ceiling(nunit*lngth)
  ic=ic+n
  lngth=xlim(1)-xlim2(1)
  n=ceiling(nunit*lngth)
  ic=ic+n
  
  ! 境界要素を作る
  open(1,file="polygon2.dat")
  write(1,*) ic

  ic=0
  ! 線分1 (3->1)
  lngth=xlim(2)-xlim(1)
  n=ceiling(nunit*lngth)
  icrv(1,1)=ic+1
  do i=1,n
     xx=xlim(2)-dble(i)/dble(n)*lngth
     yy=ylim(2)
     write(1,*) i+icrv(1,1)-1, xx, yy
     write(101,*) xx, yy
     ic=ic+1
  end do
  icrv(2,1)=ic

  ! 線分2 (3-->2)
  lngth=ylim(2)-ylim(1)
  n=ceiling(nunit*lngth)
  icrv(1,2)=ic+1
  do i=1,n
     xx=xlim(1)
     yy=ylim(2)-dble(i)/dble(n)*lngth
     write(1,*) i+icrv(1,2)-1, xx, yy
     write(102,*) xx, yy
     ic=ic+1
  end do
  icrv(2,2)=ic

  ! 線分3 (3-->2)
  lngth=xlim(2)-xlim(1)
  n=ceiling(nunit*lngth)
  icrv(1,3)=ic+1
  do i=1,n
     xx=xlim(1)+dble(i)/dble(n)*lngth
     yy=ylim(1)
     write(1,*) i+icrv(1,3)-1, xx, yy
     write(103,*) xx, yy
     ic=ic+1
  end do
  icrv(2,3)=ic

  ! 線分4 (3-->2)
  lngth=ylim(2)-ylim(1)
  n=ceiling(nunit*lngth)
  icrv(1,4)=ic+1
  do i=1,n
     xx=xlim(2)
     yy=ylim(1)+dble(i)/dble(n)*lngth
     write(1,*) i+icrv(1,4)-1, xx, yy
     write(104,*) xx, yy
     ic=ic+1
  end do
  icrv(2,4)=ic

  ! 線分5 (1-->3)
  lngth=xlim2(2)-xlim(2)
  n=ceiling(nunit*lngth)
  icrv(1,5)=ic+1
  do i=1,n
     xx=xlim(2)+dble(i)/dble(n)*lngth
     yy=ylim2(2)
     write(1,*) i+icrv(1,5)-1, xx, yy
     ic=ic+1
     write(105,*) xx, yy
  end do
  icrv(2,5)=ic

  ! 線分6 (1-->3)
  lngth=ylim2(2)-ylim2(1)
  n=ceiling(nunit*lngth)
  icrv(1,6)=ic+1
  do i=1,n
     xx=xlim2(2)
     yy=ylim2(2)-dble(i)/dble(n)*lngth
     write(1,*) i+icrv(1,6)-1, xx, yy
     ic=ic+1
     write(106,*) xx, yy
  end do
  icrv(2,6)=ic

  ! 線分7 (1-->3)
  lngth=xlim2(2)-xlim2(1)
  n=ceiling(nunit*lngth)
  icrv(1,7)=ic+1
  do i=1,n
     xx=xlim2(2)-dble(i)/dble(n)*lngth
     yy=ylim2(1)
     write(1,*) i+icrv(1,7)-1, xx, yy
     ic=ic+1
     write(107,*) xx, yy
  end do
  icrv(2,7)=ic

  ! 線分8 (1-->3)
  lngth=ylim2(2)-ylim2(1)
  n=ceiling(nunit*lngth)
  icrv(1,8)=ic+1
  do i=1,n
     xx=xlim2(1)
     yy=ylim2(1)+dble(i)/dble(n)*lngth
     write(1,*) i+icrv(1,8)-1, xx, yy
     write(108,*) xx, yy
     ic=ic+1
  end do
  icrv(2,8)=ic

  ! 線分9 (1-->3)
  lngth=xlim(1)-xlim2(1)
  n=ceiling(nunit*lngth)
  icrv(1,9)=ic+1
  do i=1,n
     xx=xlim2(1)+dble(i)/dble(n)*lngth
     yy=ylim2(2)
     write(1,*) i+icrv(1,9)-1, xx, yy
     write(109,*) xx, yy
     ic=ic+1
  end do
  icrv(2,9)=ic
    

  write(1,*) ic

  ic=0
  ! 線分1
  do i=1,icrv(2,1)-1
     write(1,*) i, i+1, i, 1, 3
  end do
  write(1,*) icrv(2,1), i+1, i, 2, 3
  ! 線分2->4
  do i=icrv(1,2),icrv(2,4)-1
     write(1,*) i, i+1, i, 2, 3
  end do
  !write(1,*) icrv(2,4), icrv(1,1), icrv(2,4), 2, 3 !bug 
  write(1,*) icrv(2,4), icrv(1,1), icrv(2,4), 1, 3 !fixed 2023.09.13
  
  ! 線分5->9
  do i=icrv(1,5),icrv(2,9)-1
     write(1,*) i, i, i+1, 1, 2
  end do
  write(1,*) icrv(2,9), icrv(2,4), icrv(1,5), 1, 2
  close(1)
    
end program gen_polygon2
