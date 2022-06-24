module class_point_in_polygon
  implicit none
  private
  
  type :: point

     !> 点の数
     integer :: n(0:2)

     !> 点の座標
     real(8),allocatable :: x(:,:)

     !> 領域の数
     integer :: ndom
     
     !> 点の属する領域
     integer,allocatable :: dom(:)
     
  end type point

  type :: polygon

     !> 境界要素の数
     integer :: n

     !> 境界要素の節点
     real(8),allocatable :: x(:,:)

     !> 要素の裏表の領域番号 \n
     !> ichdom(1,i) = i番要素の「裏」の領域番号 \n
     !> ichdom(2,i) = i番要素の「表」の領域番号 \n
     !> 要素iの法線はichdom(1,i)->ichdom(2,i)に向く
     integer,allocatable :: ichdom(:,:)

     !> 要素コネクティビティ (2,ne) \n
     !> nd(j,i) = j番要素のローカルi番節点の全体節点番号
     integer,allocatable :: nd(:,:)

     !> 要素選点座標 (2,ne) \n
     !> c(i,j) = j番要素の選点のi成分
     real(8),allocatable :: c(:,:)

     !> 接線ベクトル (2,ne) \n
     !> at(i,j) = j番要素の接線ベクトルのi成分
     real(8),allocatable :: at(:,:)
     
     !> 外向き単位法線ベクトル (2,ne) \n
     !> an(i,j) = j番要素の法線ベクトルのi成分
     real(8),allocatable :: an(:,:)

     !> 要素長 (ne)
     !> le(i) = i番要素の長さ
     real(8),allocatable :: le(:)
     
  end type polygon
  
  type,public :: pointinpolygon

     type(point) :: po
     type(polygon) :: poly
     
   contains

     !> init
     procedure :: init

     !> del
     procedure :: del

     !> polygonを描画
     procedure :: plot

     !> polygonの法線ベクトルを描画
     procedure :: plot_normal_vectors
     
     !> main
     procedure :: run

  end type pointinpolygon
  
contains

  !> initialise
  !! \param [out] self
  !! \param [in] file_po (点の情報)
  !! \param [in] file_poly (ポリゴンの情報)
  subroutine init(self,file_po,file_poly)
    class(PointInPolygon),intent(out) :: self
    character(*),intent(in) :: file_po, file_poly

    integer :: i, itmp

    ! polygonを読み込み
    open(10,file=file_poly,status="old")
    read(10,*) self%poly%n
    if(.not.allocated(self%poly%x)) allocate(self%poly%x(2,self%poly%n))
    do i=1,self%poly%n
       read(10,*) itmp, self%poly%x(:,i)
    end do
    read(10,*) self%poly%n
    if(.not.allocated(self%poly%nd)) allocate(self%poly%nd(2,self%poly%n))
    if(.not.allocated(self%poly%ichdom)) allocate(self%poly%ichdom(2,self%poly%n))
    do i=1,self%poly%n
       read(10,*) itmp, self%poly%nd(:,i), self%poly%ichdom(:,i)
    end do
    close(10)

    ! 領域の数
    self%po%ndom=maxval(self%poly%ichdom)

    ! 選点を設定
    if(.not.allocated(self%poly%c)) allocate(self%poly%c(2,self%poly%n))
    do i=1,self%poly%n
       self%poly%c(:,i)=(self%poly%x(:,self%poly%nd(1,i))+self%poly%x(:,self%poly%nd(2,i)))*0.5d0
    end do

    ! 法線ベクトルを作成
    if(.not.allocated(self%poly%le)) allocate(self%poly%le(self%poly%n)) !要素長
    if(.not.allocated(self%poly%at)) allocate(self%poly%at(2,self%poly%n)) !接線ベクトル
    if(.not.allocated(self%poly%an)) allocate(self%poly%an(2,self%poly%n)) !法線ベクトル
    do i=1,self%poly%n
       self%poly%at(:,i)=self%poly%x(:,self%poly%nd(2,i))-self%poly%x(:,self%poly%nd(1,i))
       self%poly%le(i)=sqrt(dot_product(self%poly%at(:,i),self%poly%at(:,i)))
       self%poly%at(:,i)=self%poly%at(:,i)/self%poly%le(i)
       self%poly%an(1,i)=self%poly%at(2,i)
       self%poly%an(2,i)=-self%poly%at(1,i)
    end do

    ! 点を読み込み
    open(10,file=file_po,status="old")
    read(10,*) self%po%n(0), self%po%n(1), self%po%n(2)
    if(.not.allocated(self%po%x)) allocate(self%po%x(2,self%po%n(0)))
    do i=1,self%po%n(0)
       read(10,*) itmp, self%po%x(:,i)
    end do
    close(10)

    allocate(self%po%dom(self%po%n(0)))

  end subroutine init

  !> del
  subroutine del(self)
    class(PointInPolygon),intent(inout) :: self
    if(allocated(self%poly%x)) deallocate(self%poly%x)
    if(allocated(self%poly%nd)) deallocate(self%poly%nd)
    if(allocated(self%poly%ichdom)) deallocate(self%poly%ichdom)
    if(allocated(self%poly%c)) deallocate(self%poly%c)
    if(allocated(self%poly%at)) deallocate(self%poly%at)
    if(allocated(self%poly%an)) deallocate(self%poly%an)
    if(allocated(self%poly%le)) deallocate(self%poly%le)


    if(allocated(self%po%x)) deallocate(self%po%x)
    if(allocated(self%po%dom)) deallocate(self%po%dom)
    
  end subroutine del

  !> 境界要素メッシュを描画
  subroutine plot(self, output_file)
    class(PointInPolygon),intent(in) :: self
    character(*),intent(in) :: output_file

    integer :: i, j
    
    open(1,file=output_file)
    do i=1,self%poly%n
       do j=1,2
          write(1,*) self%poly%x(:,self%poly%nd(j,i))
       end do
       write(1,*)
       write(1,*)
    end do
    close(1)
    
  end subroutine plot


  !> 境界要素メッシュを描画
  subroutine plot_normal_vectors(self, output_file)
    class(PointInPolygon),intent(in) :: self
    character(*),intent(in) :: output_file

    integer :: i, j
    
    open(1,file=output_file)
    do i=1,self%poly%n
       write(1,'(4e16.8)') (self%poly%c(j,i), j=1,2), (2.d-2*self%poly%an(j,i), j=1,2)
    end do
    close(1)
    
  end subroutine plot_normal_vectors  
  
  !> main
  subroutine run(self, output_file)
    class(PointInPolygon),intent(inout) :: self
    character(*),intent(in) :: output_file

    integer :: ix, iy, idom, i
    real(8) :: x(2), y, th, sgn=0.d0

    self%po%dom(:)=1

    !$omp parallel do private(ix,idom,th,iy,sgn,y,x) collapse(2)
    do ix=1,self%po%n(0)
       do idom=1,self%po%ndom
          th=0.d0
          do iy=1,self%poly%n
             if(idom.eq.self%poly%ichdom(1,iy))then
                sgn=1.d0
             elseif(idom.eq.self%poly%ichdom(2,iy))then
                sgn=-1.d0
             else
                cycle
             end if
             y=   (self%po%x(1,ix)-self%poly%x(1,self%poly%nd(1,iy)))*self%poly%an(1,iy)&
                  +(self%po%x(2,ix)-self%poly%x(2,self%poly%nd(1,iy)))*self%poly%an(2,iy)
             x(1)=(self%po%x(1,ix)-self%poly%x(1,self%poly%nd(1,iy)))*self%poly%at(1,iy)&
                  +(self%po%x(2,ix)-self%poly%x(2,self%poly%nd(1,iy)))*self%poly%at(2,iy)
             x(2)=(self%po%x(1,ix)-self%poly%x(1,self%poly%nd(2,iy)))*self%poly%at(1,iy)&
                  +(self%po%x(2,ix)-self%poly%x(2,self%poly%nd(2,iy)))*self%poly%at(2,iy)
             th=th+sign(1d0,sgn*y)*(atan2(x(2),abs(y))-atan2(x(1),abs(y)))
          end do
          if(th>1.d0) then
             if(self%po%dom(ix).ne.1) stop "error"
             self%po%dom(ix)=idom
          end if
       end do
    end do
    !$omp end parallel do

    do idom=1,self%po%ndom
       do ix=1,self%po%n(0)
          if(self%po%dom(ix).eq.idom) then
             write(100+idom,*) self%po%x(:,ix)
          end if
       end do
    end do

    open(10,file=output_file)
    write(10,*) self%po%n(0), self%po%n(1), self%po%n(2)
    do i=1,self%po%n(0)
       write(10,*) i, self%po%x(:,i), self%po%dom(i)
    end do
    close(10)

    
  end subroutine run
    
end module class_point_in_polygon
