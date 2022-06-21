module class_mesh2d
  implicit none
  private

  !> 二次元境界要素メッシュ
  type,public :: Mesh2D

     !> 要素数
     integer :: ne

     !> 節点数
     integer :: np

     !> 節点座標 (2,np) \n
     !> x(i,j) = j番節点のi成分
     real(8),allocatable :: x(:,:)

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

     !> 要素長 (ne) \n
     !> le(i) = i番要素の長さ
     real(8),allocatable :: le(:)

     !> 境界の属する領域番号 (2,ne) \n
     !> ichdom(1,i) = i番境界要素の「裏」の領域番号\n
     !> ichdom(2,i) = i番境界要素の「表」の領域番号\n
     !> 要素iの法線ベクトルはichdom(1,i)->ichdom(2,i)を向く
     integer,allocatable :: ichdom(:,:)

   contains

     !> コンストラクタ
     procedure :: init

     !> デストラクタ
     procedure :: del
     
     !> meshを描画
     procedure :: plot

     !> 法線ベクトルを描画
     procedure :: plot_normal_vectors
     
  end type Mesh2D

contains

  !> meshを読み込み, メンバを初期化
  subroutine init(self, filename)
    class(Mesh2D),intent(inout) :: self
    character(*),intent(in) :: filename

    integer :: i, itmp

    ! meshの読み込み
    open(10,file=filename,status="old")
    read(10,*) self%np
    if(.not.allocated(self%x)) allocate(self%x(2,self%np))
    do i=1,self%np
       read(10,*) itmp, self%x(:,i)
    end do
    read(10,*) self%ne
    if(.not.allocated(self%nd)) allocate(self%nd(2,self%ne))
    if(.not.allocated(self%ichdom)) allocate(self%ichdom(2,self%ne))
    do i=1,self%ne
       read(10,*) itmp, self%nd(:,i), self%ichdom(:,i)
    end do
    close(10)

    ! 選点を設定
    if(.not.allocated(self%c)) allocate(self%c(2,self%ne))
    do i=1,self%ne
       self%c(:,i)=(self%x(:,self%nd(1,i))+self%x(:,self%nd(2,i)))*0.5d0
    end do

    ! 法線ベクトルを作成
    if(.not.allocated(self%le)) allocate(self%le(self%ne)) !要素長
    if(.not.allocated(self%at)) allocate(self%at(2,self%ne)) !接線ベクトル
    if(.not.allocated(self%an)) allocate(self%an(2,self%ne)) !法線ベクトル
    do i=1,self%ne
       self%at(:,i)=self%x(:,self%nd(2,i))-self%x(:,self%nd(1,i))
       self%le(i)=sqrt(dot_product(self%at(:,i),self%at(:,i)))
       self%at(:,i)=self%at(:,i)/self%le(i)
       self%an(1,i)=self%at(2,i)
       self%an(2,i)=-self%at(1,i)
    end do

  end subroutine init

  !> 境界要素メッシュを描画
  subroutine plot(self, output_file)
    class(Mesh2D),intent(in) :: self
    character(*),intent(in) :: output_file

    integer :: i, j
    
    open(1,file=output_file)
    do i=1,self%ne
       do j=1,2
          write(1,*) self%x(:,self%nd(j,i))
       end do
       write(1,*)
       write(1,*)
    end do
    close(1)
    
  end subroutine plot


  !> 境界要素メッシュを描画
  subroutine plot_normal_vectors(self, output_file)
    class(Mesh2D),intent(in) :: self
    character(*),intent(in) :: output_file

    integer :: i, j
    
    open(1,file=output_file)
    do i=1,self%ne
       write(1,'(4e16.8)') (self%c(j,i), j=1,2), (2.d-2*self%an(j,i), j=1,2)
    end do
    close(1)
    
  end subroutine plot_normal_vectors  

  !> デストラクタ
  subroutine del(self)
    class(Mesh2D),intent(inout) :: self

    if(allocated(self%x)) deallocate(self%x)
    if(allocated(self%nd)) deallocate(self%nd)
    if(allocated(self%ichdom)) deallocate(self%ichdom)
    if(allocated(self%c)) deallocate(self%c)
    if(allocated(self%at)) deallocate(self%at)
    if(allocated(self%an)) deallocate(self%an)
    if(allocated(self%le)) deallocate(self%le)
    
  end subroutine del
  
end module class_mesh2d
