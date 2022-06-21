program check_polygon
  use class_mesh2d
  implicit none

  type(Mesh2D) :: poly

  call poly%init("./polygon2.dat")

  call poly%plot("mesh.res")
  call poly%plot_normal_vectors("nvec.res")
  
end program check_polygon
