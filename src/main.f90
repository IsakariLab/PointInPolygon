program main
  use class_point_in_polygon
  implicit none

  type(pointinpolygon) :: pip

  character(len=256) :: file_poly = "./examples/resonator_demo/resonator_demo.el2"
  character(len=256) :: file_po = "./examples/points/points.dat"

  
  call pip%init(file_po,file_poly)
  call pip%plot("./poly.res")
  call pip%plot_normal_vectors("./norm_poly.res")
  call pip%run("./points_with_domainum.res")
  call pip%del

  call system("rm -f *.mod a.out")
  call system("mkdir -p outputs")
  call system("mv *res fort.* outputs")
  
end program main
