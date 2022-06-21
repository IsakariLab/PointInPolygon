# PointInPolygon
点群(Point)と多領域2次元境界要素メッシュ(Polygon)を渡すと各点がどの領域に属するかを返す。2次元Laplace方程式の二重層ポテンシャルの計算に基づく。点の数をM、境界要素の数をNとしたときにO(MN)の計算をしているので遅い。この計算は多重極法などで加速はできるが。

## Usage
gfortran -fopenmp src/class_point_in_polygon.f90 src/main.f90   
./a.out

