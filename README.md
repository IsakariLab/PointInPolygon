# PointInPolygon
点群(Point)と多領域2次元境界要素メッシュ(Polygon)を渡すと各点がどの領域に属するかを返す。2次元Laplace方程式の二重層ポテンシャルの計算に基づく。点の数をM、境界要素の数をNとしたときにO(MN)の計算をしているので遅い。この計算は多重極法などで加速はできるが。

## Usage
``src/main.f90`` の ``file_poly`` で境界要素メッシュの情報が記載されたファイルを指定``file_po``で点群の情報が載ったファイル(これらのファイルのフォーマットは後述する)を指定し、
~~~shell
gfortran -fopenmp src/class_point_in_polygon.f90 src/main.f90   
./a.out
~~~
とすると、``outputs/`` に

- ``points_with_domainnum.res``
- ``fort.1?? ``

が出てくる(これらのファイルの中身は後述する)。

## Input file
### 境界要素(Polygon)ファイルのフォーマット
~~~
np !点の節数
1 x1 y1 !1番の節点のx座標とy座標
.
.
np x y !np番の点のx座標とy座標
ne !境界要素(=ポリゴンを構成する線分)の数
1 nd1 nd2 ichdom1 ichdom2 !下を見よ
.
.
ne nd1 nd2 ichdom1 ichdom2 
~~~
ここに、
- nd1: i番境界要素の始点の節点番号
- nd2: i番境界要素の終点の節点番号
- ichdom1: i番境界要素の「裏」の領域番号
- ichdom2: i番境界要素の「表」の領域番号

を表す。i番境界要素上の法線はichdom1からichdom2を向く。例として、以下を参照されたい: 

``example/polygon1/polygon1.dat``
``example/polygon1/polygon1.dat``

### 点群ファイルのフォーマット
~~~
n !点の数
1 x y !1番の点の座標
.
.
n x y !n番の点の座標
~~~
例として、以下を参照されたい: 

``example/points/points.dat``

## Output file
### ``points_with_domainnum.res``
インプットの天狗ファイルの各行の末尾にその点が属する領域番号を返す
~~~
n !点の数
1 x y dom !1番の点のx座標, y座標, 1番の点が属する領域番号
.
.
n x y dom !n番の点のx座標, y座標, 1番の点が属する領域番号
~~~

### ``fort.1?? ``
領域番号??に属する点群の座標を並べたファイル

gnuplotで
~~~
p "fort.101","fort.102","fort.103","fort.104","fort.105","fort.106","fort.107","fort.108"
~~~
などとすると(領域ごとの点群)[./outputs/example1.png]のように結果を可視化してくれる。