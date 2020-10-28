set -e

mkdir -p built
cd liburing
make clean --silent
./configure
CFLAGS="-g -fomit-frame-pointer -O0 -fPIC" make -C src --silent
cp src/liburing.so.2.0.0 ../built/dlluring.so
cp src/liburing.a ../built/liburing.a
cd ..
dune build
