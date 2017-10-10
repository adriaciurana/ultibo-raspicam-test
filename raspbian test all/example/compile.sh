#g++ camera.c -o camera -I../src -L../build -l:craspicam.a -L/opt/vc/lib -lmmal -lmmal_core -lmmal_util -Wl,--no-as-needed -lraspicam

#gcc camera.c -o camera -I../src -L../build -l:craspicam.a -L/opt/vc/lib -lmmal -lmmal_core -lmmal_util
gcc camera.c -o camera -I../src -L../build -l:craspicam.a -L../ -l:libraspicam.so.0.1.6 -L/opt/vc/lib -lmmal -lmmal_core -lmmal_util

