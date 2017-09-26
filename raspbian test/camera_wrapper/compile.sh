g++ -c src/craspicam.cc -I/usr/local/include -Wl, -Bstatic -l:libraspicam.a -Wl, -Bdynamic -L/opt/vc/lib -lmmal -lmmal_core -lmmal_util -Wl,--as-needed -o build/craspicam.o
ar  rcs build/craspicam.a build/craspicam.o
