#g++ -c src/craspicam.cc -I/usr/local/include -o build/craspicam.o
#ar  rcs build/aux_craspicam.a build/craspicam.o
#ar -M <<EOM
#CREATE build/craspicam.a
#ADDLIB build/aux_craspicam.a
#ADDLIB libraspicam.a
#ADDLIB
#SAVE
#END
#EOM
g++ -c src/craspicam.cc -I/usr/local/include -o build/craspicam.o -L. -l:libraspicam.so.0.1.6
ar rcs build/craspicam.a build/craspicam.o
#g++ -shared src/craspicam.cc -I/usr/local/include -o build/craspicam.so -L. -l:libraspicam.so.0.1.6
#g++ -nostartfiles src/craspicam.cc -o build/craspicam.o -Wl,--no-as-needed -lraspicam
