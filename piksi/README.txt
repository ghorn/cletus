libswiftnav is a submodule, so type

$ git submodule init
$ git submodule update

Now build libswiftnav if you haven't already. From this directory (piksi) type:

$ cd libswiftnav
$ mkdir build
$ cd build
$ cmake ..
$ make

now from this directory (piksi) you can just type

$ make
