a very partial list of dependencies, missing almost everything:

    $ apt-get install protobuf-compiler protobuf-c-compiler libprotobuf-dev libprotobuf-c0-dev

Compiling

    $ make

Compiling haskell stuff

    $ make hs
    $ cd hs
    $ cabal configure
    $ cabal build


Getting rcusb working

add a "usb" group and add yourself to that group:
    $ sudo addgroup --system usb
    $ sudo usermod -a -G usb YOUR_USER_NAME

then make a udev rule which changes usb devices to group "usb"

    $ cat /etc/udev/rules.d/10-local.rules 
    # libusb device nodes
    SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", GROUP="usb"

and reload the rules

    $ sudo udevadm control --reload

