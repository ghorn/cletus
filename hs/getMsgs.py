#!/usr/bin/env python

import os

def list_modules(startpath,modules):
    print "  exposed-modules:     Messages"
    for root, dirs, files in os.walk(startpath):
        for f in files:
            if f[-3:] == ".hs":
                print ('                       , ' + root[4:].replace('/','.') + '.' + f[:-3])

if __name__=='__main__':
    list_modules("src/Messages",'')

#for path, dirs, files in os.walk(startpath):
#  print path
#  for f in files:
#    print f

#import os
