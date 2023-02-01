#!/bin/bash
sudo mt -f /dev/nst0 setblk 0 # set block size
sudo ./sunio -i -v -m -Ffits # tape mode
