# Check status of tape
sudo mt -f /dev/st0 status

# Set Block Size
sudo mt -f /dev/st0 setblk 28800

# Fast-forward
sudo mt -f /dev/st0 fsf 1

# Backwards
sudo mt -f /dev/st0 bsf 1

# Set variable block size for Exabyte tapes
sudo mt -f /dev/st0 setblk 0

# Read
sudo dd if=/dev/st0 bs=4k of=tape.dat status=progress

# If there are multiple files on the tape use the no rewind version
sudo dd if=/dev/st0 bs=4k of=tape.dat

# Rewind
sudo mt -f /dev/st0 rewind
