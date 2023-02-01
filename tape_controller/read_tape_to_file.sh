# Read tape to file
output_name=$1
sudo mt -f /dev/st0 setblk 0
sudo dd if=/dev/nst0 bs=4k of=../tape_data/$output_name #status=progress
