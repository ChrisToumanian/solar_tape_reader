# Read tape to file
output_name=$1
sudo dd if=/dev/st0 bs=4k of=tape_data/$output_name status=progress