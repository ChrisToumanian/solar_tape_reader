# Read tape to file
output_name=$1
sudo dd if=/dev/st0 bs=1k of=tape_data/$output_name