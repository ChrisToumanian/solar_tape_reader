# Read tape to file
output_name=$1
echo "Reading TAPE0 to $output_name at current position"
sudo dd if=/dev/st0 bs=4k of=tape_data/$output_name