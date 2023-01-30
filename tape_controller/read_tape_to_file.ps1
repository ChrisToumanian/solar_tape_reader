# Read tape to file
# This requires dd.exe

$output_name=$args[0]
write-host "Reading TAPE0 to $output_name at current position"
.\dd\dd.exe if=\\.\TAPE0 bs=4k of=tape_data\$output_name --progress
