# Read range of files from tape
$tape_name = $args[0]
$current_tape = $args[1]
$n_files = $args[2]

write-host "Reading $n_files files from tape $filename at $(Get-Date -Format G)"

# Loop through tapes

For ($i=$current_tape; $i -le $n_files; $i++) {
	$filename = "$tape_name.$(([string]$i).PadLeft(3,'0')).DAT"
	write-host "Copying tape to file $filename"
	.\read_tape_to_file $filename
	write-host "Finished writing $filename at $(Get-Date -Format G)"
}

write-host "Job finished."

