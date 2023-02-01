# Read range of files from tape
# Example: .\extract_tape.ps1 "CCD.PC4.ZHD.030619" 0 1
$tape_name = $args[0]
$current_tape = $args[1]
$n_files = $args[2]

write-host "Reading $n_files files from tape at $(Get-Date -Format G)"

# Loop through tapes

For ($i=$current_tape; $i -le $n_files; $i++) {
	$filename = "$tape_name.$(([string]$i).PadLeft(3,'0')).DAT"
	write-host "Copying tape to file $filename"
	.\read_tape_to_file $filename
	write-host "Finished writing $filename at $(Get-Date -Format G)"
}

write-host "Job finished."

