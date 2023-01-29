# ==========================================================================================
# Convert Solar Tape to header and images
# Maintainers: Christopher Toumanian, cct_580@usc.edu
# ==========================================================================================
import os
import argparse

# ==========================================================================================
# Main & Arguments
# ==========================================================================================

def main(args):
    buffer = read_file_to_buffer(args.input_file)

    header = read_file_header(buffer)

    if (args.append_filename):
        append_filename_with_header_timestamp(args.input_file, args.output_path, header, buffer)

def parse_arguments():
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input_file", help="Input DAT file", type=str, required=True)
    parser.add_argument("-o", "--output_path", help="Output path", type=str, default="out/", required=False)
    parser.add_argument("-a", "--append_filename", help="Append filename option", action="store_true")
    args = parser.parse_args()
    return args

# ==========================================================================================
# Header
# ==========================================================================================
def read_file_header(buffer):
    month = buffer[4*0]
    day = buffer[4*1]
    year = buffer[4*2]
    hour = buffer[4*3]
    minute = buffer[4*4]
    second = buffer[4*5]
    tick = buffer[4*6]
    nlines = buffer[4*7]
    nrecs = buffer[4*8]
    nsamps = buffer[4*9]
    dtype = buffer[4*19]
    dsize = buffer[4*21]
    lsize = buffer[4*22]
    dtime = buffer[4*24]
    ostat = buffer[4*25]
    c1 = buffer[4*64]
    c2 = buffer[4*66]
    timestamp = ""

    if year < 10:
        year += 2000
    elif year < 1900:
        year += 1900

    if nrecs <= 0:
        nrecs = (lsize * size * nlines + 8192) / 8192 + 1

    timestamp = "{:04d}-{:02d}-{:02d}_{:02d}{:02d}{:02d}{:02d}".format(year, month, day, hour, minute, second, tick)

    header = {
        "month": month,
        "day": day,
        "year": year,
        "hour": hour,
        "minute": minute,
        "second": second,
        "tick": tick,
        "nlines": nlines,
        "nrecs": nrecs,
        "nsamps": nsamps,
        "dtype": dtype,
        "dsize": dsize,
        "lsize": lsize,
        "dtime": dtime,
        "ostat": ostat,
        "c1": c1,
        "c2": c2,
        "timestamp": timestamp
    }

    print(header)

    return header

# ==========================================================================================
# I/O
# ==========================================================================================
def read_file_to_buffer(filepath):
    with open(filepath, mode='rb') as f:
        buffer = f.read()
    return buffer

def append_filename_with_header_timestamp(input_file, output_path, header, buffer):
    filename = "{}.{}.DAT".format(os.path.basename(input_file).rsplit('.',1)[0], header["timestamp"])
    
    with open(f"{output_path}{filename}", "wb") as binary_file:
        binary_file.write(buffer)

def export_header(header):
    print("to-do")

# ==========================================================================================
# Entry
# ==========================================================================================
if __name__ == '__main__':
    args = parse_arguments()
    main(args)
