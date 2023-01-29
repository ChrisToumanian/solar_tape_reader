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
        append_filename_with_header_timestamp()

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

    header = {
        "month": buffer[4*0],
        "day": buffer[4*1],
        "year": buffer[4*2],
        "hour": buffer[4*3],
        "minute": buffer[4*4],
        "second": buffer[4*5],
        "tick": buffer[4*6],
        "nlines": buffer[4*7],
        "nrecs": buffer[4*8],
        "nsamps": buffer[4*9],
        "dtype": buffer[4*19],
        "dsize": buffer[4*21],
        "lsize": buffer[4*22],
        "dtime": buffer[4*24],
        "ostat": buffer[4*25],
        "c1": buffer[4*64],
        "c2": buffer[4*66],
        "timestamp": ""
    }

    if header["year"] < 10:
        header["year"] += 2000
    elif header["year"] < 1900:
        header["year"] += 1900

    if header["nrecs"] <= 0:
        header["nrecs"] = (header["lsize"] * header["size"] * header["nlines"] + 8192) / 8192 + 1

    print(header)

    return header

# ==========================================================================================
# I/O
# ==========================================================================================
def read_file_to_buffer(filepath):
    with open(filepath, mode='rb') as f:
        buffer = f.read()
    return buffer

def append_filename_with_header_timestamp(header):
    print("append filename with header timestamp")

def export_header(header):


# ==========================================================================================
# Entry
# ==========================================================================================
if __name__ == '__main__':
    args = parse_arguments()
    main(args)
