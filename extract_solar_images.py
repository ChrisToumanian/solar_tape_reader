# ==========================================================================================
# Convert Solar Tape to header and images
# Maintainers: Christopher Toumanian, cct_580@usc.edu
# ==========================================================================================
import os

# ==========================================================================================
# Main & Arguments
# ==========================================================================================

def main(args):
    buffer = read_file_to_buffer(args.input_file)

    header = read_file_header(buffer)

    if (args.append_filename):
        append_filename()

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
        "month": 0,
        "day": 0,
        "year": 0,
        "hour": 0,
        "minute": 0,
        "second": 0,
        "tick": 0,
        "nlines": 0,
        "nrecs": 0,
        "nsamps": 0,
        "dtype": 0,
        "dsize": 0,
        "lsize": 0,
        "dtime": 0,
        "ostat": 0,
        "c1": 0,
        "c2": 0,
    }

    return header

# ==========================================================================================
# I/O
# ==========================================================================================
def read_file_to_buffer(filepath):
    print("read file to buffer")

def append_filename_with_header_date(header):
    print("append filename with header date")

# ==========================================================================================
# Entry
# ==========================================================================================
if __name__ == '__main__':
    args = parse_arguments()
    main(args)
