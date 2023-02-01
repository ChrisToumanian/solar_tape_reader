static char id_string[] = "sunio version of 1000-01-05 13:00 drs";
/* static char id_string[] = "sunio version of 1999-03-16 10:10 drs"; */
/* static char id_string[] = "sunio version of 941103 09:00 drs"; */

/*
 *	sunio - read in sun pictures, reformating I*2's & splitting into
 *	one file per image.  Write out sun pictures to tape, using
 *	a log file made by "sunio -i", or a reasonable facsimile.
 *	Adapted 11/87 by Dennis R. Smith, Univ. of Southern Calif. from:
 *
 * ************************************************************************
 * *    tcp - this routine runs in input (-i) and output (-o) modes. On	  *
 * *    input, it reads a tape's contents (out to the double tape-marks)  *
 * *    and writes that data to files in a the current directory, along	  *
 * *    with a file of data which will allow reconstruction of the tape	  *
 * *    in the output mode.					          *
 * *								          *
 * *    Copyright (C) 1983, 1984 Lyle McElhaney			          *
 * *    Permission to copy for non-commercial use granted under condition *
 * *    that this notice remains intact in the copy.			  *
 * *									  *
 * *    Address: 2489 W. Ridge Rd., Littleton, CO 80120			  *
 * *    ....denelcor!lmc						  *
 * ************************************************************************
 *
 *  	Modified 9/12/86 by Dennis R. Smith, Univ. of Southern Calif.
 * 	Fix multiple options.
 *	Add -v (verbose) switch.
 *
 *	Modified 2/12/87 by Dennis R. Smith, Univ. of Southern Calif.
 *	Added -e (error action) switch: On error, behave as if user
 *	 typed the character following the '-e'.  Options: Abort, Ignore,
 *	 Eof (treat as tape mark).
 *
 *	Modified 4/10/87 by Dennis R. Smith, Univ. of Southern Calif.
 *	Added -hHOSTNAME to allow for 'remote tcp': rmt code stolen
 *	from rtar.
 *
 *	Modified 5/15/88 to call newer rmt library
 *
 *	Modified 7/11/88 to un-vaxize header as well as data.
 *	  Header is treated as Integer*4, while data is treated as Integer*2
 *
 *	Modified 7/18/91 to support Exabyte drive on  IBM PC with NovaStor
 *	 controller.
 *
 *	Modified 9/9/91 to support stardent (stellar).
 *
 *	Modified 6/1/92 to add background mode (-B).
 *	 Includes logic for "*" records in control file (write a tape mark
 *	 in -o mode, or tapemark seen in -i multifile mode).
 *
 *	Modified 6/5/92 to delete input files and control file in
 *	 -B mode at end of control file or after two consecutive
 *	 "*" records.
 *
 *	Modified 7/28/92 to support Framcp format in to_tape.
 *
 *	Modified 10/19/92 to reorganize debugging output.  Made -D take an
 *	 argument with bits specifing kinds of debugging output to give.
 *	 Removed the -V flag.
 *
 *	Modified 2/4/92 to add -E (error file).  Reorganized help/usage
 *	 messages, reducing main "usage()" message and breaking out help
 *	 for some switches into categories accessable with new -I switches.
 *
 *	Modified 2/5/92 to try to allow PC record sizes up to 32768 bytes.
 *
 *	Modified 2/24/93 for IBM RS6000.
 *	Note: need to "chdev -l rmt0 -a block_size=000" on the RS6000 (if
 *	       rmt0 is the desired tape drive).
 *
 *	Modified 10/20/94 to add FITS format output
 *	No longer support -Fname for specifying name of FRAMCP file
 *
 *	Modified 11/03/94 to support ULTRIX and SGI systems
 *	Moved all use of compiler-supplied system definitions to beginning
 *	of "defines" page and made internal system-specific #ifdef's reference
 *	macros beginning "SY_".
 *
 *	Modified 09/19/96 to make File Format 1 the default.
 *
 *	Modified 03/16/99 for Y2K.  Added file formats 5 & 6.  Changed spec 
 *	for year in FRAMCP format to allow 4 digit year.  Use new FITS 4-digit 
 *	year format when year > 2000.
 */


/********************************
 *				*
 *	Includes, Defines	*
 *				*
 ********************************/

/**** translate pre-defined system definitions to internal flags ****/

/* Need to set up SY_ULTRIX & SY_SGI automatically */

#ifdef __hpux
#define SY_HPUX
#define SysV
#endif /* __hpux */

#ifdef vms
#define SY_VMS
#endif /* vms */

#ifdef MSDOS
#define SY_MSDOS
#endif /* MSDOS */

#ifdef uts
#define SY_UTS			/* ahmdal */
#define SysV
#endif /* uts */

#ifdef alliant
#define SY_ALLIANT
#endif /* alliant */

#ifdef cray
#define SY_CRAY
#endif /* cray */

#ifdef _AIX
#define SY_AIX
#endif /* _AIX */


/**** End of use of compiler-/preprocessor- supplied system definitions ****/

#define MEMALLOC

#ifdef SY_VMS
#define NOMTOP
#define noshowstatus
#define NO_REMOTE
#define unlink delete
#define link rename		/* careful with this! works in the case here */
#define index strchr
#define rindex strrchr
#define bcopy(from,to,size) memcpy(to,from,size)
#endif /* SY_VMS */

#ifdef SY_MSDOS
#define NovaStor
#define NOMTOP
#define noshowstatus
#define link rename		/* careful with this! works in the case here */
#define index strchr
#define rindex strrchr
#define bcopy(from,to,size) memcpy(to,from,size)
/* #define HT */
#endif /* SY_MSDOS */

#ifdef SysV
#define index strchr
#define rindex strrchr
#define bcopy(from,to,size) memcpy(to,from,size)
#endif /* SysV */

#ifdef SY_ALLIANT
#ifndef i860
#define LCVEC			/* libcvec exists */
#endif /* i860 */
#endif /* SY_ALLIANT */

#include <stdio.h>
#include <time.h>
#ifndef SY_VMS
#include <sys/types.h>
#else /* SY_VMS */
#include <types.h>
#include <file.h>
#endif /* SY_VMS */
#ifndef NOMTOP
#include <sys/ioctl.h>
#include <sys/file.h>
#endif /* NOMTOP */
#ifndef O_RDONLY
#include <fcntl.h>		/* for UNICOS */
#endif /* O_RDONLY */
#include <errno.h>
#include <signal.h>

#ifndef SY_MSDOS
#ifndef SY_VMS
#ifndef cray
#ifndef SY_UTS
#ifndef SY_AIX
#include <sys/mtio.h>
#else /* SY_AIX */
#include "mtio.h"		/* local one supplied for AIX */
#endif /* SY_AIX */
#else /* SY_UTS */
#include "mtio.h"		/* local one supplied for Amdahl */
#endif /* SY_UTS */
#else /* cray */
#include "mtio.h"		/* local one supplied for Cray */
#endif /* cray */
#include "rmt.h"
#endif /* SY_VMS */

#define IOCOUNT long int

#else /* SY_MSDOS */

#define IOCOUNT unsigned int

#define rmtopen		pc_open
#define rmtread		pc_read
#define rmtwrite	pc_write
#define rmtclose	pc_close
#define rmtioctl	pc_ioctl

#endif /* SY_MSDOS */

#ifndef SY_MSDOS
#define DIRSEP '/'
#else /* SY_MSDOS */
#define DIRSEP '\\'
#endif /* SY_MSDOS */

#ifndef L_SET			/* for UNICOS */
#define L_SET 0
#endif /* L_SET */

#define HDRVAXFMT 1		/* header is vax format on disk */
#ifndef NovaStor
#define MAXBUF 32768L            /* maximum tape physical record size */
#else /* NovaStor */
/* #define MAXBUF (2048*8) */
#define MAXBUF 32768L            /* maximum tape physical record size */
/* tspace modes */
#define PC_SR 0			/* space records */
#define PC_SF 1			/* space files (tape marks) */
#endif /* NovaStor */
/* novastore device indicator */
#define PC_DEV 1024
#define MAXFITS 80*36+1

#define HDRSIZE 512
#define MAXRETRIES 9
#define MAXFRAMES 120		/* maximum number of -f options */
#define FMTSIZ 1024       /* limit on size of filenames created + 1 */
/* #define BLKFACTOR 14 */
#ifndef YES
#define NO 0
#define YES 1
#endif /* YES */
#define NYU 2
#define CNUL '\000'

#ifndef SY_MSDOS
#define DBGMSG(msg) debug_msg(msg)
#else /* SY_MSDOS */
#define DBGMSG(msg) debug_msg((msg,dbuff))
#endif /* SY_MSDOS */

#define DEBUG_0(bit,msg) \
    if (debug & bit) {DBGMSG(sprintf(dbuff,msg));}
#define DEBUG_1(bit,msg,arg1) \
    if (debug & bit) {DBGMSG(sprintf(dbuff,msg,arg1));}
#define DEBUG_2(bit,msg,arg1,arg2) \
    if (debug & bit) {DBGMSG(sprintf(dbuff,msg,arg1,arg2));}
#define DEBUG_3(bit,msg,arg1,arg2,arg3) \
    if (debug & bit) {DBGMSG(sprintf(dbuff,msg,arg1,arg2,arg3));}
#define DEBUG_4(bit,msg,arg1,arg2,arg3,arg4) \
    if (debug & bit) {DBGMSG(sprintf(dbuff,msg,arg1,arg2,arg3,arg4));}


/* Exit Codes */
#define ERR_ARG		EINVAL	/* bad command line arguments */
#define ERR_IO		EIO	/* I/O error */
#define ERR_OPEN	128+1	/* Could not open a file */
#define ERR_EMPTY	128+2	/* no frames in file */
#define ERR_INT		128+3	/* interrupted (^C) */
#define ERR_HDR		128+4	/* bad header */
#define ERR_NOMATCH	128+5	/* no match to -T specified name */
#define ERR_INTERN	128+6	/* internal error */
#define ERR_MEM		128+7	/* error allocating memory */
#define ERR_INIT	128+8	/* error in pc_init */
#define ERR_WTM		128+9	/* error writing tapemark */
#define ERR_BSF		128+10	/* error backspacing over tapemark */
#define ERR_NSEX	128+11	/* NovaStor tape, but EXABYTE=9 not set */

/* parse_ctl return codes */
#define CTL_NAME 0		/* filename */
#define CTL_TM   1		/* TapeMark wanted ("*") */
#define CTL_EOT  2		/* End of tape wanted consecutive TM's */
#define CTL_EOF  3		/* End of file reached on CTL file */

/* hdrflag */
#define HDR_BAD_SIZE 1		/* some size is bad in the header */
#define HDR_BAD_DATE 2		/* the date/time is bad in the header */

/* kludge */
#define k_1 1
#define k_2 2
#define k_4 4
#define k_8 8
#define k_16 16
#define k_32 32
#define k_64 64
#define k_128 128
#define k_256 256
#define k_512 512
#define k_1024 1024
#define k_2048 2048

#define byte unsigned char
enum Flag {No,Yes};
#define flag enum Flag;

/********************************
 *				*
 *	   Help Messages	*
 *				*
 ********************************/

#define H_0  " -NN	where NN is a number means use PC SCSI bus unit NN\n"
#define H__1 "You must specify exactly one of -i, -o, or -s.\n"
#define H__2 "Major options are listed below.  For others, use one of the -I switches.\n"
#define H_B  " -B	background mode - switch between 2 control files until they say stop\n"
#define H_C " -CFFFF control file, where FFFF is the file to use instead of sunio0000000\n"
#define H_D1 " -D1	output general debug messages\n"
#define H_D2 " -D2	output info about headers\n"
#define H_D4 " -D4	call showstatus at various times\n"
#define H_D8 " -D8	output info about sleeping during -B\n"
#define H_D16 " -D16	output info about each read\n"
#define H_D32 " -D32	output info about each write\n"
#define H_D64 " -D64	output info about each read on PC\n"
#define H_D128 " -D128  tell when tape I/O is about to begin (PC)\n"
#define H_D256 " -D256	tell if tape write takes > 1 sec\n"
#define H_D512 " -D512	tell about each tape ctl op\n"
#define H_D1024 " -D1024	tell rem before each read from disk\n"
#define H_DF " -DFFFF	write debugging messages to file FFFF\n"
#define H_EF  " -EFFFF redirect stderr output to file FFFF\n"
#define H_Ffits " -Ffits	FITS format disk files\n"
#define H_Ffits_1 "	FITS files have the primary name as determined by the -N\n"
#define H_Ffits_2 "	switch with .fits appended and no associated header file.\n"
#define H_Ffcp " -Ffcp	FRAMCP mode\n"
#define H_Fframcp " -Fframcp FRAMCP mode\n"
#define H_Ffcp_1 "	FRAMCP mode expects the disk files to consist of a file header record\n"
#define H_Ffcp_2 "	followed by frames consisting of a frame header and data.  The file\n"
#define H_Ffcp_3 "	header contains a count of how many frames are in the file.\n"
#define H_H " -HDDDD	header directory, where DDDD is the directory containing the headers\n"
#define H_IB " -IB	list switches related to background (-B) mode\n"
#define H_IC  " -IC	list switches used for specifying file, directory, and host names\n"
#define H_ID " -ID	list debugging switches\n"
#define H_IE " -IE	list switches related to errors and error messages\n"
#define H_IF " -IF	list information about FITS and FRAMCP modes\n"
#define H_IN " -IN	list switches to set the format of the disk filenames used\n"
#define H_Ik " -Ik	list `kludge' switches (various esoteric options)\n"
#define H_Ir " -Ir	list switches related to record sizes and padding\n"
#define H_It " -It	list switches dealing with what tape device to use\n"
#define H_i  " -i	input from tape (one of the 3 modes)\n"
#define H_N " -N	(New) Format of file names: Pyymmdd:hhmmss (same as -N1)\n"
#define H_N0 " -N0	(Namfmt) Format of file names: 0= Pyy-mm-dd:hh:mm:ss.ttt (same as -O)\n"
#define H_N1 " -N1	(Namfmt) Format of file names: 1= Pyymmdd:hhmmss (same as -N)\n"
#define H_N2 " -N2	(Namfmt) Format of file names: 2= Pyymmdd.hhmmss\n"
#define H_N3 " -N3	(Namfmt) Format of file names: 3= Pyymmdd.Thhmmss\n"
#define H_N4 " -N4	(Namfmt) Format of file names: 4= yymmdd\\Phhmmss\n"
#define H_N5 " -N5	(Namfmt) Format of file names: 5= Pyyyymmdd.hhmmss\n"
#define H_N6 " -N6	(Namfmt) Format of file names: 6= yyyymmdd\\Phhmmss\n"
#define H_O  " -O	(old) Format of file names= Pyy-mm-dd:hh:mm:ss.ttt (Same as -N0)\n"
#define H_P  " -PX	prefix, where x is the prefix character to use on input instead of D\n"
#define H_S  " -S	standard-in ** DON'T USE **\n"
#define H_TN  " -TNNNN Test for match on name of first frame & abort if does not match NNNN\n"
#define H_b  " -b	force byte swapping in data\n"
#define H_b0 " -b0	force no byte swapping in data\n"
#define H_c  " -c	conversational (on tape errors)\n"
#define H_d  " -d	disk_mode: all reads for expected size (use if `tape' is disk file)\n"
#define H_eX  " -eX	error action, where X is a,e,i, or r\n"
#define H_f  " -fB,C	read only selected frames, where B = beginning frame, C = count\n"
#define H_h " -hHHHH	hostname, where HHHH is remote host with the tape drive\n"
#define H_k1 " -k1	kludge 1: ignore first record of tape file (extra header)\n"
#define H_k2 " -k2	kludge 2: read in only the headers\n"
#define H_k4 " -k4	kludge 4: reopen tape after tape mark on -m (exabyte)\n"
#define H_k8 " -k8	kludge 8: delete any frame not the expected size\n"
#define H_k16 " -k16	kludge 16: rename any frame not the expected size to *.bad?\n"
#define H_k32 " -k32	kludge 32: if a record is the wrong size, backspace and reread it\n"
#define H_k64 " -k64   kludge 64: try to reopen tape on error\n"
#define H_k128 " -k128  kludge 128: replace data files that already exist on disk\n"
#define H_k256 " -k256  kludge 256: report each record read from tape\n"
#define H_k512 " -k512  kludge 512: write&overwrite dummy record at start + TM kludge\n"
#define H_k1024 " -k1024 kludge 1024: Perform test write sequence of 512x512 images to tape\n"
#define H_k2048 " -k2048 kludge 2048: Perform test write sequence of 1024x1024 images to tape\n"
#define H_m  " -m	multifile mode (read past single tapemarks)\n"
#define H_n  " -nNN	number of the tape ** DON'T USE **\n"
#define H_o  " -o	output to tape (one of the 3 modes)\n"
#define H_p  " -pNN   pad disk file with NN extra lines (VICAR wants 12 for 1024**2 images)\n"
#define H_r  " -r	don't pad short records\n"
#define H_r0 " -r0	pad records to size found in header (default action)\n"
#define H_rN " -rNN	pad all records to size NN\n"
#define H_RN " -RNN	force tape record size to NN\n"
#define H_s  " -s	scan only (one of the 3 modes)\n"
#define H_sv " -sv	scan only (verbose) - same as -s except show key letter\n"
#define H_tD " -tDDDD	tape device name, where DDDD is the device name\n"
#define H_v  " -v	verbose mode\n"
#define H_wN " -wN	wait N seconds between ctl file checks in -B mode\n"
#define H_wNN " -wN,M	wait for the Mth second (mod N) in -B mode\n"
#define H_wNNN " -wN,M,D same as -wN,M, but don't start new frame after Dth second (mod N)\n"

/********************************
 *				*
 *	Function Prototypes	*
 *				*
 ********************************/

#ifdef NEED_IO_PROTO		/* 11/94 Don't think any systems need these */
int	open();
int	read();
int	write();
int	close();
int	ioctl();
long	lseek();
#endif /* NEED_IO_PROTO */

#ifdef SY_MSDOS
#include <io.h>
#endif /* SY_MSDOS */

void finish_disk_file();
void parse_hdr();
void make_fits_hdr();
int parse_ctl();
void delete_files();
void badhdr();
void trans_hdr();
void to_tape(), from_tape();
void xsleep();
void debug_msg();
byte *byte_swap();

#ifndef noshowstatus
void showstatus();
#else /* noshowstatus */
#define showstatus(a) a
#endif /* noshowstatus */

void delete_hdr_and_data();
void rename_diskfile();
void abort_file();
void abort_sunio();

#ifdef SIGNAL_RET_INT		/* I don't think we have any more  */
				/* systems like this */
#define HANDLER_RETURN int
#define HANDLER_VALUE 0
#else				/* the normal case these days */
#define HANDLER_RETURN void
#define HANDLER_VALUE
#endif

#ifdef SY_MSDOS			/* MSC 7 needs prototype with 1st argument */
HANDLER_RETURN sigint_handler(int,...);
HANDLER_RETURN (*defint_handler)();
#else /* SY_MSDOS */
HANDLER_RETURN sigint_handler();
HANDLER_RETURN (*defint_handler)();
#endif /* SY_MSDOS */

extern char *index(), *rindex();
#ifndef SY_MSDOS
extern int errno;
#endif /* SY_MSDOS */

long int i4pickup();
long int i4pickup_vax();
void usage();

#ifdef NovaStor
char *getenv();

long int pc_open();
long int pc_read();
long int pc_write();
long int pc_close();
long int pc_ioctl();
long int pc_init();
long int pc_space();

unsigned long scanbus();
int initdrv(), find_tape();
int tread(), twrite(), twtm(), tspace(), trew(), tstatus();
char *initif(), *memacc();
void memrel(), memfree();
#endif /* NovaStor */

void test();



/********************************
 *				*
 *	GLobal Variables	*
 *				*
 ********************************/

/* Long ints */

long int rem, recno, isize = 0, osize = 0, esize = 0, out_reclen = 0;
long int nlines, nrecs, nsamps, dtype, dsize, lsize, rsize, dtime, ostat;
long int month, day, year, hour, minute, second, tick;

#ifdef NovaStor
unsigned long int busscan;
#endif /* NovaStor */

/* Ints */

int tape_drive_no = 8;
int prefix_offset = 0;	/* where in the file name the prefix char is */
int nfile;
#ifndef SY_MSDOS
int dwo_mode=O_CREAT|O_EXCL|O_WRONLY;	/* disk write open mode */
int dro_mode=O_RDONLY;			/* disk read open mode */
#else /* SY_MSDOS */
int dwo_mode=O_CREAT|O_EXCL|O_WRONLY|O_BINARY|O_TRUNC;	/* disk write open mode */
int dro_mode=O_RDONLY|O_BINARY;			/* disk read open mode */
#endif /* SY_MSDOS */
int filen;
int multi_file=0;
int wait_period=60;		/* wait a minute */
int wait_sync=-1;		/* no sync */
int deadline=-1;
int reclen = 0;
int pad_records = -1;		/* -1: pad records to rsize, 0: don't pad, */
				/* 1: pad records to specified size */
int vicar_padding = 0;
int tapen=0;
int frame_begin[MAXFRAMES+1] = {0,-1};
int frame_count[MAXFRAMES] = {32767};
int frame_ptr = 0;
int inmode=NYU;         /* input mode - not yet set. User must choose. */
int wrmode=O_WRONLY;
int diskfd = -1;
int hdrfd = -1;
int tapefd = -1;
int nframes = 0;		/* number of freames seen */
int fframes = 0;		/* number of frames in this file */
int frames_in_file;		/* number of frames in framcp file */
int frame_no;			/* counter for frames_in_file */
int firstrec = 0;
int retries = 0;
int fitscount;

#ifdef SY_CRAY
#define  NAMFMT 1		/* new format: PYYMMDD:HHMMSS */
#else /* SY_CRAY */
#ifdef SY_UTS
#define  NAMFMT 1		/* new format: PYYMMDD:HHMMSS */
#else /* SY_UTS */
#ifdef SY_VMS
#define NAMFMT 2		/* format 2: PYYMMDD.HHMMSS */
#else /* SY_VMS */
#ifdef SY_MSDOS
#define NAMFMT 4		/* format 4 (DOS) YYMMDD\PHHMMSS*/
#else /* SY_MSDOS */
#define NAMFMT 1		/* new format: PYYMMDD:HHMMSS */
#endif /* SY_MSDOS */
#endif /* SY_VMS */
#endif /* SY_UTS */
#endif /* SY_CRAY */
int namfmt = NAMFMT;
int kludge=0;
int hrec = 0;
int hdrflag = 0;
#ifndef DEBUG
int debug=0;
#else /* DEBUG */
int debug=1;
#endif /* DEBUG */
/* debug bits */
#define D_1	1		/* 1 = general debug messages */
#define D_2	2		/* 2 = info about headers, each frame */
#define D_4	4		/* 3 = showstatus */
#define D_8	8		/* 8 = info about sleeping during -B */
#define D_16	16		/* 16 = info about each read */
#define D_32	32		/* 32 = info about each write */
#define D_64	64		/* 64 = info about each read on PC */
#define D_128	128		/* 128 = tell when tape I/O is about to begin (PC) */
#define D_256	256		/* 256 = tell if tape write takes > 1 sec */
#define D_512	512		/* 512 = info about each tape ctl op */
#define D_1024 1024		/* 1024 = tell rem before each read from disk */
#define D_any	(0xFFFF)	/* any flags at all */


/* Pointers */

#ifdef MEMALLOC
byte *buff;
byte *obuff;
byte *fitshdr;
#endif /* MEMALLOC */

byte *buffp;
FILE *ctldev = NULL;
FILE *DBGfil = stderr;
char *hdir = "";		/* header directory if not . */
char *cfile_end;

/* Shorts */

#define SZSHORT sizeof(short int)
union {
  short sh;
  byte by[SZSHORT];
} short_byte;
#define BIGENDIAN short_byte.by[SZSHORT-1]

/* Flags */

enum Flag swap_bytes;		/* byte swapping? */
enum Flag dprefix_set = No;	/* default prefix set on command line */
enum Flag ctl_tm_flag = Yes;	/* no records since last tape mark */
enum Flag disk_mode=No;		/* disk mode */
enum Flag use_stdin=No;		/* use stdin instead of tape */
enum Flag background=No;	/* background mode: toggle between CTL files */enum Flag verbose=No;
enum Flag scan_only=No;		/* scan tape but do not store data to disk */
enum Flag scan_flag;		/* 0 = show key letter, 1 = do not */
enum Flag framcp=No;		/* disk file(s) in framcp format */
enum Flag fits=No;		/* disk files in FITS format */
enum Flag convmode=No;		/* conversational mode */

/* following flag changes dynamicly */
enum Flag skip = No;		/* skip this frame */

/* Chars and bytes */

#ifndef MEMALLOC
byte buff[MAXBUF];
byte obuff[MAXBUF];
byte fitshdr[MAXFITS];
#endif /* MEMALLOC */
byte hbuff[HDRSIZE];

char program_name[] = "sunio";
#ifndef NovaStor
char def_tape_name[]="/dev/nrmt%d";
#else /* NovaStor */
char def_tape_name[]="Unit %d";
#endif /* NovaStor */
char def_rmt_tape_name[]="%s:/dev/nrmt%d";
char def_disk_mode_tape_name[]="nrmt%d";
char tape_name[100];
char host[25] = "";
char *spec_tape_name = NULL;	/* specified tape name (-ttape_name) */
char *test_name = NULL;
char nfh[]="sunio";
/* format: format of filenames. args: [fcn]fh, tapen, nfile */
#ifndef SY_MSDOS
char format[]="%s%02d%05d";
#else /* SY_MSDOS */
char format[]="%s%02d%1d";
#endif /* SY_MSDOS */
char cfile[FMTSIZ] = "",
     hfile[FMTSIZ] = "H",
     ffile[FMTSIZ] = "D",
     mfile[FMTSIZ] = "";	/* framcp file */
char hprefix = 'H',
     dprefix = 'D';
char hdrprefix = 0;
char action= '\0';		/* no forced action */
char ttybuf[20];
char dbuff[120];		/* debug buffer */

/* structures */

#ifndef NOMTOP
struct mtop mtop;
#endif /* NOMTOP */


/********************************
 *				*
 *	Main Program		*
 *				*
 ********************************/

main(argc, argv)
    int argc;
    char **argv;
{
    long int tmp_long;
    int optc = argc;
    char *ap;
    char **optv = argv;

    short_byte.sh = 1;		/* if bigendian, .by[SZSHORT-1] is 1 */
    /*SUPPRESS 112*/ /* Retrieving <unsigned char> from variable short_byte. */
    if (short_byte.by[SZSHORT-1])
        swap_bytes = Yes;	/* swap bytes */
    else
        swap_bytes = No;	/* don't swap bytes */

#ifdef NovaStor
    {
	int i,j;
	char *p;

	if ((p =getenv("DEBUG")) != NULL)
	    debug = atoi(p);
	p = getenv("EXABYTE");
	if (*p != '9') {
	    fprintf(stderr,
		    "%s: You must 'set EXABYTE=9' before running this program\n",
		    program_name);
	    exit(ERR_NSEX);
	}
scan_bus:
	busscan = scanbus();
	j = busscan & 0x7fffffff;
	DEBUG_1(D_1,"@scan_bus: scanbus returned 0x%0lx",busscan);
	if (j) {
	    for (i = 0; i <8; i++,j>>=1) {
		if (j&1)
		    break;
	    }
	    tape_drive_no = i;	/* default tape drive is first scsi tape */
	}
    }
#endif /* NovaStor */

parse_opts:
    optv++;
    while (optc > 1) {
	ap = *optv;
	if (*ap == '-') {
	    ap++;
	    if (*ap != '\0') {
		switch (*ap++) {
		  case 'b':	/* -b = byteswap data */
		    if (*ap == '\0')
		      swap_bytes = Yes;
		    else
		      swap_bytes = (atoi(ap) == 0) ? No : Yes;
		    break;
		  case 'B':	/* -B = Background mode */
		    background = Yes;
		    break;
		  case 'c':	/* -c = 'Conversational' (on errors) */
		    convmode = Yes;
		    break;
		  case 'C':	/* -Cfile specifies name of the control file */
		    strcpy(cfile,ap);
		    break;
		  case 'd':	/* -d = 'Disk_Mode' */
		    disk_mode = Yes;
		    wrmode = O_WRONLY|O_CREAT;
		    break;
		  case 'D':	/* -D = Debug */
		    if (*ap == '\0') {
			debug = 1;
		    } else if (*ap >= '0' && *ap <= '9') {
			debug |= atoi(ap);
		    } else {
			DBGfil = fopen(ap,"w");
			if (DBGfil == NULL) {
			    fprintf (stderr, "%s: cannot open %s\n",
				     program_name, ap);
			    perror(program_name);
			    exit(ERR_OPEN);
			}
			setbuf(DBGfil,NULL);
		    }
		    break;
		  case 'e':	/* -eX = Automatically take action X on error */
		    /*        Actions: 	'a' = abort */
		    /*                 	'e' = treat as EOF */
		    /* 			'i' = ignore */
		    /*     		'r' = retry */
		    if (((action = *ap++) != 'r' && action != 'e' &&
			 action != 'i'))
			action = 'a';
		    convmode = Yes;
		    break;
		  case 'E':	/* -E = Error log redirect stderr */
		    if (*ap == '\0') {
			usage('E',ERR_ARG);
		    } else {
			FILE *ERRfil;
			ERRfil = freopen(ap,"w",stderr);
			if (ERRfil == NULL) {
			    fprintf (stderr, "%s: cannot open %s\n",
				     program_name, ap);
			    perror(program_name);
			    exit(ERR_OPEN);
			}
			setbuf(ERRfil,NULL);
		    }
		    break;
		  case 'f':	/* -fB,C select frames */
		    if (frame_ptr < MAXFRAMES) {
			if (sscanf(ap,"%d,%d",
				   &frame_begin[frame_ptr],
				   &frame_count[frame_ptr]) != 2)
			    usage(CNUL,ERR_ARG);
			frame_begin[++frame_ptr] = -1;
		    }
		    break;
		  case 'F':	/* -F = 'Format of disk files' */
		    {
			char c = toupper(*ap);

			/* Since both special formats begin with F, allow */
			/* the format name with or without the F */
			if (c == 'F') c = toupper(ap[1]); 

			if (c == 'I') {	/* FITS */
			    fits = Yes;
			    swap_bytes = Yes; /* must be swapped */
			} else if (c == 'R' || c == 'C') { /* FRAMCP or FCP */
			    framcp = Yes;
			    swap_bytes = No; /* must not be swapped */
			} else {
			    fprintf(stderr,
				    "%s: Invalid format specified with -F\n",
				    program_name);
			    exit(ERR_ARG);
			}

		    }
		    break;
#ifndef NO_REMOTE
		  case 'h':	/* -hHOSTNAME use tape on remote host */
		    strcpy(host,ap); /* copy host name */
		    break;
#endif /* NO_REMOTE */
		  case 'H':	/* -Hdirectory = get headers from directory */
		    hdir = ap;
		    break;
		  case 'i':	/* -i = 'Input' mode ('tape' to disk files */
		    if (inmode != NYU) usage(CNUL,ERR_ARG);
		    inmode = Yes;
		    break;
		  case 'I':
		    usage(*ap,0);
		  case 'k':	/* kludge */
		    kludge |= atoi(ap);
		    break;
		  case 'm':	/* -m = Multifile (read all files) */
		    if (*ap == '\0')
			multi_file = 99999;
		    else
			multi_file = atoi(ap);
		    break;
		  case 'n':	/* -nXX = 'Tape number': allows multiple tapes */
		    tapen = atoi (ap); /*        in one directory */
		    break;
		  case 'N':	/* new format file names */
		    if (*ap == '\0')
			namfmt = 1; /* "new" format */
		    else
			namfmt = atoi(ap);
		    break;
		  case 'o':	/* -o = 'Output' mode (disk file to 'tape' */
		    if (inmode != NYU) usage(CNUL,ERR_ARG);
		    inmode = No;
		    break;
		  case 'O':	/* old format file names */
		    namfmt = 0;
		    break;
		  case 'p':	/* -pNN VICAP Padding */
		    vicar_padding = atoi(ap);
		    break;
		  case 'P':	/* -Px prefix character for data file */
		    {
			dprefix = *ap++;
			if (((dprefix >= 'A') && (dprefix <  'H')) || /* not H */
			    ((dprefix >  'H') && (dprefix <= 'Z')) ||
			    ((dprefix >= 'a') && (dprefix <= 'z'))) {
			    dprefix_set++;
			    break;
			} else {
			    fprintf(stderr,"%s: Invalid prefix character\n",
				    program_name);
			    exit(ERR_ARG);
			}
		    }
		  case 'r':
		    if (*ap == '\0') {
			pad_records = 0; /* don't pad records */
		    } else {
			pad_records = 1; /* pad records to specified size */
			reclen = atoi(ap);
			if (reclen > MAXBUF) reclen = MAXBUF;
			if (reclen == 0) pad_records = -1; /* based on rsize */
		    }
		    break;
		  case 'R':
		    if ((out_reclen = atoi(ap)) == 0) {
			fprintf(stderr,"%s: Invalid record length\n",
				program_name);
			exit(ERR_ARG);
		    }
		    break;
		  case 's':
		    scan_only = Yes;
		    inmode = Yes;
		    switch (*ap) {
		      case '\0':
			scan_flag = Yes; /* do not show key letter */
			break;
		      case 'v':
			scan_flag = No; /* show key letter */
			break;
		      default:
			usage(CNUL,ERR_ARG); /* error, do not return */
		    }
		    break;
		  case 'S':
		    use_stdin = Yes;
		    break;
		  case 't':	/* -tdevice_name : specify device fully */
				/* overrides numeric tape specification */
		    spec_tape_name = ap;
		    break;
		  case 'T':	/* -Tname test for match of (initial) */
				/* filename string specified with first */
				/* frame read */
		    test_name = ap;
		    break;
		  case 'v':	/* -v = 'Verbose' mode: tell each file processed */
		    verbose = Yes;
		    break;
		  case 'w':	/* -w wait parameters for background mode */
		    if (sscanf(ap,"%d,%d,%d",&wait_period,
				   &wait_sync,&deadline) < 1)
			usage('B',ERR_ARG);
		    break;
		  case '2':	/* XX = use mag tape drive XX */
		  case '3':
		  case '4':
		  case '5':
		  case '6':
		  case '7':
		  case '8':
		  case '9':
		  case '0':
		  case '1':
			tape_drive_no = atoi((--ap));
		    break;
		  default:
		    usage(CNUL,ERR_ARG);
		}		/* switch */
	    }
	    else
		usage(CNUL,ERR_ARG);
	    optc--;
	    optv++;
	}
	else
	    usage(CNUL,ERR_ARG);
    }				/*  while optc > 1 */

/********************************
 *				*
 *	Check options, setup	*
 *				*
 ********************************/

    if (fits)
	swap_bytes = Yes;	/* FITS format requires it */
    if (framcp)
	swap_bytes = No;	/* FRAMCP format in vax format */
	
    if (verbose) {
      fprintf(stderr,"%s\n",id_string);
        if (!swap_bytes)
	    fprintf(stderr,"not ");
      fprintf(stderr,"byte swapping data\n");
    }
    DEBUG_1(D_any,"%s",id_string);
    if (debug) {
	char cmdline[256];

	cmdline[0] = '\000';
	for (optc = argc, optv = argv; optc > 0; optc --) {
	    strcat(cmdline,*optv++);
	    strcat(cmdline," ");
	}
	DEBUG_0(D_any,cmdline);
    }
    DEBUG_4(D_any,"Debug=%d (0x%x), kludge=%d (0x%x)",
	    debug,debug,kludge,kludge);
    if (inmode == NYU)
        usage(CNUL,ERR_ARG);
    if (framcp && scan_only) {
	fprintf(stderr,"%s: Scan mode and FRAMCP mode are incompatable\n",
		program_name);
	exit(ERR_ARG);
    }
    if (framcp && fits) {
	fprintf(stderr,"%s: FITS and FRAMCP modes are incompatable\n",
		program_name);
	exit(ERR_ARG);
    }
    if (fits && !inmode) {
	fprintf(stderr,"%s: FITS mode is only available in input (-i) mode\n",
		program_name);
	exit(ERR_ARG);
    }

    if (namfmt == 4) {
	prefix_offset = 7;
	scan_flag = 0;
    }

    if (spec_tape_name != NULL) {
	if (*host) {
	    sprintf(tape_name,"%s:%s",host,spec_tape_name);
	} else {
	    strcpy(tape_name,spec_tape_name);
	}
    } else {
	if (disk_mode) {
	    sprintf(tape_name,def_disk_mode_tape_name,tape_drive_no);
	} else if (*host) {
	    sprintf(tape_name,def_rmt_tape_name,host,tape_drive_no);
	} else {
	    sprintf(tape_name,def_tape_name,tape_drive_no);
	}
    }
    if (cfile[0] == '\0' && !scan_only)
	sprintf (cfile, format, nfh, tapen, 0);
    if (background) {
	if (inmode) {
	    fprintf(stderr,"%s: Background mode is only available in output (-o) mode\n",program_name);
	    exit(ERR_ARG);
	}
	strcat(cfile,".CT1");	/* 1st CTL file extention (other is CT2) */
	cfile_end = cfile + strlen(cfile) - 1; /* find end of name */
    }

#ifndef NovaStor
#ifdef MEMALLOC
    if ((buff = (byte *) malloc(MAXBUF)) == NULL) {
	fprintf(stderr,"%s: Could not get main buffer\n",program_name);
	exit(ERR_MEM);
    }
    if ((obuff = (byte *) malloc(MAXBUF)) == NULL) {
	fprintf(stderr,"%s: Could not get aux buffer\n",program_name);
	exit(ERR_MEM);
    }
    if (fits) {
	if ((fitshdr = (byte *) malloc(MAXFITS)) == NULL) {
	    fprintf(stderr,"%s: Could not get FITS header buffer\n",program_name);
	    exit(ERR_MEM);
	}
    }
#endif /* MEMALLOC */
#else /* NovaStor */
    /* buffers allocated in pc_init */
    if (pc_init() < 0) {
	/* error message already issued */
	exit(ERR_INIT);
    }

#endif /* NovaStor     */


    defint_handler = signal(SIGINT,sigint_handler);

    if (kludge & (k_1024|k_2048)) {
	test();
	exit(0);
    }

    if (inmode) {
	from_tape();
    } else {
	to_tape();
    }
    exit(0);
}

/************************************************
 *						*
 *	from_tape: -i,-s routines		*
 *						*
 ************************************************/
void
from_tape()
{
    /*
     *      input mode - read tape into files.
     */
    long int ret;
    long int nret;
    long int tmp_long;
    IOCOUNT rwcount, rcount;
    byte *p,*q;

    if (kludge & k_128)
	dwo_mode &= ~O_EXCL;	/* replace file if it exists */
    rcount = MAXBUF;
    if (cfile[0] != '\0') {
	if ((ctldev = fopen (cfile, "w+")) == NULL) {
	    fprintf (stderr, "%s: cannot open %s\n", program_name, cfile);
	    perror(program_name);
	    exit(ERR_OPEN);
	}
	setbuf(ctldev,NULL);
    }

    if (use_stdin) {
	if (verbose) fprintf(stderr,"%s: using stdin\n", program_name);
	tapefd = 0;
    } else {
	if (verbose)
	    if (*host)
		fprintf(stderr,"Opening %s on %s for read\n", tape_name, host);
	    else
		fprintf(stderr,"Opening %s for read\n", tape_name);
	tapefd = rmtopen(tape_name, O_RDONLY,0600);
	if (tapefd < 0) {
	    fprintf (stderr, "%s: cannot open %s\n", program_name, tape_name);
	    perror(program_name);
	    exit(ERR_OPEN);
	}
	if (debug & D_4) showstatus(tapefd);
    }
    filen = 0;
    frame_ptr = 0;
    for (;;) {			/* for each file on tape */
	int is_header;

	recno = 0;
	if (disk_mode) {
	    rcount = HDRSIZE;
	    is_header = 1;
	}
      reread:
	while ((nret = rmtread(tapefd, (char *) buff, rcount)) > 0) {
	    DEBUG_1(D_16,"@reread: read %ld bytes from tape",nret);
	    isize += nret;
	    if (kludge & k_1) {
rd_xtra_hdr:
		nret = rmtread(tapefd, (char *) buff, rcount);
		DEBUG_1(D_16,"@rd_xtra_hdr: read %ld bytes from tape",nret);
		isize = nret;
		kludge -= k_1;
	    }
	    if (isize == HDRSIZE && nframes == 1) {
		/* kludge for extra header at beginning of some tapes */
rd_xtra_hdr2:
		nret = rmtread(tapefd, (char *) buff, rcount);
		DEBUG_1(D_16,"@rd_xtra_hdr2: read %ld bytes from tape",nret);
		isize = nret;
	    }
	    if (kludge & k_256) {
		long int L1 = filen,
		L2 = recno,
		L3 = nframes-1,
		L4 = recno-firstrec;
		fprintf(stderr,"%ld %ld (%ld.%ld)\015",
			L1,L2,L3,L4);
	    }

	    /************************************
	     *					*
	     *		Header Processing	*
	     *					*
	     ************************************/

	    if (!disk_mode)
		is_header =  (nret == HDRSIZE);

	    if (is_header) {
		is_header = 0;
		/* this is header */
		if (nframes >= frame_begin[frame_ptr]+frame_count[frame_ptr]) { /* enough frames? */
		    if (frame_begin[++frame_ptr] < 0) {
			isize -= nret; /* yes, get actual size of frame */
			nret = 0;	/* fake an eof */
			break;
		    }
		}
		nframes++;
		fframes++;
#ifdef HDRVAXFMT
		bcopy(buff,hbuff,HDRSIZE); /* save last header */
#else /* HDRVAXFMT */
		trans_hdr(buff,hbuff,HDRSIZE); /* save last header */
#endif /* HDRVAXFMT */
		isize -= HDRSIZE;
		finish_disk_file(!framcp); /* finish disk file if any */
		firstrec = recno+1;
		parse_hdr(hbuff);
parsed_hdr:
		if (disk_mode) {
		    long int L1 = (hrec = recno + nrecs - 1),
		    L2 = (rcount = rsize);

		    DEBUG_2(D_1,"@parsed_hdr: hrec = %ld, rcount = %ld",
			    L1,L2);
		}
		esize = nlines*dsize*lsize;
		if (pad_records < 0)
		    reclen = rsize;

		if (!dprefix_set && (hdrprefix != 0))
		    dprefix = hdrprefix;

		make_filename();

 		if (*hdir != '\0') {
		    sprintf(hfile,"%s%c%s",hdir,DIRSEP,ffile);
		    hfile[strlen(hdir)+prefix_offset] = hprefix;
	        }
		else {
		    strcpy(hfile,ffile);
		    hfile[prefix_offset] = hprefix;
	        }
		if (fits)
		    strcat(ffile,".fits");


		if (test_name) { /* need to test for correct frame? */
		    char *f = ffile;
		    char *t = test_name;
		    int l = strlen(test_name);

		    if (*t == '?') { /* allow wildcarding of 1st char */
			f++; t++; l--;
		    }
		    if (strncmp(t,f,l)) {
			fprintf(stderr,
				"%s: first frame does not match string specified by -T%s: %s\n",
				program_name,test_name,ffile);
			exit(ERR_NOMATCH);
		    } else {
			test_name = NULL; /* testing done */
		    }
		}
		rwcount = nret;
		if (framcp) {
		    if (nframes == 1) {
			if (*mfile == 0) {
			    strcpy(mfile,ffile);
			    mfile[0] = 'f';
			}
			fprintf(stderr,"Tape ->> %s\n",mfile);
			if ((diskfd = open(mfile,O_CREAT|O_EXCL|O_WRONLY,0666)) < 0) {
			    fprintf (stderr,"%s: cannot open %s\n",
				     program_name, mfile);
			    perror(program_name);
			    exit(ERR_OPEN);
			}
framcp_wr_1:
			if ((ret = write(diskfd,hbuff,rwcount)) < 0) {
			    fprintf(stderr,"%s: cannot write disk header\n",
				    program_name);
			    perror(program_name);
			    exit(ERR_IO);
			}
			DEBUG_2(D_32,"@framcp_wr_1: wrote %ld bytes to disk, ret = %ld",
				nret,ret);
		    }
		    if ((ret = write(diskfd,hbuff,rwcount)) < 0) {
			fprintf(stderr,"%s: cannot write frame header\n",
				program_name);
			perror(program_name);
			exit(ERR_IO);
		    }
framcp_wr_n:
		    DEBUG_2(D_32,"@framcp_wr_n: wrote %ld bytes to disk, ret = %ld",nret,ret);
		}

		skip = scan_only || (nframes <= frame_begin[frame_ptr]);
		if (scan_only || (framcp && verbose) || (skip && verbose))
		    fprintf(stderr,"[%s]", ffile+scan_flag);
		else if (verbose) {
		    if (kludge & k_2)
			fprintf(stderr,"Tape -> %s",hfile);
		    else
			fprintf(stderr,"Tape -> %s",ffile);
		}
		if (ctldev != NULL)
		    fprintf(ctldev,"%s\n",ffile);
		/* write header file */
		if (!skip && !framcp) {
		    if (!fits) {
			if ((hdrfd =
			     open (hfile,dwo_mode,0666)) < 0) {
			    if (errno != EEXIST) {
				fprintf(stderr,
					"\n%s: cannot open %s\n",
					program_name, hfile);
				perror(program_name);
				exit(ERR_OPEN);
			    } else {
				if (verbose)
				    fprintf(stderr," [H exists]");
				else
				    fprintf(stderr,"[%s exists]\n",hfile);
			    }
			} else {
			  hf_wr_hdr:
			    rwcount = nret;
			    if ((ret = write(hdrfd,buff,rwcount)) <= 0) {
				long int L1 = errno;

				fprintf (stderr,
					 "%s: file write error #%ld in %s\n",
					 program_name, L1, hfile);
				perror(program_name);
				abort_sunio(ERR_IO);
				DEBUG_2(D_32,"@hf_wr_hdr: wrote %ld bytes to disk, ret = %ld",
					nret,ret);
			    }
			    close(hdrfd);
			    hdrfd = -1;
			}	/* if (hdrfd = open(...) < 0) */
		    }		/* if (!fits) */
		    /* open data file */
		    if (!(kludge & k_2)) {
			if ((diskfd =
			     open (ffile,dwo_mode,0666)) < 0) {
			    if (errno != EEXIST) {
				fprintf (stderr,
					 "\n%s: cannot open %s\n",
					 program_name, ffile);
				perror(program_name);
				exit(ERR_OPEN);
			    } else {
				if (verbose)
				    fprintf(stderr," [%c exists]",
					    ffile[prefix_offset]);
				else
				    fprintf(stderr,"[%s exists]\n",ffile);
			    }
			} else { /* successful open */
			    if (fits) {
				make_fits_hdr();
				if ((ret = write(diskfd, fitshdr, fitscount)) <= 0) {
				    fprintf (stderr, "%s: file write error #%ld in FITS header in %s\n",
					     program_name, (tmp_long=errno), ffile);
				    perror(program_name);
				    abort_sunio(ERR_IO);
				}
			    }
			}
		    }		/* if (!(kludge & k_2)) */
		}		/* if (!skip && !framcp) */
		if (verbose || scan_only)
		    fprintf(stderr,"\n");
		recno++;
		continue;
	    }			/* if (is_header) */


	    /************************************
	     *				     	*
	     *	   Data Record Procesing     	*
	     *				     	*
	     ************************************/

	    if ((recno == 0) && (esize == 0)) {	/* no header yet */
		fprintf(stderr,
			"%s: first record is not a header (size = %ld), skipping...\n",
			program_name,nret);
	    }
	    if ((nret != reclen) && (isize < esize) && !disk_mode) {
		long int L1,L2,L3,L4,L5,L6;
		fprintf(stderr,
			"%s: record %ld (%ld.%ld) is %ld bytes (@%ld/%ld)\n",
			program_name, (L1=recno), (L2=nframes-1),
			(L3=recno-firstrec),(L4=nret),
			(L5=isize-nret),(L6=esize));
	    } else if (retries != 0) {
		fprintf(stderr," OK\n");
	    }
	    if  (diskfd > 0) {
#ifndef SY_VMS
		if ((nret != reclen) && (isize < esize)) {
		    if (kludge & k_32) {
			if (retries++ >= MAXRETRIES) {
			    fprintf(stderr,
				    "maximum retries exceeded\n");
			} else {
			    isize -= nret;
			    if (debug & D_4) showstatus(tapefd);
			    fprintf(stderr,
				    "retrying... ");
#ifndef NovaStor
			    mtop.mt_count = 1;
			    mtop.mt_op = MTBSR;
			    /*SUPPRESS 68*/ /* Benign type mismatch in call */
			    if (rmtioctl(tapefd, MTIOCTOP, &mtop) >= 0) {
				if (debug & D_4) showstatus(tapefd);
				goto reread; /* bsr worked */
			    }
			    fprintf(stderr,
				    "error %ld in ioctl (BSR); ignoring prev error.\n",
				    (tmp_long=errno));
#else /* NovaStor */
			    if (pc_space(tapefd, PC_SR, -1) == 0) { /* bsr 1 */
				goto reread; /* bsr worked */
			    }
			    fprintf(stderr,
				    "error %ld in tspace (BSR); ignoring prev error.\n", (tmp_long=errno));
#endif /* NovaStor */
			    if (debug & D_4) showstatus(tapefd);
			    if (kludge & k_64) {
				int tmpfd;

				if ((tmpfd =rmtopen(tape_name, O_RDONLY,0600)) > 0) {
				    long int L1,L2;
				    fprintf(stderr,
					    "%s: Tape reopened as fd=%ld, old fd=%ld\n",
					    program_name, (L1=tmpfd),
					    (L2=tapefd));
				    tapefd = tmpfd;
				}
			    }
			}	/* if (retries++ >= MAXRETRIES) */
		    }		/* if (kludge & k_32) */
		    abort_file(); /* do the right thing */
		}		/* if ((n != reclen) && (isize < esize)) */
#endif /* SY_VMS */
		/* Byte Swapping */

		if (framcp) {
		    buffp = buff;
		} else {
		    buffp = byte_swap(buff,obuff,nret);
		}

		/* Padding */

		if (pad_records) {
		    if (nret != reclen) {
			if (nret < reclen) {
#ifdef LCVEC
			    vec_wset(buffp+(nret/2*2),32767,(reclen-n+1)/2);
#else /* LCVEC */
			    for (q = buffp+(nret/2*2);q < buffp+reclen;)
				*q++ = 0;
#endif /* LCVEC */
			}
			isize += reclen -nret;
		    }
		}

		/* Write to disk */

		if (diskfd > 0)	{ /* might have been closed due to error */
wr_disk:
		    rwcount = (pad_records ? reclen : nret);
		    if ((ret = write(diskfd, (buffp), rwcount)) <= 0) {
			fprintf (stderr, "%s: file write error #%ld in %s\n",
				 program_name, (tmp_long=errno), ffile);
			perror(program_name);
			abort_sunio(ERR_IO);
		    }
		    DEBUG_2(D_32,"@wr_disk: wrote %ld bytes to disk, ret = %ld",nret,ret);
		    osize += (pad_records ? reclen : nret);
		}
	    }			/* if (diskfd > 0)  */
	    recno ++;
	    retries = 0;

	    if (disk_mode) {
		if (isize >= esize) {
		    rcount = HDRSIZE;
		    is_header = 1;
		} else if (esize - isize < rcount)
		    rcount = esize - isize;
	    }
	}	/* while ((nret = rmtread(tapefd, (char *) buff, rcount)) > 0) */


	/********************************
	 *				*
	 *      Error Processing	*
	 *				*
	 ********************************/

	if (nret == 0) {
	  Eof:
	    if (debug & D_4) showstatus(tapefd);
	    if (multi_file && (ctldev != NULL))
		fprintf(ctldev,"* %ld frames\n",(tmp_long=fframes));
	    if (recno == 0) {
		if (verbose) {
		    if (multi_file) {
			long int L1,L2,L3;
			fprintf(stderr,
				"Number of frames: %ld [%ld in file %ld].\n",
				(L1=nframes),(L2=fframes),(L3=filen));
		    } else {
			long int L1;
			fprintf(stderr,"Number of frames: %ld\n",(L1=nframes));
		    }
		}
		if (filen)
		    break;
		else
		    exit(ERR_EMPTY);
	    } else {
		filen ++;
		finish_disk_file(!framcp);
		if (verbose) {
		    if (multi_file) {
			long int L1,L2,L3;
			fprintf(stderr,
				"Number of frames: %ld [%ld in file %ld]\n",
				(L1=nframes),(L2=fframes),(L3=filen-1));
		    } else {
			long int L1;
			fprintf(stderr,"Number of frames: %ld\n",(L1=nframes));
		    }
		}
		if (--multi_file<=0) {
		    break;
		} else {
		    fframes = 0;
		    if (kludge & k_4) { /* exabyte needs reopening to get */
			/* past the EOF */
			if (frame_begin[frame_ptr] < 0)
			    break; /* done, so just quit */
			if (use_stdin) {
			    if (verbose) fprintf(stderr,"Using stdin\n");
			    tapefd = 0;
			} else {
			    rmtclose(tapefd); /* exabyte close always fails */
			    if (verbose)
				if (*host)
				    fprintf(stderr,"Opening %s on %s for read\n",tape_name,host);
				else
				    fprintf(stderr,"Opening %s for read\n",tape_name);
			    if ((tapefd = rmtopen(tape_name, O_RDONLY,0600)) < 0) {
				fprintf (stderr, "%s: cannot open %s\n",
					 program_name, tape_name);
				perror(program_name);
				exit(ERR_OPEN);
			    }
			}
		    }
		}
	    }
	} else {
	    long int L1,L2,L3,L4;
	    fprintf(stderr,"%s: Tape read error %ld in record %ld (%ld.%ld)\n",
		    program_name, (L1=nret), (L2=recno),
		    (L3=nframes-1),(L4=recno-firstrec));
	    perror(program_name);
rd_tape_err:
	    DEBUG_4(D_2,"@rd_tape_err: rcount=%ld,esize=%ld,isize=%ld,osize=%ld",
		    (tmp_long=rcount),esize,isize,osize);

	    for (;;) {
		if (convmode) {
		    fprintf(stderr,"Abort, Retry, Eof, or Ignore? ");
		    *ttybuf = action;
		    if (action)
			fprintf(stderr,"%c\n",action);
		    if ((action) || fgets(ttybuf,sizeof(ttybuf),stdin) != NULL) {
			if (*ttybuf == 'R' || *ttybuf == 'r') {
#ifndef SY_VMS
			    if (retries++ >= MAXRETRIES) {
				fprintf(stderr,
					"maximum retries exceeded\n");
				abort_sunio(ERR_IO);
			    } else {
#ifndef NovaStor
				mtop.mt_count = 1;
				mtop.mt_op = MTBSR;
				/*SUPPRESS 68*/ /* Benign type mismatch in call */
				if (rmtioctl(tapefd, MTIOCTOP, &mtop) < 0) {
				    fprintf(stderr,
					    "error %ld in ioctl (BSR); ignoring prev error.\n", (tmp_long=errno));
				    recno ++;
				    retries = 0;
				}
#else /* NovaStor */
				if (pc_space(tapefd, PC_SR, -1) < 0) { /* bsr 1 */
				    fprintf(stderr,
					    "error %ld in tspace (BSR); ignoring prev error.\n", (tmp_long=errno));
				    recno ++;
				    retries = 0;
				}
#endif /* NovaStor */
			    }
			    goto reread;
#else /* SY_VMS*/
			    abort_sunio(ERR_IO); /* vms: treat as abort */
#endif /* SY_VMS */
			} else if (*ttybuf == 'I' || *ttybuf == 'i') {
			    recno ++;
			    goto reread;
			} else if (*ttybuf == 'E' || *ttybuf == 'e')
			    goto Eof;
			else if (*ttybuf == 'A' || *ttybuf == 'a')
			    abort_sunio(ERR_IO);
		    }
		} else
		    abort_sunio(ERR_IO);
	    }
	}
    }

    /********************************
     *				    *
     *        End of Tape	    *
     *				    *
     ********************************/

    if (framcp) {
	hbuff[96] = nframes;
	hbuff[97] = 0;
	hbuff[98] = 0;
	hbuff[99] = 0;
	if (lseek(diskfd,0,L_SET) < 0) {
	    fprintf(stderr,
		    "%s: cannot seek to beginning of file\n",
		    program_name);
	    perror(program_name);
	}
	rwcount = HDRSIZE;
	if (write(diskfd,hbuff,rwcount) < 0) {
	    fprintf(stderr,
		    "%s: cannot rewrite disk header\n", program_name);
	    perror(program_name);
	    exit(ERR_IO);
	}
wr_fr_hdr:
	DEBUG_2(D_32,"@wr_framcp_hdr: wrote %ld bytes to disk (file hdr), ret = %ld",nret,ret);
	close(diskfd);
	diskfd = -1;
    }				/* if (framcp) */

    rmtclose(tapefd);
    if (ctldev != NULL)
	fclose (ctldev);
}				/* end from_tape */

/************************************************
 *						*
 *	to_tape: -o routine			*
 *						*
 ************************************************/
void
to_tape()
{
    /*
     *      output mode - copy files back out to tape.
     */
    long int ret;
    long int nret;
    long int tmp_long;
    IOCOUNT rwcount, rcount;
    byte *p,*q;

    if (! background) {
	if ((ctldev = fopen (cfile, "r")) == NULL) {
	    fprintf (stderr, "%s: cannot open %s\n", program_name, cfile);
	    perror(program_name);
	    exit(ERR_OPEN);
	}
    } else {
	/* First check for any control files left from aborted run */
	if (
	    (ctldev = fopen(cfile, "r")) == NULL &&
	    ((*cfile_end ^= '2' ^ '1'),(ctldev = fopen(cfile, "r")) == NULL)
	    ) {
	    /* neither exists, just wait for first */
	    *cfile_end ^= '2' ^ '1'; /* switch ctl file names back */
	} else {
	    /* one of the files already existed */
	    if (verbose)
		fprintf(stderr,"%s: Found existing control file %s, starting with that.\n",
			program_name,cfile);
	    fclose(ctldev);	/* close it to reopen below */
	}

	if (verbose)
	    fprintf(stderr,"%s: Opening control file %s ...",
		    program_name,cfile);
	while ((ctldev = fopen(cfile, "r")) == NULL) {
	    xsleep(wait_period,wait_sync);
	}
	if (verbose)
	    fprintf(stderr," OK.\n");
    }

    recno = 0;
    for (;;) {
	switch (parse_ctl()) {
	  case CTL_TM:
	    /* close & reopen tape (write tape mark) */
	    rmtclose(tapefd);
	    tapefd = -1;	/* show tape no longer open */
	    break;		/* continue the for(;;) loop */
	  case CTL_EOT:
	    if (background) {
		if (!framcp)
		    delete_files(); /* delete the data and header files */
		unlink(cfile); /* delete the background control file */
	    }
	    fclose(ctldev); /* close the control file */
	    goto totape_eot;	/* exit the for(;;) loop */
	  case CTL_EOF:
	    if (background) {
		if (!framcp)
		    delete_files(); /* delete the data and header files */
		unlink(cfile);	/* delete this control file */
		fclose(ctldev);	/* close the file */
		*cfile_end ^= '2' ^ '1'; /* switch ctl file names */
		if (verbose)
		    fprintf(stderr,"%s: Opening control file %s ...",
			    program_name,cfile);
		while ((ctldev = fopen(cfile, "r")) == NULL) {
		    xsleep(wait_period,wait_sync);
		}
		if (verbose)
		    fprintf(stderr," OK.\n");
		continue;
	    } else {		/* not background: */
		fclose(ctldev);	/* close the file */
		goto totape_eot;	/* exit the for(;;) loop */
	    }
	  case CTL_NAME:	/* normal case: read file name */
	    if (tapefd < 0) {	/* if tape not open yet... */
		if (verbose)
		    if (*host)
			fprintf(stderr,"Opening %s on %s for write\n",
				tape_name,host);
		    else
			fprintf(stderr,"Opening %s for write\n",tape_name);
		if ((tapefd = rmtopen (tape_name,wrmode,0600)) < 0) {
		    fprintf(stderr,"%s: cannot open %s\n",
			    program_name,tape_name);
		    perror(program_name);
		    exit(ERR_OPEN);
		}
	    }

	    if (deadline >= 0) {
		time_t timev;
		int modtime;
		
		(void) time(&timev);
		modtime = timev % wait_period;
ck_dl: 		if(debug & D_1) {
		    long int L1,L2;
		    DEBUG_2(D_8,"@ck_dl: modtime=%ld, deadline=%ld",
			    (L1=modtime),(L2=deadline));
		    if (modtime > deadline) {
			long int L1;
			fprintf(DBGfil,", sleeping til %ld\n",(L1=wait_sync));
		    } else {
			fprintf(DBGfil,", continuing\n");
		    }
		}
		if (modtime > deadline)
		    xsleep(wait_period,wait_sync);
	    }
	    if (framcp) {
		strcpy(mfile,ffile);
		if (verbose) fprintf(stderr,"Tape <<- %s\n",mfile);
		/* open framcp file */
		if ((diskfd = open(mfile,dro_mode,0)) < 0) {
		    fprintf(stderr,"%s: cannot open %s\n", program_name,mfile);
		    perror(program_name);
		    exit(ERR_OPEN);
		}
		/* read in file header */
fc_rd_file_hdr:
		rwcount = HDRSIZE;
		if ((nret = (IOCOUNT) read(diskfd,buff,rwcount)) != HDRSIZE) {
		    DEBUG_1(D_2,"@fc_rd_file_hdr: read nret = %ld",nret);
		    fprintf(stderr,"%s: error reading file header %s\n",
			    program_name,mfile);
		    perror(program_name);
		    exit(ERR_IO);
		}
		DEBUG_1(D_16,"@fc_rd_file_hdr: read %ld bytes from file header",nret);
		parse_hdr(buff);

		if (buff[0])	/* header goes on tape in vax format  */
		    bcopy(buff,hbuff,HDRSIZE); /* already in vax format */
		else
		    trans_hdr(buff,hbuff,HDRSIZE); /* put it in vax format */
		frames_in_file = dtime; /* number frames/file */
	    } else
		frames_in_file = 1;
	    for (frame_no = 0; frame_no < frames_in_file; frame_no++) {
		if (!framcp) {
		    if (verbose) fprintf(stderr,"Tape <- %s\n",ffile);
		    if ((diskfd = open(hfile,dro_mode,0)) < 0) {
			fprintf(stderr,"%s: cannot open %s\n", program_name,hfile);
			perror(program_name);
			exit(ERR_OPEN);
		    }
		}
disk_rd_hdr:
		rwcount = HDRSIZE;
		if ((nret = (IOCOUNT) read(diskfd,buff,rwcount)) != HDRSIZE) {
		    DEBUG_1(D_2,"disk_rd_hdr: read nret = %ld",nret);
		    if (framcp)
			fprintf(stderr,"%s: error reading header from %s\n",
				program_name,mfile);
		    else
			fprintf(stderr,"%s: error reading header %s\n",
				program_name,hfile);
		    perror(program_name);
		    exit(ERR_IO);
		}
		DEBUG_1(D_16,"@disk_rd_hdr: read %ld bytes from hdr",nret);
		if (!framcp)
		    close(diskfd);

		parse_hdr(buff);

		if (buff[0])	/* header goes on tape in vax format  */
		    bcopy(buff,hbuff,HDRSIZE); /* already in vax format */
		else
		    trans_hdr(buff,hbuff,HDRSIZE); /* put it in vax format */

		hbuff[256] = dprefix; /* save prefix */
		hbuff[257] = dprefix ^ 0xFF; /* and verification */

		if (framcp & verbose) {
		    make_filename(); /* create something to display */
		    fprintf(stderr,"Tape <- [%s]\n",ffile);
		}

tape_wr_hdr:
		if ((ret = rmtwrite(tapefd, (char *) hbuff, nret)) <= 0) {
		    fprintf (stderr, "%s: tape write error #%ld in %s\n",
			     program_name, (tmp_long=errno), tape_name);
		    perror(program_name);
		    exit(ERR_IO);
		}
		DEBUG_2(D_32,"@tape_wr_hdr: wrote %ld bytes to tape, ret = %ld",nret,ret);
		if (!framcp) {
		    if ((diskfd = open (ffile,dro_mode,0)) < 0) {
			fprintf (stderr, "%s: cannot open %s\n", program_name, ffile);
			perror(program_name);
			exit(ERR_OPEN);
		    }
		}
		/* rem = rsize*(nrecs-1); */
		rem = nlines * lsize * dsize;
		rcount = rsize;
		for (; rem > 0;) {
		    if (rem < rcount) rcount = rem;
		    DEBUG_1(D_1024,"@dsk_rd: rem=%ld",rem);
		    rwcount = rcount;
		    if ((nret = (IOCOUNT) read(diskfd, buff, rwcount)) <= 0) {
			if (framcp)
			    fprintf (stderr, "%s: file read error #%ld in %s\n",
				     program_name, (tmp_long=errno), mfile);
			else
			    fprintf (stderr, "%s: file read error #%ld in %s\n",
				     program_name, (tmp_long=errno), ffile);
disk_rd:
			DEBUG_4(D_1,"@disk_rd: read rcount=%ld,rem=%ld,n=%ld,recno=%ld",
				(tmp_long=rcount),rem,nret,recno);
			perror(program_name);
			exit(ERR_IO);
		    }
		    DEBUG_1(D_16,"@disk_rd: read %ld bytes from data",nret);

		    buffp = byte_swap(buff,obuff,nret);
tape_wr:
		    if ((ret = rmtwrite(tapefd, (char *) buffp, nret)) <= 0) {
			fprintf (stderr, "%s: tape write error #%ld in %s\n",
				 program_name, (tmp_long=errno), tape_name);
			perror(program_name);
			exit(ERR_IO);
		    }
		    DEBUG_2(D_32,"@tape_wr: wrote %ld bytes to tape, ret = %ld",nret,ret);
		    rem -= rcount;
		    recno ++;
		}
		if (!framcp)
		    close(diskfd);
		filen ++;
		recno = 0;
	    }
	    if (framcp)
		close(diskfd);
	    break;		/*  */
	}			/* switch(parse_ctl()) */
    }				/* for (;;) */
totape_eot:
    if (tapefd >= 0 || (kludge & k_512 && tapefd == PC_DEV))
	rmtclose(tapefd);
}				/* end to_tape() */

/********************************
 *				*
 *	parse_ctl		*
 *				*
 ********************************/
int
parse_ctl()
{
    char *p,*q;

/*	Parses one line of a control file and returns a code based on
	the type of line read.  Codes are CTL_XXXX as defined earlier.
	Does not return on null or comment only lines, but just reads
	another record.

	Side effects: set ffile, hfile to filenames.
*/

    for(;;) {			/* loop for non-null record */
	if (fgets (ffile, FMTSIZ - 1, ctldev) == NULL) {
	    return(CTL_EOF);	/* end of file on control file */
	} else {
	    if ((p = index(ffile,' ')) != NULL ||
		(p = index(ffile,'\t')) != NULL ||
		(p = index(ffile,'\n')) != NULL )
		*p = '\0';	/* tie off filename */
	    if (*ffile == '\0' || *ffile == '#') continue;
	    if (*ffile == '*') {
		if (ctl_tm_flag) { /* empty file: end of tape */
		    return(CTL_EOT);
		} else {
		    ctl_tm_flag = 1;
		    return(CTL_TM);
		}
	    }
	    ctl_tm_flag = 0;	/* not a TM */
	    if ((p = rindex(ffile,DIRSEP)) == NULL)
		p = ffile;	/* first character if no directory */
	    else
		p++;		/* after the directory terminator */
	    if (!framcp && !dprefix_set)
		dprefix = *p;
	    if (*hdir != '\0') {
		sprintf(hfile,"%s%c%s",hdir,DIRSEP,ffile);
		q = hfile + (p-ffile) + strlen(hdir) + 1;
	    }
	    else {
		strcpy(hfile,ffile);
		q = hfile + (p-ffile);
	    }
	    *q = hprefix;
	    return(CTL_NAME);
	}
    }
}

make_filename() {

    switch (namfmt) {
      case 0:			/* original, long format */
	if (tapen) {
	    sprintf(ffile,
		    "%c%03d,%02d-%02d-%02d:%02d:%02d:%02d.%03d",
		    dprefix,tapen,(year%100),month,day,hour,minute,
		    second,tick);
	} else {
	    sprintf(ffile,
		    "%c%02d-%02d-%02d:%02d:%02d:%02d.%03d",
		    dprefix,(year%100),month,day,hour,minute,
		    second,tick);
	}
	break;
      case 1:			/* short format */
	sprintf(ffile,
		"%c%02d%02d%02d:%02d%02d%02d",
		dprefix,(year%100),month,day,hour,minute,second);
	break;
      case 2:			/* VMS format */
      default:
	sprintf(ffile,
		"%c%02d%02d%02d.%02d%02d%02d",
		dprefix,(year%100),month,day,hour,minute,second);
	break;
      case 3:			/* IBM format */
	sprintf(ffile,
		"%c%02d%02d%02d.T%02d%02d%02d",
		dprefix,(year%100),month,day,hour,minute,second);
	break;
      case 4:			/* MS Dos format */
	sprintf(ffile,
		"%02ld%02ld%02ld\\%c%02ld%02ld%02ld",
		(year%100),month,day,dprefix,hour,minute,second);
	break;
      case 5:			/* Y2k short format */
	sprintf(ffile,
		"%c%04d%02d%02d:%02d%02d%02d",
		dprefix,year,month,day,hour,minute,second);
	break;
      case 6:			/* MS Dos format */
	sprintf(ffile,
		"%04ld%02ld%02ld\\%c%02ld%02ld%02ld",
		year,month,day,dprefix,hour,minute,second);
	break;
    }				/* switch (namfmt) */
}

/********************************
 *				*
 *	delete_files		*
 *				*
 ********************************/
void
delete_files()
{
    /* delete data and header files from control file */
    if (fseek(ctldev,0,0)) {	/* rewind the control file */
	fprintf(stderr,"%s: Error rewinding control file %s\n",
		program_name,cfile);
	perror(program_name);
	exit(ERR_IO);
    }
    for (;;) {
	switch(parse_ctl()) {
	  case CTL_TM:
	    continue;		/* tape mark: ignore */
	  case CTL_EOT:
	    return;		/* end of tape: quit */
	  case CTL_EOF:
	    return;		/* end of ctl file: quit */
	  case CTL_NAME:
	    unlink(ffile);	/* delete the data file */
	    unlink(hfile);	/* delete the header file */
	    continue;
	}			/* switch */
    }				/* for (;;) */
}

/********************************
 *				*
 *	i4pickup & i4putdown	*
 *				*
 ********************************/

long int
i4pickup_vax(inp)
     unsigned char *inp;
{
    return((inp[3]<<24) + (inp[2]<<16) + (inp[1]<<8) + inp[0]);
}

void
i4putdown_vax(outp,value)
     unsigned char *outp;
     unsigned long int value;
{
    outp[3] = (value >> 24) & 0xff;
    outp[2] = (value >> 16) & 0xff;
    outp[1] = (value >> 8) & 0xff;
    outp[0] = (value) & 0xff;
    return;
}

void
i4putdown(outp,value)
     unsigned char *outp;
     unsigned long int value;
{
    outp[0] = (value >> 24) & 0xff;
    outp[1] = (value >> 16) & 0xff;
    outp[2] = (value >> 8) & 0xff;
    outp[3] = (value) & 0xff;
    return;
}

long int
i4pickup(inp)
     unsigned char *inp;
{
    return((inp[0]<<24) + (inp[1]<<16) + (inp[2]<<8) + inp[3]);
}

void trans_hdr(buf1,buf2,bufsiz)
     byte *buf1,*buf2;
     int bufsiz;
{
    int i;

#ifdef SY_ALLIANT
#pragma loop novector
#pragma loop noconcur
#endif /* SY_ALLIANT */
    for (i = 0; i < bufsiz; i += 4) {
	buf2[i+3] = buf1[i+0];
	buf2[i+2] = buf1[i+1];
	buf2[i+1] = buf1[i+2];
	buf2[i+0] = buf1[i+3];
    }
}

void parse_hdr(buffer)
     byte *buffer;
{
    long int (*pickup)();
    void (*putdown)();
    byte c1, c2;
    byte cff = 0xFF;

    hdrflag = 0;		/* assume the best */
    if (buffer[0]) {		/* if 1st byte is non-zero, hdr is vax fmt */
	pickup = i4pickup_vax;
	putdown = i4putdown_vax;
    } else {
	pickup = i4pickup;
	putdown = i4putdown;
    }
    month = (*pickup)(&buffer[4*0]);
    if (month < 1 || month > 12) badhdr("Invalid Month",month,HDR_BAD_DATE);
    day = (*pickup)(&buffer[4*1]);
    if (day < 1 || day > 31) badhdr("Invalid Day",day,HDR_BAD_DATE);
    year = (*pickup)(&buffer[4*2]);
    if (year < 10) {
      year += 2000;		/* 10 years grace after Y2K */
    } else if (year < 70) {
	badhdr("Invalid Year",year,HDR_BAD_DATE);
    }
    if (year < 1900) {
      year += 1900;		/* old pre-Y2K year */
    } else {
      if (year < 1900 || year > 2099) badhdr("Invalid Year",year,HDR_BAD_DATE);
    }
    hour = (*pickup)(&buffer[4*3]);
    if (hour < 0 || hour > 24) badhdr("Invalid Hour",hour,HDR_BAD_DATE);
    minute = (*pickup)(&buffer[4*4]);
    if (minute < 0 || minute > 59) badhdr("Invalid Minute",minute,HDR_BAD_DATE);
    second = (*pickup)(&buffer[4*5]);
    if (second < 0 || second > 59) badhdr("Invalid Second",second,HDR_BAD_DATE);
    tick = (*pickup)(&buffer[4*6]);
    if (tick < 0 || tick > 99) badhdr("Invalid Tick",tick,HDR_BAD_DATE);
    nlines = (*pickup)(&buffer[4*7]);
    nsamps = (*pickup)(&buffer[4*9]);
    nrecs = (*pickup)(&buffer[4*8]);
    if (nrecs <= 0)
	badhdr("Invalid nrecs [hdr(9)], assuming something reasonable",nrecs,0);
    dtype = (*pickup)(&buffer[4*19]);
    if (dtype < 0 || dtype > 1) badhdr("Invalid Dtype",dtype,0);
    if (fits && dtype) badhdr("Invalid Dtype for FITS format",dtype,0);
    dsize = (*pickup)(&buffer[4*21]);
    if (dsize <= 0 || dsize > 8) badhdr("Invalid dsize",dsize,HDR_BAD_SIZE);
    lsize = (*pickup)(&buffer[4*22]);
    if (lsize <= 0) badhdr("Invalid Tw/ln",lsize,HDR_BAD_SIZE);
    dtime = (*pickup)(&buffer[4*24]);
    ostat = (*pickup)(&buffer[4*25]);
    /*SUPPRESS 51*/ /* Losing information during assignment. */
    c1 = (*pickup)(&buffer[4*64]);
    c2 = (*pickup)(&buffer[4*64]) >> 8;
    if ((c1 ^ c2 == cff) && (c1 > '@') &&  (c1 <= 'z') && (c1 != hprefix))
	hdrprefix = c1;
    else
	hdrprefix = 0;

    if (hdrflag & HDR_BAD_DATE)
	month=day=year=hour=minute=second=tick=0; /* don't make the filename too screwy */
    if (hdrflag & HDR_BAD_SIZE)
	abort_sunio(ERR_HDR);
    if (nrecs <= 0) {
	nrecs = (lsize*dsize*nlines+8191)/8192 + 1;
	(*putdown)(&buffer[4*8],nrecs);
    }
    if (out_reclen != 0 && !inmode) {
	long int lines_per_rec = out_reclen / (lsize*dsize);

	if (lines_per_rec < 1)
	    lines_per_rec = 1;
	rsize = lines_per_rec * (lsize*dsize);
	nrecs = (nlines + lines_per_rec -1) / lines_per_rec + 1;
	(*putdown)(&buffer[4*8],nrecs);
    } else {
	rsize = (dsize*lsize) * ((nlines+nrecs-2)/(nrecs-1));
    }
    if (rsize > MAXBUF)
	badhdr("Invalid record size computed",rsize,HDR_BAD_SIZE);
parse_hdr_ret:
    DEBUG_1(D_2,"@parse_hdr_ret: dtime=0x%lx",dtime);
    DEBUG_3(D_2,"@parse_hdr_ret: nlines=%ld, nrecs=%ld, dtype=%ld",
	    nlines,nrecs,dtype);
    DEBUG_3(D_2,"@parse_hdr_ret: dsize=%ld, lsize=%ld, rsize=%ld",
	    dsize,lsize,rsize);
}

void
make_fits_hdr()
{
    int i;
    char *fmt1 = "%-8s= %20s / %-47s";
    char *fmt2 = "%-8s= %20d / %-47s";
    char date[10],time[10];
    char *fitsdata = (char *) fitshdr;

    if (year < 2000) {
      sprintf(date,"'%2.2d/%2.2d/%2.2d'", day, month, (year%100));
    } else {
      sprintf(date,"'%4.4d-%2.2d-%2.2d'", year, month, day );
    }
    sprintf(time,"'%2.2d:%2.2d:%2.2d'", hour,minute,second);

    sprintf(fitsdata     ,fmt1, "SIMPLE", "T", "Written by sunio");
    sprintf(fitsdata+80*1,fmt2, "BITPIX", dsize*8, "bits/pixel");
    sprintf(fitsdata+80*2,fmt2, "NAXIS", 2, "");
    sprintf(fitsdata+80*3,fmt2, "NAXIS1", nlines, "Lines");
    sprintf(fitsdata+80*4,fmt2, "NAXIS2", nsamps, "Samples/Line");
    if (year < 2000) {
      sprintf(fitsdata+80*5,fmt1, "DATE", date, "Observation date dd/mm/yy");
    } else {
      sprintf(fitsdata+80*5,fmt1, "DATE", date, "Observation date yyyy-mm-dd");
    }
    sprintf(fitsdata+80*6,fmt1, "TIME", time, "Observation time, PDT");
    sprintf(fitsdata+80*7,fmt2, "KTIME", dtime, 
	    "Observation time, 100th sec since...");
    sprintf(fitsdata+80*8,fmt2, "STATUS", ostat, "Observation status");
    sprintf(fitsdata+80*9,"%-80s", "END");
    for (i=80*10; i<80*36; i+=80) {
	sprintf(fitsdata+i,"%80s", "");
    }
    fitscount = 36*80;

}    

byte *
byte_swap(ibuf,obuf,nbytes)
     byte *ibuf, *obuf;
     long int nbytes;
{
    byte *bufp;
    int i;
    byte *p,*q;

    if (!swap_bytes || dsize == 1) /* no byte swapping needed? */
	return(ibuf);
    bufp = obuf;
    if (dsize == 2) {
#ifdef LCVEC
	vec_bmov_s(obuf+1,ibuf,nbytes/2,2,2);
	vec_bmov_s(obuf,ibuf+1,nbytes/2,2,2);
#else /* LCVEC */
	for (p = ibuf,q = obuf; p < ibuf+nbytes;) {
	    q[1] = *p++;
	    *q++ = *p++;
	    q++;
	}
#endif /* LCVEC */
    } else {			/* !(dsize == 2) */
	for (p = ibuf+dsize-1,q = obuf; q < obuf+nbytes;p+=dsize*2) {
	    for (i = 0; i < dsize;i++) {
		*q++ = *p--;
	    }
	}
    }				/* (dsize == 2) */
    return(bufp);
}


void
badhdr(message,value,eflag)
     char *message;
     long int value;
     int eflag;
{
    fprintf(stderr,"%s: %s -- %ld decimal = %lx hex\n",program_name,message,value,value);
    hdrflag |= eflag;
}

/********************************
 *				*
 *	finish_disk_file	*
 *				*
 ********************************/



void finish_disk_file(doclose)
     int doclose;
{
    long int psize;
    long int tmp_long;
    IOCOUNT rwcount;
    int i;
    unsigned int padding;

    DEBUG_4(D_2,"@finish_disk_file: diskfd=%ld,esize=%ld,isize=%ld,osize=%ld",
	    (tmp_long=diskfd),esize,isize,osize);
    if (isize  != esize) {
	fprintf(stderr,
		"%s: frame is not expected length (%ld instead of %ld)\n",
		program_name, isize,esize);
    }				/* if (isize  != esize) */
	if (diskfd > 0) {	/* if previous file open */
#ifdef BLKFACTOR
	    if (framcp) {
		psize = esize;
	    } else {
		psize = ((nlines+BLKFACTOR-1) / BLKFACTOR * BLKFACTOR) * dsize * lsize;
	    }
	    padding = psize - osize;
do_pad:
	    while (padding > 0) {
		DEBUG_2(D_2,"@do_pad: psize=%ld, padding=%ld",
			psize,(tmp_long=padding));
#ifdef LCVEC
		vec_bset(obuff,0,MAXBUF);
#else /* LCVEC */
		for (i = 0; i < MAXBUF; ) {
		    buff[i++] = 0;
		}
#endif /* LCVEC */
		rwcount = (padding > MAXBUF ? MAXBUF : padding)
		write(diskfd, obuff, rwcount);
		padding -= rwcount;
	    }
#else /* BLKFACTOR */
do_v_pad:
	    if (vicar_padding) {
		if (framcp)
		    psize = esize;
		else
		    psize = esize + vicar_padding*dsize*lsize;
		padding = psize - osize;
		while (padding > 0) {
		    DEBUG_2(D_2,"@do_v_pad: psize=%ld, padding=%ld",
			    psize,(tmp_long=padding));
#ifdef LCVEC
		    vec_bset(obuff,0,MAXBUF);
#else /* LCVEC */
		    for (i = 0; i < MAXBUF; ) {
			buff[i++] = 0;
		    }
#endif /* LCVEC */
		    rwcount = (padding > MAXBUF ? MAXBUF : padding);
		    write(diskfd, obuff, rwcount);
		    padding -= rwcount;
		}
	    }			/* if (vicar_padding) */
#endif /* BLKFACTOR */

	    if (isize != esize)
		abort_file();

	    if (doclose) {
		close(diskfd);
		diskfd = -1;
	    }
	}			/*  if (diskfd > 0) */
    esize = isize = osize = 0;
}

#ifndef noshowstatus
void showstatus(tapefd)
     int tapefd;
{
    struct mtget mtget;
#ifdef MTIOCVSR
    struct mtvsr mtvsr;
#endif /* MTIOCVSR */
    long int nret;

    mtget.mt_type = 0;
#ifndef SY_HPUX
    mtget.mt_dsreg = 0;
#endif /* SY_HPUX */
    mtget.mt_erreg = 0;
    mtget.mt_resid = 0;
#ifndef SY_ULTRIX
    mtget.mt_fileno = 0;
    mtget.mt_blkno = 0;
#endif /* SY_ULTRIX */

    /*SUPPRESS 68*/ /* Benign type mismatch in call */
    nret = rmtioctl(tapefd,MTIOCGET,&mtget);
    DEBUG_1(D_4,"###ioctl(tapefd,MTIOCGET,mtget) returned %ld\n",nret);
    if (nret >= 0) {
	DEBUG_1(D_4,"### mt_type=%x\n",mtget.mt_type);
#ifndef SY_HPUX
	DEBUG_1(D_4,"### mt_dsreg=%x\n",mtget.mt_dsreg);
#endif /* SY_HPUX */
	DEBUG_1(D_4,"### mt_erreg=%x\n",mtget.mt_erreg);
	DEBUG_1(D_4,"### mt_resid=%x\n",mtget.mt_resid);
#ifndef SY_ULTRIX
	DEBUG_1(D_4,"### mt_fileno=%x\n",mtget.mt_fileno);
	DEBUG_1(D_4,"### mt_blkno=%x\n",mtget.mt_blkno);
#endif /* SY_ULTRIX */
    }
#ifdef NEVER
#ifdef MTIOCVSR
    nret = rmtioctl(tapefd,MTIOCVSR,mtvsr);
    DEBUG_1(D_4,"### ioctl(tapefd,MTIOCVSR,mtvsr) returned %ld\n",nret);
    if (n >= 0) {
 	DEBUG_1(D_4,"### mt_status=%x\n",mtvsr.mt_status);
	DEBUG_1(D_4,"### mt_valid=%x\n",mtvsr.mt_valid);
    }
#endif /* MTIOCVSR */
#endif /* NEVER */
}
#endif /* noshowstatus */

HANDLER_RETURN sigint_handler(sig,code /* ,scp,addr */ )
/*ARGSUSED*/
     int sig, code;
/*
     struct sigcontext *scp; 
     char *addr; 
*/
{
    long int L1,L2,L3,L4;

    fprintf(stderr,"%s: Interrupted in file %ld,  record %ld (%ld.%ld)\n",
	    program_name,(L1=filen),(L2=recno),(L3=nframes-1),
	    (L4=recno-firstrec));
    kludge |= k_8;		/* don't leave partial file */
    abort_sunio(ERR_INT);
#ifdef SY_ALLIANT
    return HANDLER_VALUE;	/* this is to make Alliant FX/C happy */
#endif /* SY_ALLIANT */
}

void delete_hdr_and_data()
{
    fprintf(stderr,"%s: deleting data and header files for %s\n",
	    program_name, ffile);
    if (!fits) unlink(hfile);
    unlink(ffile);
    close(diskfd);
    diskfd = -1;
}

void rename_diskfile()
{
    char badname[FMTSIZ+5];
    long int tmp_long;
    int badnum = 1;

    for (badnum = 1; badnum < 10; badnum++) {
	sprintf(badname,"%s.bad%ld",ffile,(tmp_long=badnum));
	if (link(ffile,badname) == 0) {
	    unlink(ffile);
	    fprintf(stderr,"%s: %s renamed to %s\n",
		    program_name, ffile,badname);
/*** do not stop storing ***
	    close(diskfd);
	    diskfd = -1;
 *** do not stop storing ***/
	    break;
	}
    }
}

void abort_file()
{
    if (hdrfd > 0 && inmode) {
	fprintf(stderr,"%s: deleting header %s\n", program_name,hfile);
	unlink(hfile);
	close(hdrfd);
	hdrfd = -1;
    }
    if (diskfd > 0 && inmode) {
	if (kludge & k_16) {
	    rename_diskfile();
	} else if (kludge & k_8) {
	    delete_hdr_and_data();
	}
    }
}

void abort_sunio(code)
     int code;
{
    abort_file();
    exit(code);
}

void xsleep(period,sync)
     int period,sync;
{
    long int tmp_long;
    time_t timev;
    unsigned times;

    (void) time(&timev);	/* get current time */
    if (sync < 0)
	times = period;	/* no sync, just wait period */
    else {
	/*           (next period + sync)               - now   */
	times = ((timev-1+period)/period*period + sync) - timev;
    }
xsleep1:
    DEBUG_2(D_8,"@xsleep1: time is %-24.24s, sleeping %ld seconds",
	    ctime(&timev),(tmp_long=times));
    sleep(times);
}

void debug_msg(msg)
     char *msg;
{
    time_t timev;
    char *time_str;

    (void) time(&timev);
    time_str = ctime(&timev);
    fprintf(DBGfil,"### %-8.8s ### %s\n",time_str+11,msg);
}

void
usage(which,err)
     char which;
     int err;
{
#define F(msg) fputs(msg,stderr)

    switch (which) {
      case 'B':
      case 'w':
	F(H_B);
	F(H_wN);
	F(H_wNN);
	F(H_wNNN);
	break;
      case 'C':
      case 'h':
      case 'H':
	F(H_C);
	F(H_h);
	F(H_H);
	F(H_DF);
	F(H_EF);
	F(H_tD);
	break;
      case 'D':
	F(H_D1);
	F(H_D2);
	F(H_D4);
	F(H_D8);
	F(H_D16);
	F(H_D32);
	F(H_D64);
	F(H_D128);
	F(H_D256);
	F(H_D512);
	F(H_D1024);
	F(H_DF);
	break;
      case 'E':
      case 'e':
      case 'c':
	F(H_c);
	F(H_eX);
	F(H_EF);
	break;
      case 'F':
	F(H_Ffits);
	F(H_Ffits_1);
	F(H_Ffits_2);
	F(H_Fframcp);
	F(H_Ffcp);
	F(H_Ffcp_1);
	F(H_Ffcp_2);
	F(H_Ffcp_3);
	break;
      case 'k':
	F(H_k1);
	F(H_k2);
	F(H_k4);
	F(H_k8);
	F(H_k16);
	F(H_k32);
	F(H_k64);
	F(H_k128);
	F(H_k256);
	F(H_k512);
	break;
      case 'N':
	F(H_N);
	F(H_N0);
	F(H_N1);
	F(H_N2);
	F(H_N3);
	F(H_N4);
	F(H_N5);
	F(H_N6);
	F(H_O);
	fprintf(stderr,"   (Default name format = %d)\n",NAMFMT);
	break;
      case 'r':
      case 'R':
      case 'p':
	F(H_p);
	F(H_r);
	F(H_rN);
	F(H_r0);
	F(H_RN);
	break;
      case 't':
      case 'n':
      case 'S':
      case '0':
	F(H_d);
	F(H_n);
	F(H_S);
	F(H_tD);
	F(H_0);
	break;
      default:
	F(H__1);
	F(H__2);
	F(H_b);
	F(H_b0);
	F(H_f);
	F(H_i);
	F(H_IB);
	F(H_IC);
	F(H_ID);
	F(H_IE);
	F(H_IF);
	F(H_Ik);
	F(H_IN);
	F(H_It);
	F(H_Ir);
	F(H_m);
	F(H_o);
	F(H_P);
	F(H_s);
	F(H_sv);
	F(H_TN);
	F(H_v);
	break;
   }
    exit(err);			/* normal exit if -Ix */
}

#ifdef NovaStor

int pc_dev_no = -1;

long int
pc_open(tape_name,mode,mask)
     char *tape_name;
     int mode,mask;
{
#ifdef HT
    long int ret;
#endif /* HT */

    if (disk_mode)
	return (open(tape_name,mode,mask));
    else
	{
#ifdef HT
	    unsigned int pc_blksize = MAXBUF;

	    DEBUG_0(D_512,"pc_open: calling htopen.");
	    ret = htopen(imode?2:1,pc_blksize);
	    DEBUG_0(D_512,"pc_open done.");
#endif /* HT */
	    return(PC_DEV);		/* return funny fd */
	}
}

char *initif();			/* Is there a .h file for this??? */
char *memacc();

long int
pc_init()
{
    long int tmp_long;
    int j;
    char devtype = 0;		/* 3 = exabyte 8mm cartridge */
    int idevtype;
    int devblksize;
#define BUFSEGS  (MAXBUF + MAXBUF - 1L) >> 12
    unsigned int pc_bufsegs = BUFSEGS;
#ifndef HT
    byte *xbuff;
#endif /* HT */

#ifndef HT
    xbuff = (byte *) initif((unsigned) 0);
    DEBUG_1(D_512,"@pc_int: initif(0) = 0x%0lx",xbuff);
    if (!disk_mode) {
	long int L1,L2;

	j = initdrv(tape_drive_no,&devtype,&devblksize);
	DEBUG_2(D_1,"@pc_init: initdrv() = %ld, devblksize = %ld",
		(L1=j),(L2=devblksize));
    }
#else /* HT */
    if (!disk_mode) {
	j = htinit(&devtype);
	if (j) {
	    fprintf(stderr,"%s: htinit failed\n",program_name);
	    return(-1);
	}
    }
#endif /* HT */

#ifdef MEMALLOC
    obuff = (byte *) memacc(pc_bufsegs);
    buff = (byte *) memacc(pc_bufsegs);
#endif /* MEMALLOC */

    idevtype = devtype;
    DEBUG_3(D_1,"@pc_init: devtype = %ld, buff = 0x%0lx, obuff = 0x%0lx",
	    (tmp_long=idevtype),buff,obuff);

    if (disk_mode)
	return(0);

    if (tape_drive_no < 0 || tape_drive_no > 7) {
	    fprintf(stderr,"%s: Invalid tape drive number %ld, must be 0-7\n",
		    program_name,(tmp_long=tape_drive_no));
	    return (-1);		/* invalid number */
    }
    j = busscan & 0x7fffffff;	/* bit mask of tape drives found */
    if (((j>>tape_drive_no) & 1) == 0) {
	    fprintf(stderr,"%s: %s is not a tape drive\n",
		    program_name,tape_name);
	    return (-1);	/* not a tape drive */
    }


    if (buff == NULL) {
 	    fprintf(stderr,"%s: Could not get buffer from initif\n",
		    program_name);
	    return(-1);
    }
#ifdef TDMDOC		/* the doc says */
#define TSTATUSREADY 1	/* this bit on means ready */
#else /* TDMDOC	*/	/* but the code says */
#define TSTATUSREADY 0	/* anything else is an error */
#endif /* TDMDOC */
    j = tstatus(tape_drive_no);
    DEBUG_1(D_1,"@pc_init: tstatus = 0x%0lx",j);
    if (j&1 == TSTATUSREADY) {
 	    fprintf(stderr,"%s: Tape %s is not ready\n",
		    program_name,tape_name);
	    return(-1);
    }
    pc_dev_no = tape_drive_no;
    if (kludge & k_512 && !inmode) {
	/* write a tape mark to tension tape */
	pc_close(PC_DEV);
	/* back up over it to overwrite it */
	pc_space(PC_DEV,PC_SF,-1);
    }
    return(PC_DEV);		/* return funny fd */
}

long int
pc_read(tapefd, buff, bcount)
     int tapefd;
     IOCOUNT bcount;
     byte *buff;
{
    long int ret;
    long int L1,L2,L3;
    unsigned int actlen = 0;
    IOCOUNT rwcount = bcount;
#ifdef HT
    struct tbfr {
        byte *bufptr;
	long buflen;
} bufr;
#endif /* HT */

    DEBUG_0(D_128,"Reading from tape.");
    if (disk_mode) {
	return((IOCOUNT) read(tapefd,buff,rwcount));
    }
    if (tapefd != PC_DEV) {
	fprintf(stderr,"%s: Internal error in pc_read -- not tape device\n",
				    program_name);
			    exit(ERR_INTERN);
    }
    DEBUG_4(D_64,"@pc_read: tread(%ld,0x%0lx,%ld,%ld)",
		(L1=pc_dev_no),buff,(L2=rwcount),(L3=actlen));

#ifndef HT
    ret = tread(pc_dev_no, buff, rwcount, &actlen);
#else /* HT */
    bufr.bufptr = buff;
    ret = htget(bufr);
    actlen = bufr.buflen;
#endif /* HT */
    DEBUG_2(D_64,"@pc_read: ret = %ld, actlen = %ld",
	    (L1=ret),(L2=actlen));
    switch(ret) {
      case 0:
      case 1:
	return(actlen);
      case 2:
	return(0);
      default:
	return(-1);
    }
}

long int
pc_write(tapefd, buff, bcount)
     int tapefd;
     IOCOUNT bcount;
     byte *buff;
{
    long int ret;
    long int tmp_long;
    IOCOUNT rwcount = bcount;
#ifdef HT
    struct tbfr {
        byte *bufptr;
	long buflen;
} bufr;
#endif /* HT */

    DEBUG_1(D_128,"@pc_write: Writing to tape, rwcount=%ld.",
	    (tmp_long=rwcount));
    if (disk_mode)
	return((IOCOUNT) write(tapefd,buff,rwcount));
    if (tapefd != PC_DEV) {
	fprintf(stderr,"%s: Internal error in pc_write -- not tape device\n",
				    program_name);
			    exit(ERR_INTERN);
    }
#ifndef HT
    ret = twrite(pc_dev_no, buff, rwcount);
#else /* HT */
    bufr.buflen = bcount;
    bufr.bufptr = buf;
    ret = htput(bufr);
#endif /* HT */
    DEBUG_1(D_128,"@pc_write_1: Wrote to tape, ret=%ld",ret);
    switch(ret) {
      case 0:			/* write OK */
	return(rwcount);
      case 1:			/* write OK, EOT sensed */
				/* error, as we have no code to deal with it */
      default:
	return(-1);
    }
}

long int
pc_close(tapefd)
     int tapefd;
{
    long int tmp_long;
    long int ret;

    if (disk_mode)
	return(close(tapefd));

    if (tapefd != PC_DEV) {
	fprintf(stderr,"%s: Internal error in pc_close -- not tape device\n",
				    program_name);
			    exit(ERR_INTERN);
    }
#ifdef HT
    DEBUG_0(D_512,"Closing tape (htclose).");
    ret = htclose();
#else /* HT */
    if (!inmode) {
	if (kludge & k_512) {
	    DEBUG_1(D_512,"@pc_close_1: Writing tape mark, unit = %ld.",
		    (tmp_long=pc_dev_no));
	    ret = twtm(pc_dev_no,1);	/* write only 1 tape mark for end of file */
	    if (ret) {
		fprintf(stderr,"%s: error %ld writing tape mark at end of file\n",
		    program_name,ret);
	        return(ERR_WTM);
	    }
	} else {
	    DEBUG_0(D_512,"@pc_close_2: Writing 2 tape marks.");
	    ret = twtm(pc_dev_no,2);	/* write 2 tape marks for end of tape */
    	    if (ret) {
	        fprintf(stderr,"%s: error writing tape marks at end of tape\n",
		    program_name);
	        return(ERR_WTM);
	    }

	    DEBUG_0(D_512,"@pc_close_3: Backing up over tape mark.");
	    ret = tspace(pc_dev_no,PC_SF,-1); /* back up over one of the tape marks */
	    if (ret) {
	        fprintf(stderr,"%s: error backing up over last tapemark\n",
		    program_name);
	    return(ERR_BSF);
	    }
        }
    }
#endif /* HT */
    DEBUG_0(D_512,"pc_close done.");
    return(0);
}

long int
pc_space(tapefd, type, count)
     int tapefd, type, count;
{
    long int ret;

    if (tapefd != PC_DEV) {
	fprintf(stderr,"%s: Internal error in pc_space -- not tape device\n",
				    program_name);
			    exit(ERR_INTERN);
    }
    {
	long int L1,L2;
	DEBUG_2(D_512,"spacing type %ld, count=%ld.",
		(L1=type),(L2=count));
    }
#ifndef HT
    ret = tspace(pc_dev_no, type, count);
#else /* HT */
    ret = htspace(type,count);
#endif /* HT */
    DEBUG_0(D_512,"pc_space done.");
    return(ret);
}
#endif /* NovaStor */

			/* timing tests */

static int thbuf[512/4] = {10,22,92,12,24,
			       0,99,128,5,128,
			       0,0,0,0,0,
			       0,0,0,0,0,
			       0,64,2,128,256,
			       0};
    static short tdbuf[8192/2];

			/* test_a */
void
test_a(dbgnframes)
     int dbgnframes;
{
    long int L1,L2,L3;
    time_t dbgtime1,dbgtime2;
    int k;
    int dbgnframe;

    /* test_a: write 1 or more frames to tape */

    for (dbgnframe = 0; dbgnframe < dbgnframes; dbgnframe++) {

	DEBUG_2(D_32,"@test_a: writing frame %ld hdr, %ld bytes to tape",
		(L1=dbgnframe),(L2=512));
	if (debug & D_256) {
	    (void) time(&dbgtime1);
	}
	rmtwrite(tapefd,(char *) thbuf,512L);	/* write header */
	if (debug & D_256) {
	    (void) time(&dbgtime2);
	    if (dbgtime2-dbgtime1 > 1)
		DEBUG_2(D_256,"@test_a_wrhdr: frame %ld hdr write took %ld seconds",
			(L1=dbgnframe),(L1=dbgtime2-dbgtime1));
	}
	for (k = 0; k < thbuf[9-1]-1; k++) {
	    DEBUG_3(D_32,"@test_a: writing frame %ld record %3ld, %ld bytes to tape",
		    (L1=dbgnframe),(L2=k),(L3=8192));
	    if (debug & D_256) {
		(void) time(&dbgtime1);
	    }
	    rmtwrite(tapefd,(char *) tdbuf,8192L); /* write data record */
	    if (debug & D_256) {
		(void) time(&dbgtime2);
		if (dbgtime2-dbgtime1 > 1)
		    DEBUG_3(D_256,"@test_a_wrdata: frame %ld record %ld write took %ld seconds",
			    (L1=dbgnframe),(L2=k),(L3=dbgtime2-dbgtime1));
	    }
	}
    }
}
			/* test_b */
int waits[] = {300,120,120,60,300};
int nwaits = (sizeof(waits)/sizeof(int));

void
test_b(dbgnframes)
     int dbgnframes;
{
    int j;

    for (j = 0; j < nwaits; j++) {
	xsleep(waits[j],-1);
	test_a(dbgnframes);
    }
}
			/* test */
void
test()
{
    long int L1,L2,L3;
    int i;
    int dbgn;
    int dbgnlines,dbgnpixels,dbgnrecs;
    int dbgnframes = 2;

    if (kludge & k_2048)
	dbgn = 1;				/* k_2048: 1024x1024 */
    else
	dbgn = 0;				/* k_1024: 512x512 */

    dbgnpixels = (dbgn ? 1024 :512);
    dbgnlines = (dbgn ? 1024 :512);
    dbgnrecs = 1 + (dbgnlines*dbgnpixels*2/8192);

    thbuf[8-1]	= dbgnlines;			/* nlines */
    thbuf[9-1]	= dbgnrecs;			/* nrecs */
    thbuf[10-1]	= dbgnpixels;			/* Aw/ln */
    thbuf[23-1]	= dbgnpixels;			/* Tw/ln */


    for (i = 0; i < 8192/2; i++) {
	tdbuf[i] = i;
    }
    fprintf(stderr,"%s: performing test write sequence\n",program_name);
    fprintf(stderr,"%s: Frame size is %ld x %ld, writing %ld frames at a time\n",
	    program_name,(L1=dbgnpixels),(L2=dbgnlines),(L3=dbgnframes));
    if (DBGfil != stderr)
    DEBUG_3(D_1,"Frame size is %ld x %ld, writing %ld frames at a time\n",
	    (L1=dbgnpixels),(L2=dbgnlines),(L3=dbgnframes));
    for (i = 1; i <= 2; i++) {
	DEBUG_0(D_1,"@test: opening tape file");
	if ((tapefd = rmtopen (tape_name,wrmode,0600)) < 0) {
	    fprintf(stderr,"%s: cannot open %s",
		    program_name,tape_name);
	    perror(program_name);
	    exit(ERR_OPEN);
	}
	DEBUG_0(D_1,"@test: opened tape file");
	test_b(dbgnframes);
	DEBUG_0(D_1,"@test: closing tape file");
	rmtclose(tapefd);
	DEBUG_0(D_1,"@test: closed tape file");
    }
    if (kludge & k_512) {
	DEBUG_0(D_1,"@test: closing tape file");
	rmtclose(tapefd);
	DEBUG_0(D_1,"@test: closed tape file");
    }
}
