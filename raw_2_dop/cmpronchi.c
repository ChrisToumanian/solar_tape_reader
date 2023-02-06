/* compute the ronchi angle of an I*2 CCD image            */
/* the ronchi angle is assumed to be within +/- 25o        */
/* SGK/31-Aug-89/22-Feb-94                     Ver 1.2/0/C */

#include <stdio.h>
#include <math.h>
/* ver 1.2: increased NPTSMAX from 64000 to 80000 */
#define NPTSMAX 80000
#define NMAXX   128
#define True    1
#define False   0
#define Rad2Deg 57.29577951

struct Points {
  int x;
  int y;     
  struct Points *next; 
  struct Points *prev; 
};
void makelines();

main (argc, argv)
int    argc;
char **argv;

{
  register int i, j;
  int    nrows = 1024, ncols = 1024, npxls, nread, js, je, istd;
  unsigned int nbyts, verbose = False;
  short *image;
  int   *didj;
  float  rad = 400.0, xstd = 1.0, std;
  double rad2;
  int    ir, jr;
  struct Points *points, *pntr2point;
  int    npts, n, x, y, nslope;
  int    gaplength = 5;  /* max allowed gap lenght */
  int    linlenmin = 50; /* min length of a line to give a valid slope */
  float  sumx, sumxx, sumy, sumxy, slope, slopemin = 10.0, slopemax = -10.0;
  double sumslope = 0.0, rmsslope = 0.0, angle, anglemin, anglemax;
  FILE  *file = stdin, *fopen();

/* get the options */
  for (i=1; i<argc; i++) {
    if(      strncmp(argv[i], "-nc", 3) == 0 )   /* -nc[ols] */
      ncols = atoi(argv[++i]);
    else if( strncmp(argv[i], "-nr", 3) == 0 )   /* -nr[ows] */
      nrows = atoi(argv[++i]);
    else if( strncmp(argv[i], "-rad", 4) == 0 )  /* -rad */
      rad = atof(argv[++i]);
    else if( strncmp(argv[i], "-xst", 4) == 0 )  /* -xst[d] */
      xstd = atof(argv[++i]);
    else if( strncmp(argv[i], "-gap", 4) == 0 )  /* -gap[lenght] */
      gaplength = atoi(argv[++i]);
    else if( strncmp(argv[i], "-llm", 4) == 0 )  /* -llm */
      linlenmin = atoi(argv[++i]);
    else if( strncmp(argv[i], "-v", 2) == 0 )    /* -v */
      verbose = True;
    else if (i == argc-1) {
      if (argv[i] != "-") 
	if( (file = fopen(argv[i], "r")) == NULL) {
	  fprintf(stderr, "%s: could not open %s\n",argv[0], argv[i]);
	  exit (-1);
	}
    } else {
      fprintf(stderr, "%s: invalid option: %s\n", argv[0], argv[i]);
      fprintf(stderr,"Usage: %s [-ncols #] [-nrows #] [-rad #.#] [-xstd #.#]\n\
       [-gaplength #] [-llm #] filename\n\
       or a '-' for stdin\n", argv[0]);
      exit (-1);
    }
  }

/* echo params    */
/*  if(verbose) { */
    printf("%s: Ver 1.2/0/C\n\n", argv[0]);
    printf(" image from %s\n       %d by %d\n rad = %f, xstd = %f\n\
 gap length = %d, line length min = %d\n\n",argv[argc-1], 
	   ncols, nrows, rad, xstd, gaplength, linlenmin);
/*  } */

/* allocate room for didj and points */
  if(verbose) printf("malloc()ing ... ");
  nbyts = sizeof(*didj)*nrows;    
  if ( (didj = (int *) malloc(nbyts)) == NULL) {
    fprintf(stderr,"%s: cannot malloc didj space\n", argv[0]);
    exit(-1);
  }
  nbyts = sizeof(*points)*NPTSMAX;
  if ( (points = (struct Points *) malloc(nbyts) ) == NULL ) {
    fprintf (stderr,"%s: cannot malloc points space\n", argv[0]);
    exit(-1);
  }
  rad2 = rad*rad;

/* read the image */
  npxls = nrows*ncols; nbyts = 2*npxls;
  if( (image = (short *) malloc(nbyts)) == NULL ){
    fprintf(stderr,"%s: could not allocate memory for image buffer.\n",
	    argv[0]);
    exit(-2);
  }
  if(verbose) printf("reading ...");
  if( (nread = fread(image, 1, nbyts ,file)) != nbyts){
    fprintf(stderr,"%s: EOF reached on %s (after %d bytes)\n",
	    argv[0], argv[argc-1], nread);
    exit(-2);
  }
  if(argv[argc-1] != "-") fclose(file);
  
  if(verbose) printf("done.\n");
  npts = 0;
  for (i=0; i<ncols; i++) {                /* loop on the columns */
    ir = i-ncols/2;
    if (abs(ir) <= rad) {                  /* consider only inside rad */
      jr = (int) sqrt( rad2 - (double) (ir*ir));
      js = nrows/2 - jr - 1;
      je = nrows/2 + jr - 1;

      for (j=js; j<=je; j++)             /* compute didj */
	didj[j] = abs( (int)(image[i+j*ncols] - image[i+(j-1)*ncols]));
      std = 0.0;
      for (j=js; j<=je; j++)             /* compute std of didj */
	std += didj[j]*didj[j];
      std /= (je-js+1);
      std = sqrt((double) std);
      
      std *= xstd;                       /* multiply it by xstd */
      istd = std;
      for (j=js+1; j<=je; j++)           /* find max above std in didj */
	if(didj[j] >= istd) {
	  n = (didj[j+1]-didj[j])*(didj[j]-didj[j-1]);
	  if ( n < 0.0 ) {              /* got a peak */
	    points[npts].x = i;
	    points[npts].y = j;
	    points[npts].prev = points[npts].next = NULL;
	    npts++;
	    if(npts == NPTSMAX) goto DoneForNpts;
	  }
	}
    }
  }
 DoneForNpts:
  printf("%d point(s),", npts);
  free(image);                          /* don't need it anymore */

/* make lines from the points */
  makelines(npts, points, gaplength);

/* now you can follow the lines, -> compute each line slope */
  for (i=0 ; i<npts; i++)
    if(points[i].prev == NULL){           /* from beginning of a line */
      pntr2point = &points[i];
      sumx = sumy = sumxx = sumxy = 0.0;
      n = 0;
      while ((*pntr2point).next != NULL) {        /* to end of a line */
	sumx  += (*pntr2point).x;
	sumy  += (*pntr2point).y;
	sumxx += (*pntr2point).x * (*pntr2point).x;
	sumxy += (*pntr2point).x * (*pntr2point).y;
	n++;
	pntr2point = (*pntr2point).next;
      }
      if ( n > linlenmin ){         /* compute the slope of this line */
	slope = (n*sumxy - sumx*sumy)/(n*sumxx - sumx*sumx);
	if(verbose) printf("%d points, slope = %f\n",n , slope);
	sumslope += (double)slope;
	rmsslope += (double)slope*(double)slope;
	if(slope < slopemin) slopemin = slope;
	if(slope > slopemax) slopemax = slope;
	nslope++;
      }
    }
  if (nslope == 0 ) {
    printf(" but no line longer that 10 pts\n");
    exit (1);
  }  
  else {
    sumslope /= nslope;
    rmsslope = sqrt( rmsslope/nslope - sumslope*sumslope );
    printf(" %d lines, slope: min = %f, max = %f\n <slope> = %f +/- %f \n", 
	   nslope, slopemin, slopemax, sumslope, rmsslope);
    anglemin = Rad2Deg*atan(sumslope-rmsslope);
    anglemax = Rad2Deg*atan(sumslope+rmsslope);
    angle    = Rad2Deg*atan(sumslope);
    printf("\nRonchi angle = %f ( %f to %f ) deg.\n", 
	   angle, anglemin, anglemax);
    exit(0);
    }
}

void makelines(npts, points, gaplength)

int            npts, gaplength;
struct Points *points;

{
  register int i, j;
  int dx, dy, dist, smalestdist;

  for (i=1; i<npts-1; i++){
                         /* for each point but the 1st one and the last one */
    if (points[i-1].x == points[i].x){
                                      /* for each pt that don't start a col */
      smalestdist = gaplength;
      for (j=i-1; j>=0; j--){            /* look for the closest one before */
	if (points[i].x != points[j].x) {                   /* only if != x */
	  dx = points[i].x - points[j].x;
	  dy = points[i].y - points[j].y;
	  dist = dx*dx + dy*dy;
	  if(dist < smalestdist){
	    smalestdist = dist;
	    points[i].prev = &points[j];
	  }
	}
	if ((points[i].x - points[j].x) > smalestdist) j = 0; 
      }
    }

    if (points[i].x == points[i+1].x){ 
                                        /* for each pt that don't end a col */
      smalestdist = gaplength;
      for (j=i; j<npts; j++){             /* look for the closest one after */
	if (points[i].x != points[j].x) {                   /* only if != x */
	  dx = points[i].x - points[j].x;
	  dy = points[i].y - points[j].y;
	  dist = dx*dx + dy*dy;
	  if(dist < smalestdist){
	    smalestdist = dist;
	    points[i].next = &points[j];
	  }
	}
	if ((points[j].x - points[i].x) > smalestdist) j = npts; 
      }
    }
  }
}
