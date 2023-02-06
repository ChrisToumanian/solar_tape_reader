FC = f77
FCFLAGS = -O

SRCS = cmpdop.f cmpsum.f cmpmag.f cmpdiff.f gcsubs.f centroid.f getell.f getlmb.f diaggja.f fitell.f fitslop.f ellstuf.f despik3.f raw2sum.f cmplmb.f modfn.f xdir.f gcentr.f raw2dop.f raw2mag.f

OBJS = $(SRCS:.f=.o)

all: raw2dop raw2dif raw2mag

raw2dop: raw2dop.o gcentr.o gcsubs.o centroid.o getell.o getlmb.o fitell.o diaggja.o ellstuf.o cmpdop.o cmplmb.o fitslop.o despik3.o modfn.o xdir.o
	$(FC) $(FCFLAGS) -o $@ $^ -Bstatic

raw2dif: raw2sum.o gcentr.o gcsubs.o centroid.o getell.o getlmb.o fitell.o diaggja.o ellstuf.o cmpsum.o cmplmb.o fitslop.o despik3.o modfn.o xdir.o
	$(FC) $(FCFLAGS) -o $@ $^ -Bstatic

raw2mag: raw2mag.o gcentr.o gcsubs.o centroid.o getell.o getlmb.o fitell.o diaggja.o  ellstuf.o cmpmag.o despik3.o cmpdiff.o
	$(FC) $(FCFLAGS) -o $@ $^

.f.o:
	$(FC) $(FCFLAGS) -c $<

clean:
	$(RM) $(OBJS) raw2dop raw2dif raw2mag
