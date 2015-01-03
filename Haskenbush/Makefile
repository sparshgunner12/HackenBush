PROGS  = haskenbush
SOURCES = haskenbush.hs

all : $(PROGS)

haskenbush : $(SOURCES)
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
