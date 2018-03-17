CC = ghc
CC_FLAGS = -dynamic

SOURCES = 1-10.hs
EXEC = $(SOURCES:.hs=.out)

all: $(EXEC)

modules.out: modules.hs Geometry.hs
maketype.out: maketype.hs Shapes.hs Record.hs
%.out: %.hs
	$(CC) $(CC_FLAGS) -o $@ $<

clean:
	rm -rf *.exe *.o *.hi
	rm -rf $(EXEC)