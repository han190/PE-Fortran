# PE Fortran Solutions
export NPROB = 60
export FC = gfortran
export FFLAGS = -O3 -ffast-math -std=f2018
export FFLAGS_DEBUG = -g -O0 -Wall -Wextra -fcheck=all -fbacktrace
export FPP = fypp
export FPPFLAGS = -DNUM_PROB=$(NPROB)

.PHONY: all install debug clean

all:
	mkdir -p build
	$(MAKE) --directory=src/util
	$(MAKE) --directory=src/fypp
	$(MAKE) --directory=src/prob
	$(MAKE) --directory=src/main
	$(MAKE) --directory=data

debug: FFLAGS = $(FFLAGS_DEBUG)
debug:
	mkdir -p build
	$(MAKE) --directory=src/util
	$(MAKE) --directory=src/fypp
	$(MAKE) --directory=src/prob
	$(MAKE) --directory=src/main
	$(MAKE) --directory=data

install:
	cd build;./pe-fortran --compute-all
	cp build/ANSWER.md .

clean:
	$(RM) -rf build
	$(RM) ANSWER.md
	@echo "All generated files removed."