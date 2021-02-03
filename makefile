# PE Fortran Solutions
export NPROB = 60
export FC = gfortran
# export FFLAGS = -Wall -Wextra -fimplicit-none -fcheck=all -fbacktrace \
# 	-Warray-temporaries -Wconversion -finit-real=nan -std=f2018
export FFLAGS = -O3 -ffast-math -std=f2018
export FPP = fypp
export FPPFLAGS = -DNUM_PROB=$(NPROB)

.PHONY: all install clean

all:
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