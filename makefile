# PE Fortran Solutions
export NPROB = 60
export FC = gfortran
export FFLAGS_OPTIMIZE = -O3 -ffast-math -std=f2018
export FFLAGS_DEBUG = -g -O0 -Wall -Wextra -fcheck=all -fbacktrace
export FPP = fypp
export FPPFLAGS = -DNUM_PROB=$(NPROB)
export BUILD_DIR = $(CURDIR)/build
export MKDIR = mkdir -p
export RM = rm -rf
export EXEC = PE-Fortran.exe

.PHONY: all debug install format clean

all: FFLAGS = $(FFLAGS_OPTIMIZE)
all:
	$(MKDIR) $(BUILD_DIR)
	$(MAKE) --directory=src/util
	$(MAKE) --directory=src/fypp
	$(MAKE) --directory=src/prob
	$(MAKE) --directory=src/main
	$(MAKE) --directory=data
	@echo "Build succeded."

debug: FFLAGS = $(FFLAGS_DEBUG)
debug:
	$(MKDIR) $(BUILD_DIR)
	$(MAKE) --directory=src/util
	$(MAKE) --directory=src/fypp
	$(MAKE) --directory=src/prob
	$(MAKE) --directory=src/main
	$(MAKE) --directory=data
	@echo "Debug build succeded."

install:
	cd $(BUILD_DIR);./$(EXEC) --compute-all
	cp $(BUILD_DIR)/ANSWER.md .
	@echo "Successfully run PE-Fortran."

format:
	fprettify --recursive --indent=4 ./src
	@echo "Successfully format all source files."

clean:
	$(RM) $(BUILD_DIR)
	$(RM) ANSWER.md
	@echo "All generated files removed."
