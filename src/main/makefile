BUILD_DIR = ../../build
EXE = $(BUILD_DIR)/pe-fortran
ALLOBJS = $(wildcard $(BUILD_DIR)/*.o)

.PHONY: all clean

all:
	$(FC) $(FFLAGS) -J$(BUILD_DIR) euler_main.f90 -o $(EXE) $(ALLOBJS)

clean:
	$(RM) $(BUILD_DIR)/euler_main.o $(EXE)
