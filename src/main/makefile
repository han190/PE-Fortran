ALLOBJS = $(wildcard $(BUILD_DIR)/*.o)

.PHONY: all

all:
	$(FC) $(FFLAGS) -J$(BUILD_DIR) euler_main.f90 -o $(BUILD_DIR)/$(EXEC) $(ALLOBJS)
