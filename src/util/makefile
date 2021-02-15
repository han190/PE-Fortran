SRC = euler_utils_m.f90 \
	euler_var_arr_m.f90 \
	euler_primes_m.f90 \
	euler_mi_m.f90

OBJS = $(addprefix $(BUILD_DIR)/, $(SRC:.f90=.o))
MODS = $(addprefix $(BUILD_DIR)/, $(SRC:.f90=.mod))

.PHONY: all

all: $(OBJS) $(MODS)

$(BUILD_DIR)/%.o $(BUILD_DIR)/%.mod: %.f90
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -c $< -o $@