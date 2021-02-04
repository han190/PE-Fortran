FYPPSRC = euler_interface_m.fypp euler_prob_api_m.fypp
SRC = euler_interface_m.f90 euler_prob_api_m.f90

BUILD_DIR = ../../build
SRCS = $(addprefix $(BUILD_DIR)/, $(SRC))
OBJS = $(addprefix $(BUILD_DIR)/, $(SRC:.f90=.o))
MODS = $(addprefix $(BUILD_DIR)/, $(SRC:.f90=.mod))
SMODS = $(addprefix $(BUILD_DIR)/, $(SRC:.f90=.smod))

.PHONY: all

all: $(SRCS) $(OBJS)

$(BUILD_DIR)/%.f90: %.fypp
	$(FPP) $(FPPFLAGS) $< $@

$(BUILD_DIR)/%.o $(BUILD_DIR)/%.mod $(BUILD_DIR)/%.smod: $(BUILD_DIR)/%.f90
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -c $< -o $@