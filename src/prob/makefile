SRC = $(wildcard *.f90)
BUILD_DIR = ../../build

SRCS = $(addprefix $(BUILD_DIR)/, $(SRC))
OBJS = $(addprefix $(BUILD_DIR)/, $(SRC:.f90=.o))
MODS = $(addprefix $(BUILD_DIR)/, $(SRC:.f90=.mod))
SMODS = $(addprefix $(BUILD_DIR)/, $(SRC:.f90=.smod))

.PHONY: all clean

all: $(OBJS)

clean:
	$(RM) $(OBJS) $(MODS) $(SMODS)

$(BUILD_DIR)/%.o $(BUILD_DIR)/%.smod: %.f90
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -c $< -o $@