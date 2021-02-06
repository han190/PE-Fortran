BUILD_DIR = ../build
CUR_DIR = $(shell pwd)
TXT_FILE = $(addprefix $(BUILD_DIR)/, $(wildcard *.txt))

.PHONY: all

all: $(TXT_FILE)

$(BUILD_DIR)/%.txt: %.txt
	cd $(BUILD_DIR); ln -sf $(CUR_DIR)/$< .