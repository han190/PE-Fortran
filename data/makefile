BUILD_DIR = ../build
TXT_FILE = $(addprefix $(BUILD_DIR)/, $(wildcard *.txt))
.PHONY: all clean

all: $(TXT_FILE)

clean:
	$(RM) $(TXT_FILE)

$(BUILD_DIR)/%.txt: %.txt
	ln -f $< $@