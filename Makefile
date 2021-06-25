IDRIS ?= idris2
SRC_DIR = src
TRGT = dot-parse
IDR_FILES := $(SRC_DIR)/Graphics/DOT.idr
IDR_FILES += $(SRC_DIR)/Graphics/DOT/Lexer.idr
IDR_FILES += $(SRC_DIR)/Graphics/DOT/Parser.idr
IPKG_FILE = $(TRGT).ipkg

all: $(TRGT)

build: $(TRGT)

$(TRGT): $(IDR_FILES)
	$(IDRIS) --build $(IPKG_FILE)

install: $(TRGT)
	$(IDRIS) --install $(IPKG_FILE)

.PHONY: all build clean

clean:
	$(RM) -r build
	$(RM) -r $(SRC_DIR)/build

