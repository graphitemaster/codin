# Compilation options:
# * LTO       - Link time optimization
# * ASAN      - Address sanitizer
# * TSAN      - Thread sanitizer
# * UBSAN     - Undefined behavior sanitizer
# * DEBUG     - Debug build
# * PROFILE   - Profile build
# * SRCDIR    - Out of tree builds
# * UNUSED    - Removed unused references
LTO ?= 0
ASAN ?= 0
TSAN ?= 0
UBSAN ?= 0
DEBUG ?= 0
PROFILE ?= 0
SRCDIR ?= src
UNUSED ?= 1

# Some recursive make functions to avoid shelling out.
rwildcard = $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2) $(filter $(subst *,%,$2),$d))
uniq = $(if $1,$(firstword $1) $(call uniq,$(filter-out $(firstword $1),$1)))

# Compiler and linker
CC := gcc
CC ?= clang
LD := $(CC)

# Determine build type.
ifeq ($(DEBUG), 1)
	TYPE := debug
else ifeq ($(PROFILE),1)
	TYPE := profile
else
	TYPE := release
endif

BIN := codin

OBJDIR := .build/$(TYPE)/objs
DEPDIR := .build/$(TYPE)/deps

# Collect all .c files for build in the source directory.
SRCS := $(call rwildcard, $(SRCDIR)/, *c)

# Generate object and dependency filenames.
OBJS := $(filter %.o,$(SRCS:%.c=$(OBJDIR)/%.o))

DEPS := $(filter %.d,$(SRCS:%.c=$(DEPDIR)/%.d))

#
# C flags
#
CFLAGS := -Wall
CFLAGS += -Wextra
CFLAGS += -std=c11
CFLAGS += -D_DEFAULT_SOURCE

ifeq ($(DEBUG),1)
	# Optimize for debugging.
	CFLAGS += -O0

	CFLAGS += -g

	# Ensure there's a frame pointer in debug builds.
	CFLAGS += -fno-omit-frame-pointer
else ifeq ($(PROFILE),1)
	# Enable profile options in profile builds.
	CFLAGS += -pg
	CFLAGS += -no-pie

	# Enable debug symbols in profile builds.
	CFLAGS += -g

	# Use slightly less aggressive optimizations in profile builds.
	CFLAGS += -O2
	CFLAGS += -fno-inline-functions
	CFLAGS += -fno-inline-functions-called-once
	CFLAGS += -fno-optimize-sibling-calls
else
	# Disable default C assertions.
	CFLAGS += -DNDEBUG

	# Highest optimization flag in release builds.
	CFLAGS += -O3

	# These are only needed for stack traces in debug builds.
	CFLAGS += -fno-unwind-tables
	CFLAGS += -fno-asynchronous-unwind-tables

	# Disable all the stack protection features in release builds.
	CFLAGS += -fno-stack-protector
	CFLAGS += -fno-stack-check

	ifeq ($(CC),gcc)
		CFLAGS += -fno-stack-clash-protection
	endif

	# Disable frame pointer in release builds when AddressSanitizer isn't present.
	ifeq ($(ASAN),1)
		CFLAGS += -fno-omit-frame-pointer
	else
		CFLAGS += -fomit-frame-pointer
	endif
endif

# Give each function and data it's own section so the linker can remove unused
# references to each, producing smaller, tighter binaries.
ifeq ($(UNUSED), 1)
	CFLAGS += -ffunction-sections
	CFLAGS += -fdata-sections
endif

# Enable link-time optimizations if requested. But not in debug builds.
ifeq ($(LTO),1)
ifeq ($(DEBUG),0)
	CFLAGS += -flto
endif
endif

# Sanitizer selection.
ifeq ($(ASAN),1)
	CFLAGS += -fsanitize=address
endif
ifeq ($(TSAN),1)
	CFLAGS += -fsanitize=thread -DRX_TSAN
endif
ifeq ($(UBSAN),1)
	CFLAGS += -fsanitize=undefined
endif

#
# Dependency flags
#
DEPFLAGS := -MMD
DEPFLAGS += -MP

#
# Linker flags.
#
LDFLAGS := -lm
LDFLAGS += -lpthread

# Strip unused symbols if requested.
ifeq ($(UNUSED), 1)
	LDFLAGS += -Wl,--gc-sections
endif

# Enable profiling if requested.
ifeq ($(PROFILE),1)
	LDFLAGS += -pg
	LDFLAGS += -no-pie
endif

# Enable link-time optimizations if requested.
ifeq ($(LTO),1)
	LDFLAGS += -flto
endif

# Sanitizer selection.
ifeq ($(ASAN),1)
	LDFLAGS += -fsanitize=address
endif
ifeq ($(TSAN),1)
	LDFLAGS += -fsanitize=thread
endif
ifeq ($(UBSAN),1)
	LDFLAGS += -fsanitize=undefined
endif

all: $(BIN)

# Build artifact directories
$(DEPDIR):
	@mkdir -p $(addprefix $(DEPDIR)/,$(call uniq,$(dir $(SRCS))))
$(OBJDIR):
	@mkdir -p $(addprefix $(OBJDIR)/,$(call uniq,$(dir $(SRCS))))

$(OBJDIR)/%.o: %.c $(DEPDIR)/%.d | $(OBJDIR) $(DEPDIR)
	$(CC) -MT $@ $(DEPFLAGS) -MF $(DEPDIR)/$*.Td $(CFLAGS) -c -o $@ $<
	@mv -f $(DEPDIR)/$*.Td $(DEPDIR)/$*.d

$(BIN): $(OBJS)
	$(LD) $(OBJS) $(LDFLAGS) -o $@

clean:
	rm -rf $(DEPDIR) $(OBJDIR) $(BIN)

$(DEPS):
include $(wildcard $(DEPS))