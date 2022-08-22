CURDIR    := $(shell pwd)
BASEDIR   := $(if $(patsubst %c_src,,$(lastword $(CURDIR))),$(abspath $(CURDIR)/..),..)
OUTDIR    := $(REBAR_BARE_COMPILER_OUTPUT_DIR)
REBAR_ENV ?= $(shell echo "${REBAR_BUILD_DIR}" | sed 's,^.*_build/,,')

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -noinput -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]), halt(0).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -noinput -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]), halt(0).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]), halt(0).")

SOURCES := $(wildcard *.cpp)
OBJECTS := $(addsuffix .o,  $(basename $(SOURCES)))
TARGETS := $(addprefix $(BASEDIR)/priv/, $(addsuffix .so, $(basename $(SOURCES))))
HEADERS := $(wildcard *.h)

ifneq ($(OUTDIR),$(BASEDIR))
ifneq ($(OUTDIR),)
	TARGETS += $(addprefix $(OUTDIR)/priv/, $(addsuffix .so, $(basename $(SOURCES))))
endif
endif

all: $(TARGETS)

ifneq ($(or $(findstring $(NIF_DEBUG),1 true),$(findstring test,$(REBAR_ENV))),)
  ADDFLAGS := -DDEBUG -O0
else
  ADDFLAGS := -O3 -DNDEBUG
	V        := 0
endif

UNAME_SYS  := $(shell uname -s)

ifeq ($(UNAME_SYS),Darwin)
  CXX      ?= cc++
  LDFLAGS  ?= -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS),FreeBSD)
  CXX 		 ?= cc++
else # ifeq ($(UNAME_SYS),Linux)
  CXX 		 ?= g++
endif

CXXFLAGS += $(ADDFLAGS) $(INC_DIR) -std=c++17 -Wall

CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)

LDLIBS  += -L $(ERL_INTERFACE_LIB_DIR) -lei -ldl
LDFLAGS += -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

$(BASEDIR)/priv/%.so: %.o | $(BASEDIR)/priv
	$(link_verbose) $(CXX) $< $(LDFLAGS) $(LDLIBS) -o $@

$(BASEDIR)/priv:
	@mkdir -p $@

%.o: %.cpp $(HEADERS)
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

ifneq ($(BASEDIR),$(OUTDIR))
ifneq (,$(OUTDIR))
$(OUTDIR)/priv/%.so: $(BASEDIR)/priv/%.so | $(OUTDIR)/priv
	@if [ "$(shell readlink -f $<)" == "$(shell readlink -f $@)" ]; then \
		echo "Skip copying file to $@: source and destination are the same"; \
	else \
	  cp -vf "$<" "$@"; \
	fi

$(OUTDIR)/priv:
	@mkdir -p $@
endif
endif

.PRECIOUS: $(OBJECTS)

clean:
	@rm -f $(TARGETS) $(OBJECTS) *.d
