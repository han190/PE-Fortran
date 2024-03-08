compiler?=gfortran
profile?=release
src_dir:=src
current_dir=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))
build_dir=$(current_dir)build/$(compiler)_$(profile)_makefile
data_dir=$(current_dir)data
include_dir=$(build_dir)/include
local_dir=${HOME}/.local
prefix=$(local_dir)/bin
data_prefix=$(local_dir)/share/PE-Fortran-data
args?=

ifeq ($(compiler), gfortran)
	compiler_flags=-std=f2018 -march=native
	ifeq ($(profile), debug)
		compiler_flags+=-g -O0 -Wall -Wextra -pedantic -fbounds-check \
			-fimplicit-none -fPIC -Wno-uninitialized -fcheck=all -fbacktrace \
			-ffree-form -fcheck=array-temps -Werror=implicit-interface
	else ifeq ($(profile), release)
		compiler_flags+=-O3 -march=native
	endif
	compile=$(compiler) $(compiler_flags) \
		-I$(include_dir) -J$(build_dir)
else ifeq ($(compiler), ifx)
	compiler_flags=-stand f18 -mtune=native
	ifeq ($(profile), debug)
		compiler_flags+=-O0 -warn all -check all,nouninit -g -traceback -no-simd
	else ifeq ($(profile), release)
		compiler_flags+=-O1 -ipo -xHost
	endif
	compile=$(compiler) $(compiler_flags) \
		-I$(include_dir) -module $(build_dir)
	AR=xiar
endif

# Preprocess objects
preproc_filename=module_file module_preprocessor preprocess
preproc_dir=$(src_dir)/preprocess
preproc_obj=$(addprefix $(build_dir)/, \
	$(addsuffix .o, $(preproc_filename)))

# Toolkit objects
toolkit_filename=module_utility module_quicksort module_prime \
	module_permutation module_multiprecision module_list module_toolkit
toolkit_dir=$(src_dir)/toolkit
toolkit_obj=$(addprefix $(build_dir)/, \
	$(addsuffix .o, $(toolkit_filename)))

# Problems objects
problems_dir=$(src_dir)/problems
problems_src=$(wildcard $(problems_dir)/*.f90)
problems_obj=$(subst $(problems_dir), $(build_dir), \
	$(patsubst %.f90, %.o, $(problems_src)))

# PE objects
pe_filename=module_problem module_driver project_euler
pe_obj=$(addprefix $(build_dir)/, $(addsuffix .o, $(pe_filename)))

.PHONY: clean build preprocess install test
all: $(build_dir)/PE-Preprocess preprocess \
	$(build_dir)/libpetk.a $(build_dir)/PE-Fortran

test:
	$(build_dir)/PE-Fortran -d $(data_dir) $(args)

install:
	mkdir -p $(data_prefix)
	mkdir -p $(prefix)
	cp $(data_dir)/* $(data_prefix)
	cp $(build_dir)/PE-Preprocess $(prefix)
	cp $(build_dir)/PE-Fortran $(prefix)
	@echo Executables installed to $(prefix)
	@echo Data copied to $(data_prefix)
	@echo PE-Fortran is installed successfully.

$(build_dir)/PE-Fortran: $(pe_obj) $(problems_obj)
	$(compile) -o $@ $(pe_obj) $(problems_obj) -L$(build_dir) -lpetk
	@echo PE-Fortran is built successfully.

$(build_dir)/%.o: $(src_dir)/%.f90
	$(compile) -c $< -o $@

$(build_dir)/%.o: $(problems_dir)/%.f90
	$(compile) -c $< -o $@

libs: $(build_dir)/libpetk.a
$(build_dir)/libpetk.a: $(toolkit_obj)
	$(AR) rcs $@ $(toolkit_obj)

$(build_dir)/%.o: $(toolkit_dir)/%.f90
	$(compile) -c $< -o $@

preprocess:
	$(build_dir)/PE-Preprocess -d $(data_prefix) -i $(include_dir)

$(build_dir)/PE-Preprocess: $(preproc_obj)
	$(compile) -o $@ $(preproc_obj)

$(build_dir)/%.o: $(preproc_dir)/%.f90 build
	$(compile) -c $< -o $@

build:
	mkdir -p $(build_dir) $(include_dir)

clean:
	$(RM) -r $(build_dir)
	@echo Build files cleaned.

uninstall:
	$(RM) $(prefix)/PE-Fortran $(prefix)/PE-Preprocess
	$(RM) -r $(data_prefix)
	@echo PE-Fortran uninstalled.
