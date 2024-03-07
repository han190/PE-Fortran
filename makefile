FC?=gfortran
profile?=release
build_dir=build/$(FC)_$(profile)_makefile
current_dir=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))
data_dir=$(current_dir)/data
src_dir=src
include_dir=$(build_dir)/include
local_dir=${HOME}/.local
prefix=$(local_dir)/bin
data_prefix=$(local_dir)/share/PE-Fortran-data
test_args?=

ifeq ($(FC), gfortran)
	FFLAGS=-std=f2018
	ifeq ($(profile), debug)
		FFLAGS+=-g -o0 -Wall -Wextra -pedantic -fbounds-check -fimplicit-none \
			-fPIC -Wno-uninitialized -fcheck=all -fbacktrace -ffree-form \
			-fcheck=array-temps -Werror=implicit-interface
	else ifeq ($(profile), release)
		FFLAGS+=-O3 -march=native
	endif
	
	compile=$(FC) $(FFLAGS) -I$(include_dir)
	compile_obj=$(compile) -J$(build_dir)
	compile_exe=$(compile) -I$(build_dir)
endif

# Preprocess objects
preproc_filename=module_file module_preprocessor preprocess
preproc_dir=$(src_dir)/preprocess
preproc_src=$(addprefix $(preproc_dir)/, $(addsuffix .f90, $(preproc_filename)))
preproc_obj=$(addprefix $(build_dir)/, $(addsuffix .o, $(preproc_filename)))

# Toolkit objects
toolkit_filename=module_utility module_quicksort module_prime \
	module_permutation module_multiprecision module_list module_toolkit
toolkit_dir=$(src_dir)/toolkit
toolkit_src=$(addprefix $(toolkit_dir)/, $(addsuffix .f90, $(toolkit_filename)))
toolkit_obj=$(addprefix $(build_dir)/, $(addsuffix .o, $(toolkit_filename)))

# Problems objects
problems_dir=$(src_dir)/problems
problems_src=$(wildcard $(problems_dir)/*.f90)
problems_obj=$(subst $(problems_dir), $(build_dir), \
	$(patsubst %.f90, %.o, $(problems_src)))

# PE objects
pe_filename=module_problem module_driver project_euler
pe_src=$(addprefix $(src)/, $(addsuffix .f90, $(pe_filename)))
pe_obj=$(addprefix $(build_dir)/, $(addsuffix .o, $(pe_filename)))

.PHONY: clean build preprocess install test
all: $(build_dir)/PE-Preprocess preprocess \
	$(build_dir)/libpetk.a $(build_dir)/PE-Fortran

test:
	$(build_dir)/PE-Fortran -d $(data_dir) $(test_args)

install:
	mkdir -p $(data_prefix)
	mkdir -p $(prefix)
	cp $(data_dir)/* $(data_prefix)
	cp $(build_dir)/PE-Preprocess $(prefix)
	cp $(build_dir)/PE-Fortran $(prefix)
	@echo Executables installed to $(prefix)
	@echo Data copied to $(data_prefix)

$(build_dir)/PE-Fortran: $(pe_obj) $(problems_obj)
	$(compile_exe) -o $@ $(pe_obj) $(problems_obj) -L$(build_dir) -lpetk
	@echo PE-Fortran is built successfully.

$(build_dir)/%.o: $(src_dir)/%.f90
	$(compile_obj) -c $< -o $@

$(build_dir)/%.o: $(problems_dir)/%.f90
	$(compile_obj) -c $< -o $@

libs: $(build_dir)/libpetk.a
$(build_dir)/libpetk.a: $(toolkit_obj)
	$(AR) rcs $@ $(toolkit_obj)

$(build_dir)/%.o: $(toolkit_dir)/%.f90
	$(compile_obj) -c $< -o $@

preprocess:
	$(build_dir)/PE-Preprocess -d $(data_prefix) -i $(include_dir)

$(build_dir)/PE-Preprocess: $(preproc_obj)
	$(compile_exe) -o $@ $(preproc_obj)

$(build_dir)/%.o: $(preproc_dir)/%.f90 build
	$(compile_obj) -c $< -o $@

build:
	mkdir -p $(build_dir) $(include_dir)

clean:
	$(RM) -r $(build_dir)
	$(RM) $(prefix)/PE-Fortran $(prefix)/preprocess
	$(RM) -r $(data_prefix)
	@echo All files cleaned.
