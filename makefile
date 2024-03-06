fc=gfortran
profile?=release
build_dir=build
current_dir=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))
data_dir=$(current_dir)/data
src_dir=src
preproc_dir=$(src_dir)/preprocess
local_dir=${HOME}/.local
prefix=$(local_dir)/bin
data_prefix=$(local_dir)/share/PE-Fortran-data
test_args?=

ifeq ($(fc), gfortran)
	ifeq ($(profile), debug)
		fc_flags=-g -o0 -Wall -Wextra -pedantic -fbounds-check \
			-fimplicit-none -fpic -Wno-uninitialized \
			-fcheck=all -fbacktrace -std=f2018 -ffree-form \
			-fcheck=array-temps -Werror=implicit-interface
	else ifeq ($(profile), release)
		fc_flags=-O3 -march=native -std=f2018
	endif
	
	compile=$(fc) $(fc_flags)
	compile_obj=$(compile) -J$(build_dir)
	compile_exe=$(compile) -I$(build_dir)
endif

toolkit_dir=$(src_dir)/toolkit
toolkit_src=$(wildcard $(toolkit_dir)/*.f90)
toolkit_obj=$(filter-out $(build_dir)/module_toolkit.o, \
	$(subst $(toolkit_dir), $(build_dir), \
	$(patsubst %.f90, %.o, $(toolkit_src))))

problems_dir=$(src_dir)/problems
problems_src=$(wildcard $(problems_dir)/*.f90)
problems_obj=$(subst $(problems_dir), $(build_dir), \
	$(patsubst %.f90, %.o, $(problems_src)))

.PHONY: clean build preprocess install test
all: $(build_dir)/PE-Preprocess preprocess $(build_dir)/PE-Fortran

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

$(build_dir)/PE-Fortran: $(src_dir)/project_euler.f90 $(build_dir)/module_driver.o
	$(compile_exe) -o $@ $< $(build_dir)/*.o
	@echo Project is successfully built.

$(build_dir)/module_driver.o: $(src_dir)/module_driver.f90 \
	$(build_dir)/module_problem.o $(problems_obj)
	$(compile_obj) -c $< -o $@

$(build_dir)/%.o: $(problems_dir)/%.f90
	$(compile_obj) -c $< -o $@

$(build_dir)/module_problem.o: $(src_dir)/module_problem.f90 \
	$(build_dir)/module_toolkit.o
	$(compile_obj) -c $< -o $@

$(build_dir)/module_toolkit.o: $(toolkit_dir)/module_toolkit.f90 $(toolkit_obj)
	$(compile_obj) -c $< -o $@

$(build_dir)/module_list.o: $(toolkit_dir)/module_list.f90
	$(compile_obj) -c $< -o $@

$(build_dir)/module_multiprecision.o: $(toolkit_dir)/module_multiprecision.f90 \
	$(build_dir)/module_utility.o
	$(compile_obj) -c $< -o $@ 

$(build_dir)/module_permutation.o: $(toolkit_dir)/module_permutation.f90 \
	$(build_dir)/module_utility.o
	$(compile_obj) -c $< -o $@ 

$(build_dir)/module_prime.o: $(toolkit_dir)/module_prime.f90 \
	$(build_dir)/module_utility.o
	$(compile_obj) -c $< -o $@ 

$(build_dir)/module_quicksort.o: $(toolkit_dir)/module_quicksort.f90 \
	$(build_dir)/module_utility.o
	$(compile_obj) -c $< -o $@ 

$(build_dir)/module_utility.o: $(toolkit_dir)/module_utility.f90
	$(compile_obj) -c $< -o $@

preprocess:
	$(build_dir)/PE-Preprocess -d $(data_prefix)

$(build_dir)/PE-Preprocess: $(preproc_dir)/preprocess.f90 \
	$(build_dir)/module_file.o $(build_dir)/module_preprocessor.o
	$(compile_exe) -o $@ $< $(build_dir)/*.o

$(build_dir)/module_preprocessor.o: $(preproc_dir)/module_preprocessor.f90 build
	$(compile_obj) -c $< -o $@

$(build_dir)/module_file.o: $(preproc_dir)/module_file.f90 build
	$(compile_obj) -c $< -o $@

build:
	mkdir -p $(build_dir)

clean:
	rm -rf PE-Preprocess PE-Fortran
	rm -rf $(build_dir)/*.o $(build_dir)/*.mod $(build_dir)/*.smod
	rm -rf $(prefix)/PE-Fortran
	rm -rf $(prefix)/preprocess
	rm -rf $(data_prefix)
	@echo All files cleaned.
