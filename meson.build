project('PE-Fortran', 'fortran', 
    version: '0.0.3', 
    meson_version: '>=0.51',
    license: 'MIT',
    default_options: [
        'buildtype=release', 
        'fortran_std=f2018'
    ]
)

# fortran stdlib
fortran_stdlib = dependency('fortran_stdlib', version: '>=0.1.0')

# Meson doesn't have wildcards, instead one has to use extra commands if 
# they have many sources files. For more details, consult:
# https://github.com/mesonbuild/meson/blob/master/docs/markdown/FAQ.md

result = run_command('python', 'problem_id.py')
problem_numbers = result.stdout().split()

# Number of problems
nproblems = problem_numbers[-1].to_int()
message('Total number of problems available: @0@'.format(nproblems))

# Directories
src_dir = 'src'
euler_dir = 'src/euler'
test_dir = 'src/test'
app_dir = 'src/app'

# Check compiler
fc = meson.get_compiler('fortran')
if fc.get_id() == 'gcc'
    if get_option('buildtype') == 'release'
        dialect = [
            '-funroll-loops',
            '-Wimplicit-interface',
            '-fPIC',
            '-fmax-errors=1',
            '-fcoarray=single'
        ]
    elif get_option('buildtype') == 'debug'
        dialect = [
            '-fcheck=all', 
            '-fbacktrace', 
            '-fdump-core',
            '-pedantic', 
            '-fcheck=all', 
            '-fimplicit-none',
            '-fPIC',
            '-fmax-errors=1',
            '-fcheck=bounds',
            '-fcheck=array-temps',
            '-fcoarray=single'
        ]
    endif
elif fc.get_id() == 'intel'
    if get_option('buildtype') == 'release'
        dialect = ['-xHost', '-Ofast']
    elif get_option('buildtype') == 'debug'
        dialect = ['-traceback', '-check', '-fpe0', '-O0']
    endif
endif
add_global_arguments(dialect, language: 'fortran')

# Fortran preprocessor
fypp = find_program('fypp')
fypp_flag = '-DNUM_PROB=@0@'.format(nproblems)
message('fypp_flag = @0@'.format(fypp_flag))
gen_f90 = generator(
    fypp, output: '@BASENAME@.f90',
    arguments: ['@INPUT@', '@EXTRA_ARGS@', '@OUTPUT@']
)
fypp_files = [
    src_dir/'utility_module.fypp',
    src_dir/'prime_module.fypp',
    src_dir/'multiprecision_module.fypp',
    src_dir/'multiprecision_submodule.fypp',
    src_dir/'interface_module.fypp', 
    src_dir/'problem_module.fypp'
]

fypp_src = []
foreach fypp_file: fypp_files
    fypp_src += gen_f90.process(fypp_file, extra_args: fypp_flag)
endforeach

# Source files
util_src = [
    src_dir/'constant_module.f90',
    src_dir/'driver_module.f90'
]

app_src = [
    app_dir/'project_euler.f90'
]

test_src = [
    test_dir/'prime_test.f90',
    test_dir/'multiprecision_test.f90',
    test_dir/'utility_test.f90'
]

prob_src = []
foreach problem_number : problem_numbers
    prob_src += euler_dir/'problem_' + problem_number + '.f90'
endforeach

# Data
data_dir = 'data'
data_src = [
    'data_0008.txt', 
    'data_0011.txt', 
    'data_0013.txt', 
    'data_0018.txt',
    'data_0022.txt',
    'data_0042.txt',
    'data_0054.txt',
    'data_0059.txt'
]
foreach data_file : data_src
    configure_file(copy:true, input: data_dir/data_file, output: data_file)
endforeach

# Executables
src_codes = [util_src, fypp_src, prob_src, app_src]
pe_fortran = executable(
    'PE-Fortran', 
    sources: src_codes, 
    install: true,
    dependencies: fortran_stdlib
)

# test_prime_m = executable(
#     'Test-prime', 
#     sources: [util_src, fypp_src, test_src[0]],
#     dependencies: fortran_stdlib
# )

# test_multiprecision_m = executable(
#     'Test-multiprecision',  
#     sources: [util_src, fypp_src, test_src[1]],
#     dependencies: fortran_stdlib
# )

# test_utility_m = executable(
#     'Test-utility', 
#     sources: [util_src, fypp_src, test_src[2]],
#     dependencies: fortran_stdlib
# )

# test('Test multiprecision module', test_multiprecision_m)
# test('Test prime module', test_prime_m)
# test('Test utility module', test_utility_m)

argument_list = ['--fancy', '--all', nproblems.to_string()]
test_name = 'Test all problems available '
test(
    test_name + '(@0@ problems)'.format(nproblems), 
    pe_fortran, args : argument_list
)