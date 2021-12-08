project('PE-Fortran', 'fortran', 
    version: '0.0.1', 
    meson_version: '>=0.51',
    license: 'MIT',
    default_options: [
        'buildtype=release', 
        'fortran_std=f2018'
    ]
)

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

# Check compiler
fc = meson.get_compiler('fortran')
if fc.get_id() == 'gcc'
    if get_option('buildtype') == 'release'
        dialect = ['-Ofast']
    elif get_option('buildtype') == 'debug'
        dialect = ['-fcheck=all', '-fbacktrace']
    endif
elif fc.get_id() == 'intel'
    if get_option('buildtype') == 'release'
        dialect = ['-xHost', '-ipo']
    elif get_option('buildtype') == 'debug'
        dialect = ['-warn', '-debug extended', '-fpe0']
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
    src_dir/'prime.fpp',
    src_dir/'interface.fpp', 
    src_dir/'euler_problems.fpp',
    src_dir/'utility.fpp'
]
fypp_src = gen_f90.process(fypp_files, extra_args: fypp_flag)

# Source files
util_src = [
    src_dir/'constant.f90',
    src_dir/'multiprecision.f90',
    src_dir/'driver.f90'
]

app_src = [
    src_dir/'project_euler.f90'
]

test_src = [
    test_dir/'test_multiprecision.f90',
    test_dir/'test_prime.f90',
    test_dir/'test_utility.f90'
]

prob_src = []
foreach problem_number : problem_numbers
    prob_src += euler_dir/'problem_' + problem_number + '.f90'
endforeach

data_dir = 'data'
data_src = [
    'data_0008.txt', 'data_0011.txt', 'data_0013.txt', 'data_0018.txt',
    'data_0022.txt', 'data_0042.txt', 'data_0054.txt', 'data_0059.txt'
]
foreach data_file : data_src
    configure_file(copy:true, input: data_dir/data_file, output: data_file)
endforeach


inc_dir = include_directories('./data')
# message(inc_dir)
# Tests and executables.
src = [util_src, fypp_src, prob_src]
exec = executable('PE-Fortran', sources: [src, app_src], include_directories: inc_dir)
test_multiprecision_m = executable('Test-multiprecision_m',  sources: [src, test_src[0]])
test_prime_m = executable('Test-prime_m', sources: [src, test_src[1]])
test_utility_m = executable('Test-utility_m', sources: [src, test_src[2]])

test('Test multiprecision module', test_multiprecision_m)
test('Test prime module', test_prime_m)
test('Test utility module', test_utility_m)

argument_list = ['--fancy', '--all', nproblems.to_string()]
test_name = 'Test all problems available '
test(test_name + '(@0@ problems)'.format(nproblems), exec, args : argument_list)