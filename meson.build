project('PE-Fortran', 'fortran', 
    version: '0.0.1', 
    meson_version: '>=0.51',
    license: 'MIT',
    default_options: [
        'buildtype=release', 
        'fortran_std=f2018'
    ]
)

# Directories
SRC_DIR = 'src'

# Check compiler
fc = meson.get_compiler('fortran')
if fc.get_id() == 'gcc'
    if get_option('buildtype') == 'release'
        dialect = ['-ffast-math']
    elif get_option('buildtype') == 'debug'
        dialect = ['-fcheck=all', '-fbacktrace']
    endif
elif fc.get_id() == 'intel'
    if get_option('buildtype') == 'release'
        dialect = ['-xHost', '-ipo']
    elif get_option('buildtype') == 'debug'
        dialect = ['-check', '-traceback', '-fpe0']
    endif
endif
add_global_arguments(dialect, language: 'fortran')

# Source files
src = [
    SRC_DIR/'euler_utils_m.f90', 
    SRC_DIR/'euler_var_arr_m.f90',
    SRC_DIR/'euler_primes_m.f90', 
    SRC_DIR/'euler_multiprecision_m.f90',
    SRC_DIR/'euler_interface_m.f90', 
    SRC_DIR/'euler_prob_api_m.f90', 
    SRC_DIR/'euler_main_m.f90', 
    SRC_DIR/'euler_main.f90'
]

# Meson doesn't have wildcards, instead one has to use extra commands if 
# they have many sources files. For more details, consult:
# https://github.com/mesonbuild/meson/blob/master/docs/markdown/FAQ.md

num_probs = 61
prob_num = run_command(
    'python', 'numbers.py', num_probs.to_string()
).stdout().split()

foreach prob : prob_num
    src += SRC_DIR/'prob/euler_prob_' + prob + '_m.f90'
endforeach

data_src = [
    'euler0008.txt',
    'euler0011.txt',
    'euler0013.txt',
    'euler0018.txt',
    'euler0022.txt',
    'euler0042.txt',
    'euler0054.txt',
    'euler0059.txt'
]

foreach data_file : data_src
    configure_file(
        copy:true, input: 'data'/data_file, 
        output: data_file
    )
endforeach

exec = executable('PE-Fortran', sources: src)
test('Compute All Problems', exec, args: '--compute-all')