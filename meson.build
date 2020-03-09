project('Project Euler with Modern Fortran', 
    'fortran', 
    default_options: 'buildtype=release',
    license: 'MIT',
    meson_version: '>=0.53',
    version: '0.0.1'
)

build_type = get_option('buildtype')
fc = meson.get_compiler('fortran')

if build_type == 'debug'
    supported_arguments = fc.get_supported_arguments([
        '-g', '-Wall', '-Wextra',
        '-Warray-temporaries',
        '-Wconversion',
        '-fimplicit-none',
        '-fbacktrace',
        '-ffree-line-length-0',
        '-fcheck=all',
        '-ffpe-trap=zero,overflow,underflow',
        '-finit-real=nan'
    ])
elif build_type == 'release'
    supported_arguments = fc.get_supported_arguments([
        '-pthread', '-w'
    ])
endif

add_project_arguments(supported_arguments, language: 'fortran')

utils = [
    'utils/euler_utils.f90',
    'utils/euler_lexical_sort.f90',
    'utils/euler_mi.f90',
    'utils/euler_poker.f90',
    'utils/euler_primes.f90',
    'utils/euler_var_arr.f90'
]

euler = [
    'src/euler_interface.f90',
    'src/euler_main.f90',
    'src/euler_prob_api.f90'
]

probs = [
    'probs/euler_prob_0001.f90',
    'probs/euler_prob_0002.f90',
    'probs/euler_prob_0003.f90',
    'probs/euler_prob_0004.f90',
    'probs/euler_prob_0005.f90',
    'probs/euler_prob_0006.f90',
    'probs/euler_prob_0007.f90',
    'probs/euler_prob_0008.f90',
    'probs/euler_prob_0009.f90',
    'probs/euler_prob_0010.f90',
    'probs/euler_prob_0011.f90',
    'probs/euler_prob_0012.f90',
    'probs/euler_prob_0013.f90',
    'probs/euler_prob_0014.f90',
    'probs/euler_prob_0015.f90',
    'probs/euler_prob_0016.f90',
    'probs/euler_prob_0017.f90',
    'probs/euler_prob_0018.f90',
    'probs/euler_prob_0019.f90',
    'probs/euler_prob_0020.f90',
    'probs/euler_prob_0021.f90',
    'probs/euler_prob_0022.f90',
    'probs/euler_prob_0023.f90',
    'probs/euler_prob_0024.f90',
    'probs/euler_prob_0025.f90',
    'probs/euler_prob_0026.f90',
    'probs/euler_prob_0027.f90',
    'probs/euler_prob_0028.f90',
    'probs/euler_prob_0029.f90',
    'probs/euler_prob_0030.f90',
    'probs/euler_prob_0031.f90',
    'probs/euler_prob_0032.f90',
    'probs/euler_prob_0033.f90',
    'probs/euler_prob_0034.f90',
    'probs/euler_prob_0035.f90',
    'probs/euler_prob_0036.f90',
    'probs/euler_prob_0037.f90',
    'probs/euler_prob_0038.f90',
    'probs/euler_prob_0039.f90',
    'probs/euler_prob_0040.f90',
    'probs/euler_prob_0041.f90',
    'probs/euler_prob_0042.f90',
    'probs/euler_prob_0043.f90',
    'probs/euler_prob_0044.f90',
    'probs/euler_prob_0045.f90',
    'probs/euler_prob_0046.f90',
    'probs/euler_prob_0047.f90',
    'probs/euler_prob_0048.f90',
    'probs/euler_prob_0049.f90',
    'probs/euler_prob_0050.f90',
    'probs/euler_prob_0051.f90',
    'probs/euler_prob_0052.f90',
    'probs/euler_prob_0053.f90',
    'probs/euler_prob_0054.f90',
    'probs/euler_prob_0055.f90',
    'probs/euler_prob_0056.f90',
    'probs/euler_prob_0057.f90',
    'probs/euler_prob_0058.f90'
]

data = [
    'data/euler0008.txt',
    'data/euler0011.txt',
    'data/euler0013.txt',
    'data/euler0018.txt',
    'data/euler0022.txt',
    'data/euler0042.txt',
    'data/euler0054.txt'
]

# until version 0.53
fs = import('fs')
foreach i : data
    configure_file(copy: true, input: i, output: fs.name(i))
endforeach

executable('euler_test_mi', 'tests/test_euler_mi.f90', utils)
executable('euler_test_utils', 'tests/test_euler_utils.f90', utils)
executable('euler', utils, euler, probs)
