project('PE-Fortran', 'fortran', 
    version: '0.0.1', 
    meson_version: '>=0.51',
    license: 'MIT',
    default_options: [
        'buildtype=release', 
        'warning_level=0', 
        'optimization=3',
        'auto_features=disabled']
)

# check os
if host_machine.system() == 'windows'
    fypp_dir = 'src\\fypp'
    util_dir = 'src\\util'
    prob_dir = 'src\\prob'
    main_dir = 'src\\main'
elif host_machine.system() == 'linux'
    fypp_dir = 'src/fypp'
    util_dir = 'src/util'
    prob_dir = 'src/prob'
    main_dir = 'src/main'
endif
data_dir = 'data'

# check compiler
fc = meson.get_compiler('fortran')
if fc.get_id() == 'gcc'
    if get_option('buildtype') == 'release'
        dialect = ['-O3', '-ffast-math', '-std=f2018']
    elif get_option('buildtype') == 'debug'
        dialect = ['-Wall', '-Wextra', '-fcheck=all', '-fbacktrace']
    endif
elif fc.get_id() == 'intel'
    if get_option('buildtype') == 'release'
        dialect = ['-O3', '-xHost', '-ipo']
    elif get_option('buildtype') == 'debug'
        dialect = []
    endif
endif
add_global_arguments(dialect, language: 'fortran')

fypp = find_program('fypp')
num_probs = 60
fypp_flag = '-DNUM_PROB=@0@'.format(num_probs)

gen = generator(fypp,
    output: '@BASENAME@.f90',
    arguments: ['@INPUT@', '@EXTRA_ARGS@', '@OUTPUT@'])
fypp_src = gen.process(fypp_dir / 'euler_interface_m.fypp', 
    fypp_dir / 'euler_prob_api_m.fypp', extra_args: fypp_flag)

util_src = [
    util_dir / 'euler_utils_m.f90', util_dir / 'euler_var_arr_m.f90',
    util_dir / 'euler_primes_m.f90', util_dir / 'euler_mi_m.f90'
]

main_src = [main_dir / 'euler_main.f90']

prob_src = [
    prob_dir / 'euler_interface_m@euler_prob_0001_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0002_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0003_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0004_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0005_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0006_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0007_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0008_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0009_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0010_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0011_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0012_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0013_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0014_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0015_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0016_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0017_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0018_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0019_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0020_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0021_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0022_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0023_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0024_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0025_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0026_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0027_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0028_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0029_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0030_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0031_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0032_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0033_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0034_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0035_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0036_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0037_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0038_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0039_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0040_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0041_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0042_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0043_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0044_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0045_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0046_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0047_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0048_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0049_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0050_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0051_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0052_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0053_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0054_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0055_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0056_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0057_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0058_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0059_m.f90',
    prob_dir / 'euler_interface_m@euler_prob_0060_m.f90'
]

data_src = [
    'euler0008.txt', 'euler0011.txt', 'euler0013.txt', 'euler0018.txt',
    'euler0022.txt', 'euler0042.txt', 'euler0054.txt', 'euler0059.txt'
]

foreach data_file : data_src
    configure_file(copy:true,
        input: data_dir / data_file, output: data_file)
endforeach

executable('PE-Fortran', sources: [util_src, fypp_src, prob_src, main_src])