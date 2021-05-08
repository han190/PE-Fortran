project('PE-Fortran', 'fortran', 
    version: '0.0.1', 
    meson_version: '>=0.51',
    license: 'MIT',
    default_options: [
        'buildtype=release', 
        'fortran_std=f2018'
    ]
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
        dialect = ['-ffast-math']
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

num_probs = 60
fypp = find_program('fypp')
fypp_flag = '-DNUM_PROB=@0@'.format(num_probs)
message('fypp_flag = @0@'.format(fypp_flag))
gen_f90 = generator(
    fypp, output: '@BASENAME@.f90',
    arguments: ['@INPUT@', '@EXTRA_ARGS@', '@OUTPUT@']
)
fypp_src = gen_f90.process(
    fypp_dir/'euler_interface_m.fypp', 
    fypp_dir/'euler_prob_api_m.fypp', 
    extra_args: fypp_flag
)

util_src = [
    util_dir/'euler_utils_m.f90', util_dir/'euler_var_arr_m.f90',
    util_dir/'euler_primes_m.f90', util_dir/'euler_multiprecision_m.f90'
]

main_src = [main_dir/'euler_main_m.f90', main_dir/'euler_main.f90']
prob_num = [
    '0001', '0002', '0003', '0004', '0005', '0006', '0007', '0008', '0009', 
    '0010', '0011', '0012', '0013', '0014', '0015', '0016', '0017', '0018', 
    '0019', '0020', '0021', '0022', '0023', '0024', '0025', '0026', '0027', 
    '0028', '0029', '0030', '0031', '0032', '0033', '0034', '0035', '0036', 
    '0037', '0038', '0039', '0040', '0041', '0042', '0043', '0044', '0045', 
    '0046', '0047', '0048', '0049', '0050', '0051', '0052', '0053', '0054', 
    '0055', '0056', '0057', '0058', '0059', '0060'
]
prob_src = []
foreach prob : prob_num
    prob_src += prob_dir/'euler_interface_m@euler_prob_' + prob + '_m.f90'
endforeach

data_src = [
    'euler0008.txt', 'euler0011.txt', 'euler0013.txt', 'euler0018.txt',
    'euler0022.txt', 'euler0042.txt', 'euler0054.txt', 'euler0059.txt'
]
foreach data_file : data_src
    configure_file(copy:true, input: data_dir/data_file, output: data_file)
endforeach

src = [util_src, fypp_src, prob_src, main_src]
exec = executable('PE-Fortran', sources: src)
test('Compute All Problems', exec, args: '--compute-all')