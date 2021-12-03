project('PE-Fortran', 'fortran', 
    version: '0.0.1', 
    meson_version: '>=0.51',
    license: 'MIT',
    default_options: [
        'buildtype=release', 
        'fortran_std=f2018'
    ]
)

# Number of problems
num_probs = 26

# Directories
src_dir = 'src'
app_dir = 'src/app'
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
fypp_flag = '-DNUM_PROB=@0@'.format(num_probs)
message('fypp_flag = @0@'.format(fypp_flag))
gen_f90 = generator(
    fypp, output: '@BASENAME@.f90',
    arguments: ['@INPUT@', '@EXTRA_ARGS@', '@OUTPUT@']
)
fypp_files = [
    src_dir/'prime_m.fpp',
    src_dir/'interface_m.fpp', 
    src_dir/'euler_problems_m.fpp',
    src_dir/'utility_m.fpp'
]
fypp_src = gen_f90.process(fypp_files, extra_args: fypp_flag)

# Source files
util_src = [
    src_dir/'constant_m.f90',
    src_dir/'multiprecision_m.f90',
    src_dir/'data_m.f90',
    src_dir/'driver_m.f90'
]

app_src = [
    app_dir/'project_euler.f90'
]

# Meson doesn't have wildcards, instead one has to use extra commands if 
# they have many sources files. For more details, consult:
# https://github.com/mesonbuild/meson/blob/master/docs/markdown/FAQ.md

r = run_command('python', 'problem_ID.py', num_probs.to_string())
prob_num = r.stdout().split()

prob_src = []
foreach prob : prob_num
    prob_src += euler_dir/'problem_' + prob + '_m.f90'
endforeach

src = [util_src, fypp_src, prob_src, app_src]
exec = executable('PE-Fortran', sources: src)
test(
    'Compute all problems available', 
    exec, args : ['--fancy', '--all', num_probs.to_string()]
)