#!/usr/bin/env bash

srcdir=src
datadir=data
srcfpmdir=src-fpm
nproblem=$(ls ./$srcdir/euler/*.f90 | wc -l)
ntrails=1
fypp_flag=-DNUM_PROB=$nproblem
profile=release
FC=gfortran
generate_source_only=false
build_tool=fpm

help_message() {
    echo "PE-Fortran Installation Script"
    echo "Quick Start:"
    echo "    $ ./install.sh"
    echo
    echo "Available Arguments:"
    echo "    -h,  --help                  Pop up help message."
    echo "    -b,  --build-tool            meson/fpm (default: fpm)"
    echo "    -c,  --compiler              gfortran/ifort (default: gfortran)"
    echo "    -p,  --profile               release/debug (default: release)"
    echo "    -g,  --generate-source-only  Generate source code using fypp. "
    echo "    -np, --number-of-problems    (default: $nproblem)"
    echo "    -nt, --number-of-trails      (default: 1)"
    echo
    echo "Example:"
    echo "    $ ./install.sh -b meson -c ifort -p debug"
    echo
}

check_dependency() {
    executable=$1
    if ! command -v $executable &>/dev/null; then
        msg="[ERROR] $executable not found, \
            try: conda install $executable."
        echo $msg
        exit 0
    fi
}

while true; do
    case "$1" in
    -h | --help)
        help_message
        exit 1
        ;;
    -c | --compiler)
        FC=$2
        shift 2
        ;;
    -b | --build-tool)
        build_tool=$2
        shift 2
        ;;
    -p | --profile)
        profile=$2
        shift 2
        ;;
    -g | --generate-source-only)
        generate_source_only=true
        shift 1
        ;;
    -np | --number-of-problems)
        nproblem=$2
        shift 2
        ;;
    -nt | --number-of-trails)
        ntrails=$2
        shift 2
        ;;
    *)
        break
        ;;
    esac
done

if [ $generate_source_only == "true" ]; then
    check_dependency fypp
elif [ $build_tool == "fpm" ]; then
    check_dependency fypp
    check_dependency fpm
elif [ $build_tool == "meson" ]; then
    check_dependency meson
    check_dependency fypp
    check_dependency ninja
else
    echo "[ERROR] Invalid build tool option."
fi

if [ -d "src" ]; then

    if [ $build_tool == "fpm" ]; then

        if [ -d "src-fpm" ]; then
            echo "'src-fpm' already exists, deleting it..."
            rm -rf src-fpm
        fi
        echo "Creating folder 'src-fpm'..."
        mkdir -p src-fpm

        if [ -d "build" ]; then
            echo "'build' already exists, deleting it..."
            rm -rf build
        fi
        echo "Creating folder 'build'..."
        mkdir -p build

        for f in $srcdir/*.f90; do
            filename=$(basename -- "$f")
            echo "Copying ${filename}..."
            cp $srcdir/$filename $srcfpmdir/$filename
        done

        for f in $srcdir/*.fypp; do
            filename=$(basename -- "$f")
            extension="${filename##*.}"
            filename="${filename%.*}"
            echo "Generating ${filename} through fypp..."
            fypp "${srcdir}/${filename}.fypp" \
                "${srcfpmdir}/${filename}.f90" $fypp_flag
        done

        for d in $srcdir/*/; do
            foldername=$(basename -- "$d")
            echo "Copying folder ${foldername}..."
            cp -rf $srcdir/$foldername $srcfpmdir/$foldername
        done

        if command -v fprettify &>/dev/null; then
            echo "Found fprettify, formatting source codes..."
            fprettify -i=2 --strict-indent --disable-indent-mod -r $srcfpmdir
        else
            echo "fprettify not found."
        fi

        if [ $generate_source_only == true ]; then
            exit 0
        fi

        $build_tool build --compiler $FC --profile $profile
        $build_tool test --compiler $FC --profile $profile
        $build_tool run --compiler $FC --profile $profile -- --version
        $build_tool run --compiler $FC --profile $profile -- \
            --fancy --all $nproblem \
            --number-of-trails $ntrails \
            --data-directory $datadir
        $build_tool install --compiler $FC --profile $profile

    elif [ $build_tool == "meson" ]; then

        builddir=build-meson
        if [ -d "$builddir" ]; then
            echo "'$builddir' already exists, deleting it..."
            rm -rf $builddir
        fi
        echo "Creating folder '$builddir'..."
        mkdir -p $builddir

        FC=$FC $build_tool $builddir --buildtype=$profile
        FC=$FC $build_tool configure --prefix=$HOME/.local/ $builddir
        FC=$FC $build_tool test -C $builddir # optional
        FC=$FC $build_tool install -C $builddir

    fi

fi

if [ $? -eq 0 ]; then
    echo "Project built successfully."
fi
