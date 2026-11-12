#!/bin/sh

REL="$(dirname "$0")"
ABS="$(cd "$REL"; pwd)"

if [ -f ".mu" ]; then
    . "$(pwd)/.mu"
fi

if [ -z "$GAMBITDIR" ]; then
    GSC="gsc"
else
    GSC="$GAMBITDIR/bin/gsc"
    if [ ! -f "$GSC" ]; then
        echo "Unable to find $GAMBITDIR/bin/gsc"
        exit 1
    fi
fi

SCM="$REL/kernel/mu.scm"

display_module_info () {
    echo "$1"
    if [ -d "$2" ]; then
        cd "$2" &&
        git log -1
    fi
}

display_info () {
    (display_module_info "Zen" "$REL")
}

case "$1" in
    "info" )
        display_info
        exit
        ;;

    "environment" )
        echo "REL: $REL"
        echo "ABS: $ABS"
        echo "GAMBITDIR: $GAMBITDIR"
        echo "GSC: $GSC"
        echo "SCM: $SCM"
        exit
        ;;
esac

MU_EXE="$REL/bin/mu"
MU_LINK="mu_"
MU_C="core.c version.c manifest.c boot.c configuration.c build.c"
MU_OBJ="core.o version.o manifest.o boot.o configuration.o build.o mu_.o"

if [ ! -f "$MU_EXE" ]; then
  echo "build mu"
  mkdir -p $REL/bin
  cd $REL/bin
  gsc -o . -c ../kernel/base/core.scm
  gsc -o . -c ../kernel/base/version.scm
  gsc -o . -c ../kernel/base/manifest.scm
  gsc -o . -c ../kernel/mu/boot.scm
  gsc -o . -c ../kernel/mu/configuration.scm
  gsc -o . -c ../kernel/mu/build.scm
  gsc -link -o $MU_LINK.c -l ~~lib/_gambitgsc $MU_C
  gsc -obj $MU_C $MU_LINK.c
  gcc -o mu $MU_OBJ -L/usr/local/Gambit/lib -lgambit -lgambitgsc
fi
exit

if [ -z "$GAMBITDIR" ]; then
    exec "$GSC" -i "$SCM" "$@"
else
    exec "$GSC" -:dar -i "$SCM" "$@"
fi
