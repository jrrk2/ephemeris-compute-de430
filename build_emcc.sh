emcc -Wall -Wno-unknown-pragmas -O3 -I /Users/jonathan/ephemeris-compute-de430/src -I gsl-2.8 -D DEBUG=0 \
     -D MEMDEBUG1=0 -D MEMDEBUG2=0 -D DCFVERSION=\"4.0\" -D DATE=\"26/11/2024\" -D PATHLINK=\"/\" \
     -D SRCDIR=\"/src/\" src/argparse/argparse.c src/coreUtils/asciiDouble.c src/coreUtils/errorReport.c \
     src/coreUtils/makeRasters.c src/ephemCalc/magnitudeEstimate.c src/ephemCalc/jpl.c \
     src/ephemCalc/orbitalElements.c src/listTools/ltDict.c src/listTools/ltList.c src/listTools/ltMemory.c \
     src/listTools/ltStringProc.c src/mathsTools/julianDate.c src/mathsTools/precess_equinoxes.c \
     src/mathsTools/sphericalAst.c src/settings/settings.c src/main.c \
     gsl-2.8/sys/infnan.c gsl-2.8/sys/pow_int.c gsl-2.8/sys/hypot.c gsl-2.8/sys/fdiv.c \
     gsl-2.8/err/error.c gsl-2.8/err/stream.c \
     -lm --preload-file data/header.430@/data/ \
     --preload-file data/ascp1950.430@/data/ --preload-file data/dcfbinary.430@/data/ \
     -s ALLOW_MEMORY_GROWTH=1 -o ephem_program.js
