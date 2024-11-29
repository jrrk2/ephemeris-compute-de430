emcc -Wall -Wno-unknown-pragmas -O3 -I /Users/jonathan/ephemeris-compute-de430/src -I gsl-2.8/include -D DEBUG=0 \
     -D MEMDEBUG1=0 -D MEMDEBUG2=0 -D DCFVERSION=\"4.0\" -D DATE=\"26/11/2024\" -D PATHLINK=\"/\" \
     -D SRCDIR=\"/src/\" src/argparse/argparse.c src/coreUtils/asciiDouble.c src/coreUtils/errorReport.c \
     src/coreUtils/makeRasters.c src/ephemCalc/magnitudeEstimate.c src/ephemCalc/jpl.c \
     src/ephemCalc/orbitalElements.c src/listTools/ltDict.c src/listTools/ltList.c src/listTools/ltMemory.c \
     src/listTools/ltStringProc.c src/mathsTools/julianDate.c src/mathsTools/precess_equinoxes.c \
     src/mathsTools/sphericalAst.c src/settings/settings.c src/main.c gsl-2.8/.libs/libgsl.a \
     gsl-2.8/cblas/.libs/libgslcblas.a -lm --preload-file data/header.430@/data/ \
     --preload-file data/ascp1950.430@/data/ --preload-file data/dcfbinary.430@/data/ \
     -s ALLOW_MEMORY_GROWTH=1 -o ephem_program.js
