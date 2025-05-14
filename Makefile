target:
	gfortran -fPIC -shared -o clifford-sim/lib/libchp.so chp-fortran/src/chp.f90
	cp clifford-sim/lib/libchp.so build-c
