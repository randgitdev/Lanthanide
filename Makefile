
lanthanide:*.o
	gfortran -o lanthanide *.o

*.o:*.fpp
	gfortran -O3
