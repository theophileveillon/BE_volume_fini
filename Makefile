FC = gfortran
OPT = -g -O0 -fbounds-check
DOSSIER = test
OBJ = m_type.o prog.o sousprog.o VTSWriter.o

run : clean prog.exe
	mkdir -p $(DOSSIER)
	./prog.exe $(DOSSIER)
prog.exe :	$(OBJ)
	$(FC) $(OPT) $(OBJ) -o prog.exe

m_type.o :	m_type.f90
	$(FC) $(OPT) m_type.f90 -c

prog.o :	sousprog.o prog.f90
	$(FC) $(OPT) prog.f90 -c

sousprog.o :	sousprog.f90
	$(FC) $(OPT) sousprog.f90 -c

VTSWriter.o :	VTSWriter.f90
	$(FC) $(OPT) VTSWriter.f90 -c

clean :
	/bin/rm -f $(OBJ) *.mod *.exe *.o *.vts *.pvd $(DOSSIER)/*.vts $(DOSSIER)/*.pvd

