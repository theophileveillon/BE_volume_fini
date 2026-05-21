FC = gfortran
OPT = -g -O0 -fbounds-check
DOSSIER = rendu_paraview
OBJ = m_type.o prog.o sousprog.o VTSWriter.o
MODE_LIST = classique advection_pure_verticale advection_pure_horizontale advection_pure_verticale_CLF diffusion_pure_horizontale diffusion_pure_horizontale_R diffusion_pure_verticale diffusion_pure_verticale_R peclet_stationnaire convergence_maillage peclet_diffusion peclet_advection peclet_finale
MODE ?= classique

run : clean prog.exe
	mkdir -p $(DOSSIER)
	./prog.exe $(DOSSIER) "$(MODE_LIST)" $(MODE);
	python3 plot_courbe.py $(MODE);

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

