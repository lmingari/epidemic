SHELL = /bin/sh

FC = gfortran
FFLAGS = -fopenmp #-O3 #-Wall
NETCDF = /usr
INC = -I$(NETCDF)/include
LDFLAGS = -L$(NETCDF)/lib -lnetcdf -lnetcdff

TARGET = sir.x
OBJS = main.o mod_KindType.o mod_Shared.o mod_InpOut.o mod_Model.o

.SUFFIXES: .f90 .o

.f90.o:
	$(FC) $(FFLAGS) $(INC) -c $<

%.o: %.mod

$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $^ 

.PHONY: clean
clean:
	@rm -rf *.o *.mod 

# Add Dependencies here
main.o :       mod_KindType.o mod_Shared.o mod_InpOut.o mod_Model.o
mod_Shared.o : mod_KindType.o
mod_InpOut.o : mod_KindType.o mod_Shared.o
mod_Model.o  : mod_KindType.o mod_Shared.o
