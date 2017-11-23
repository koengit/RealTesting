.PHONY: all clean
.DEFAULT: all

all: $(patsubst %.c, %.o, $(wildcard *.c))

clean:
	rm -f *.o

%.o: %.c *.h Makefile
	gcc -O2 -march=native $< -c


MATLAB=rtw_continuous.h rtw_solver.h

define message =
The files rtw_continuous.h and rts_solver.h are missing.
You need to get them from a Matlab installation and put them in
this directory ($(PWD)).

You can find them in the simulink/include directory of your Matlab
installation. On the Chalmers remote machine, you can find them in
  /chalmers/sw/sup64/matlab-2016b/simulink/include)
endef

ifneq ($(wildcard $(MATLAB)),$(MATLAB))
$(error $(message))
endif
