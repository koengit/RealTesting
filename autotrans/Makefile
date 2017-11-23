.PHONY: all clean
.DEFAULT: all

all: AutoTransModel.hs autotrans.o

clean:
	rm -f *.o AutoTransModel.hs

OBJS = \
	Autotrans_shift_data.o Autotrans_shift.o ert_main.o rtGetInf.o \
	rtGetNaN.o rt_look1d.o rt_look2d_normal.o rt_look.o rt_nonfinite.o

autotrans.o: $(OBJS)
	ld -r -o $@ $(OBJS)

%.o: %.c *.h Makefile
	gcc -O2 -march=native $< -c

%.hs: %.hsc *.h
	hsc2hs -o $@ $<

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
