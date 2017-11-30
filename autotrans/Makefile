.PHONY: all clean auto heater afc

all: AutoTransModel.hs HeaterModel.hs AFCModel.hs autotrans.o heater.o afc.o

clean:
	rm -f *.o *.hi AutoTransModel.hs HeaterModel.hs

auto: all
	ghci AutoTrans -i.. autotrans.o -O -fobject-code

heater: all
	ghci HeaterQC -i.. heater.o -O -fobject-code

afc: all
	ghci AFCModel -i.. afc.o -O -fobject-code

AUTO_OBJS = \
	Autotrans_shift_data.o Autotrans_shift.o rtGetInf.o \
	rtGetNaN.o rt_look1d.o rt_look2d_normal.o rt_look.o rt_nonfinite.o

HEATER_OBJS = \
	Heater.o Heater_data.o

AFC_OBJS = \
	AbstractFuelControl_M1.o AbstractFuelControl_M1_data.o \
	rtGetInf.o rtGetNaN.o rt_nonfinite.o rt_zcfcn.o

autotrans.o: $(AUTO_OBJS)
	ld -r -o $@ $(AUTO_OBJS)

heater.o: $(HEATER_OBJS)
	ld -r -o $@ $(HEATER_OBJS)

afc.o: $(AFC_OBJS)
	ld -r -o $@ $(AFC_OBJS)

%.o: %.c *.h Makefile
	gcc -O2 -march=native $< -c

%.hs: %.hsc *.h
	hsc2hs -o $@ $<

MATLAB=rtw_continuous.h rtw_solver.h solver_zc.h

define message =
The files rtw_continuous.h, rts_solver.h and solver_zc.h are missing.
You need to get them from a Matlab installation and put them in
this directory ($(PWD)).

You can find them in the simulink/include directory of your Matlab
installation. On the Chalmers remote machine, you can find them in
  /chalmers/sw/sup64/matlab-2016b/simulink/include)
endef

ifneq ($(wildcard $(MATLAB)),$(MATLAB))
$(error $(message))
endif
