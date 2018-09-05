.DEFAULT: all
.PHONY: all

all clean:
	make -C simulink $@
	make -C flowstar $@
