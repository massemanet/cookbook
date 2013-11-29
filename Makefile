REBAR = ../rebar/rebar

.PHONY: all clean

all:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

cnode: cnode.c
	gcc cnode.c \
	-o cnode \
	-I/usr/local/otp/lib/erl_interface-3.2.1/include \
	-L/usr/local/otp/lib/erl_interface-3.2.1/lib \
	-lerl_interface -lei -lsocket -lnsl
