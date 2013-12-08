REBAR = ../rebar/rebar

.PHONY: all clean

all:
	@$(REBAR) compile

clean:
	@find . -name "*~" -exec rm {} \;
	@find . -name "*.o" -exec rm {} \;
	@$(REBAR) clean

.PHONY: cnog
cnog : bin/cnog
c_src/%.o : c_src/%.c
	gcc -c -g -Wall -fPIC  -I/usr/lib/erlang/lib/erl_interface-3.7.13/include -I/usr/lib/erlang/erts-5.10.3/include $< -o $@
bin/cnog : c_src/cnog.o c_src/cnog_marshal.o c_src/cnog_math.o
	gcc c_src/cnog.o c_src/cnog_marshal.o c_src/cnog_math.o -lpthread -rdynamic -ldl  -L/usr/lib/erlang/lib/erl_interface-3.7.13/lib -lerl_interface -lei -lm -o $@

cnode: cnode.c
	gcc cnode.c \
	-o cnode \
	-I/usr/local/otp/lib/erl_interface-3.2.1/include \
	-L/usr/local/otp/lib/erl_interface-3.2.1/lib \
	-lerl_interface -lei -lsocket -lnsl
