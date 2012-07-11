
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar  
#REBAR=./rebar -v 




all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) skip_deps=true doc

tests:
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build-plt

check_plt:
	@$(REBAR) check-plt

dialyzer:
	dialyzer -n -nn -pa ../xapian/ebin -pa deps/seqbind/ebin/ \
					-pa deps/erlando/ebin/ -pa deps/parse_trans/ebin/ \
					--src src/ deps/erlando/src/ deps/poolboy/src/ 

force-compile:
	@$(REBAR) clean update-deps get-deps compile

cover:
	export XAPIAN_REBAR_COVER="true"; $(REBAR) skip_deps=true clean compile eunit
	rm -rf c_cov
	mkdir c_cov
	rm *.gcov || echo "First run."
	gcov -p -o c_src/common/ c_src/common/*.cpp
	mkdir c_cov/common
	mv c_src*.gcov c_cov/common
	gcov -p -o c_src/driver/ c_src/driver/*.cpp
	mkdir c_cov/driver
	mv c_src*.gcov c_cov/driver
	gcov -p -o c_src/port/ c_src/port/*.cpp
	mkdir c_cov/port
	mv c_src*.gcov c_cov/port
	rm *.gcov || echo "No library files."
