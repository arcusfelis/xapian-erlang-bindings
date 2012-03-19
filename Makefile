
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
	@$(REBAR) dialyze

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)


