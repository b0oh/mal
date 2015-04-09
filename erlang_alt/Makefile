ERL=erl

all: compile

compile:
	@$(ERL) -make

repl: compile
	@$(ERL) -pa ebin

check: compile
	@dialyzer ebin

clean:
	@rm ebin/*.beam
