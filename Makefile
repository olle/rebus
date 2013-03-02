REBAR=`which rebar`

compile:
	@${REBAR} compile

test: compile
	@${REBAR} eunit

clean:
	@rm -rf ebin/
