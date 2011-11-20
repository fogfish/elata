TESTS  = ek_uri_tests
TESTS += ek_reg_tests
TESTS += ek_tests
TESTS += ek_msg_tests

.SECONDARY:

% : %.beam
	echo "erl -sname dev@localhost -pa ../ebin -eval 'R = case eunit:test($@, [verbose]) of ok -> 0; _ -> 1 end, timer:apply_after(100, erlang, halt, [R]).'" > $@
	chmod u+x $@ 

%.beam : %.erl
	$(ERLC) $^

CLEANFILES = $(TESTS) $(TESTS:=.beam)

