TESTS  = job_uri_tests
TESTS += job_script_tests

.SECONDARY:

% : %.beam
	echo "erl -pa ../ebin -eval 'R = case eunit:test($@, [verbose]) of ok -> 0; _ -> 1 end, timer:apply_after(100, erlang, halt, [R]).'" > $@
	chmod u+x $@ 

%.beam : %.erl
	$(ERLC) $^

CLEANFILES = $(TESTS) $(TESTS:=.beam)

