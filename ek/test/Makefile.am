TESTS  = kvs_sys_tests
TESTS += kvs_bucket_tests
TESTS += kvs_cache_tests
TESTS += kvs_evt_tests
TESTS += kvs_rrd_tests
TESTS += kvs_evt_log_tests


.SECONDARY:

% : %.beam
	echo "erl -pa ../ebin -eval 'R = case eunit:test($@, [verbose]) of ok -> 0; _ -> 1 end, timer:apply_after(100, erlang, halt, [R]).'" > $@
	chmod u+x $@ 

%.beam : %.erl
	$(ERLC) $^

CLEANFILES = $(TESTS) $(TESTS:=.beam)

