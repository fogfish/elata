TESTS  = kvs_sys_tests
TESTS += kvs_evt_tests
TESTS += kvs_ets_tests
TESTS += kvs_dets_tests
TESTS += kvs_cache_tests
TESTS += kvs_rrd_tests
TESTS += kvs_ht_tests


.SECONDARY:

% : %.beam
	echo "erl -sname dev@localhost -pa ../ebin -pa ../../*/ebin -eval 'R = case eunit:test($@, [verbose]) of ok -> 0; _ -> 1 end, timer:apply_after(100, erlang, halt, [R]).'" > $@
	chmod u+x $@ 

%.beam : %.erl
	$(ERLC) $^

CLEANFILES = $(TESTS) $(TESTS:=.beam)

