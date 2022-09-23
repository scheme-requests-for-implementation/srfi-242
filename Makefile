scheme = scheme --libdirs srfi-213/lib:lib/ --program

check:
	$(scheme) test-cfg.sps

.PHONY:	check
