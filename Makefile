.PHONY: run tail kill

run:
	./Kt.R > log.out

tail:
	tail -f log.out

kill:
	killall R
