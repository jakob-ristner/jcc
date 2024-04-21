all:
	cargo build
	cp -f target/debug/jcc ./jlc

test:
	./project/tester/Docker/runtest.sh -- .




