.PHONY: clean jar install

all: clean compile-stylesheet  jar install

clean:
	clj -T:build clean

compile-stylesheet:
	clj -T:build compile-stylesheet

jar:
	clj -T:build jar

install:
	clj -T:build install
