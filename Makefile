
./src/TestParser: 
	cd src && ghc -O2 -Wall -fno-warn-missing-signatures TestParser

TESTS:=./tests/unit8/in_2.v \
	./tests/unit8/in_1.v \
	./tests/unit4/in_2.v \
	./tests/unit4/in_1.v \
	./tests/unit9/in_2.v \
	./tests/unit9/in_1.v \
	./tests/unit2/in_2.v \
	./tests/unit2/in_1.v \
	./tests/unit6/in_2.v \
	./tests/unit6/in_1.v \
	./tests/unit5/in_2.v \
	./tests/unit5/in_1.v \
	./tests/unit3/in_2.v \
	./tests/unit3/in_1.v \
	./tests/unit7/in_2.v \
	./tests/unit7/in_1.v \
	./tests/unit1/in_2.v \
	./tests/unit1/in_1.v \

.PHONY : test $(TESTS)

test: ./src/TestParser $(TESTS)

$(TESTS): ./src/TestParser
	./src/TestParser $@
