./src/GenTestHarness $1 $3 3000 >temp/harness.h
iverilog temp/harness.h $1 -o temp/c1
iverilog temp/harness.h $2 -o temp/c2

./temp/c1 | tail -n +2 | tr ' ' "\n" > temp/t1
./temp/c2 | tail -n +2 | tr ' ' "\n" > temp/t2

diff temp/t1 temp/t2

