#/bin/bash

for x in `seq 1 10`; do 
	echo "TEST $x"
	ab -n 10000 -c 100 http://localhost:2301/questionario/questionario/$x?access_token=mcpkrCmeybHb6M7Q5mvaxmLrRpgwW52d &
done;

