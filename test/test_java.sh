#/bin/bash

for x in `seq 1 10`; do 
	echo "TEST $x"
	ab -n 10 -c 10 http://localhost:2301/unb_aula/curso/$x?access_token=9PjNssCOHWm4bUPzlRMDfhqjGAHLZmR1 &
done;

