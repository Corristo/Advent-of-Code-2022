main: main.pl
	gplc main.pl -o main

check: main
	GLOBALSZ=1000000 ./main

clean:
	rm -rf ./main
