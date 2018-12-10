cat $1 img.test.c >main.c; cc main.c libpgm.a -lm; ./a.out; display out.pgm
