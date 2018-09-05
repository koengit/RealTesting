all: Lex.hs Parse.hs

Lex.hs: Lex.x
	alex $< -o $@

clean:
	rm -f Lex.hs *.o *.hi
