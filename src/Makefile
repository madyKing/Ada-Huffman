
CFLAGS = -g	# a commenter pour enlever le debug

SRC_PACKAGES = dico.ads dico.adb \
               code.ads code.adb \
               file_priorite.ads file_priorite.adb \
               huffman.ads huffman.adb

EXE = exemple_io tp_huffman test_dico


all: $(EXE)

tp_huffman: tp_huffman.adb $(SRC_PACKAGES)
	gnatmake $(CFLAGS) $@

exemple_io: exemple_io.adb
	gnatmake $(CFLAGS) $@

test_dico: test_dico.adb
	gnatmake $(CFLAGS) $@

clean:
	gnatclean -c *
	rm -f b~* ~*

cleanall: clean
	rm -f $(EXE) exemple_io.txt

