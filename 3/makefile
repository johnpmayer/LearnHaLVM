
all: Main disk.img

Main: *.hs
	halvm-ghc --make Main.hs

# This time we'll make a disk with 16 4K sectors

disk.img:
	>disk.img
	dd bs=4K count=16 if=/dev/zero of=disk.img

.PHONY: run

run:
	sudo xend
	sudo xm create Main.config -c
