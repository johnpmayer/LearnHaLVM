
all: TestWrite disk.img

TestWrite: TestWrite.hs
	halvm-ghc --make TestWrite.hs

disk.img:
	>disk.img
	dd bs=1K count=4 if=/dev/zero of=disk.img

.PHONY: run

run:
	sudo xm create TestWrite.config -c
