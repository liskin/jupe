PACKAGES=network network-simple mtl process time containers regex-posix
STACK_PACKAGES=$(addprefix --package ,$(PACKAGES))

all:
	stack exec ghc $(STACK_PACKAGES) -- --make -outputdir build jupe.hs

ghcid:
	ghcid -c 'stack ghci $(STACK_PACKAGES) jupe.hs'
