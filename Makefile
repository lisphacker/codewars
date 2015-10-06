progs = ExplosiveSum
allprogs = ExplosiveSum

all:	ExplosiveSum

ExplosiveSum:	ExplosiveSum.hs
	ghc -prof -auto-all -O3 -main-is ExplosiveSum.main ExplosiveSum.hs

prof:	all
	/usr/bin/time ./ExplosiveSum +RTS -p
	cat ExplosiveSum.prof

clean:
	rm -f ${allprogs}
	rm -f *.hi *.o *.prof
