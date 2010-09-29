rem ghc -O2 -threaded -prof -auto-all -caf-all -o sillyChess.exe --make main.hs
ghc -O2 -o -threaded sillyChess.exe --make main.hs
del *.hi
del *.o