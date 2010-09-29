ghc -threaded -prof -auto-all -caf-all -o sillyChess.exe --make main.hs
rem ghc -O2 -o sillyChess.exe --make main.hs
del *.hi
del *.o