# HFClifford
Clifford simulator written in `Haskell` and `Fortran`

let `fortran` handles the core numerical part, and let `haskell` handles all others

### preperation
in current dir
```
make
```
then get the abspath of the `clifford-sim/lib` directory using `realpath`
```
realpath clifford-sim/lib
```
then replace the `$name` in the following command with the abspath you just got, and run it
```
export LD_LIBRARY_PATH="$name:$LD_LIBRARY_PATH"
```

### run
in current dir
```
cabal run clifford-sim -- xx
```
where xx stands for the chp file you want to run

for example:
```
cabal run clifford-sim -- ./data/qecc9.chp
```
the output should be:
```
welcome to HFClifford!
program to run: ./data/qecc9.chp
nQubit: 12
pauliRep:
+ZZIZIIIIIIII
+IZIIIIIIIIII
+IIZIIIIIIIII
+ZIIIIIIIIIII
+IIIIZIIIIIII
+IIIIIZIIIIII
+IIIIIIZIIIII
+IIIIIIIZIIII
+IIIIIIIIZIII
+IIIIIIIIIZII
+IIIIIIIIIIZI
+IIIIIIIIIIIZ
measureRes:
[False]
```