### design choice
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
cabal run clifford-sim
```