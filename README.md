# Spell Checker
## GHC version used
**ghc   9.8.2      base-4.19.1.0**


## To start the spell checker using cabal
``` sh
cabal run spell-checker
```

## To gets statistical results from dictionaries
You have to specify the language of the dictionaries you want to use (e.g, _en_).  
```
cabal run get-dictionaries en
```
If needed, you can add your own dictionaries in _/Dictionaries/my_language_ and change _en_ by _my\_language_.

## To run the bench 
``` sh
cabal bench
```


## To stop it
Type **enter**
