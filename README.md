# Quine-Maker

Haskell command-line tool for creating quines.

The tool itself happens to be a quine.

## Usage

Type `ghc quine.hs` to compile.

- `./quine.exe make test.hs newTest.hs` will create a new Haskell source file named `newTest.hs` which works like `test.hs`, except it is a quine.

- `./quine.exe check newTest.hs` checks wether `newTest.hs` is a quine.

To access the output of `./test.exe` when given the empty string, call `./newTest.exe @`.

Similarly, to access the output of `./test.exe @`, call `./newTest.exe @@`, and so on.

## Example

```
$ ghc quine.hs
[1 of 2] Compiling Main             ( quine.hs, quine.o )
[2 of 2] Linking quine.exe

$ ./quine.exe check test.hs
[1 of 2] Compiling Main             ( test.hs, test.o )
[2 of 2] Linking test.exe
Oh no, not quite Quine :(

$ ./quine.exe make test.hs newTest.hs
Quintessence: Quite queerly quined!

$ ./quine.exe check newTest.hs
[1 of 2] Compiling Main             ( newTest.hs, newTest.o )
[2 of 2] Linking newTest.exe
It is a Quine!

$ ./test.exe @
["@"]

$ ./newTest.exe @@
["@"]

$ ./quine.exe check quine.hs
It is a Quine!
```

## Requirements

Tested with `ghc 9.6.2`.

## Limitations

The file from which the quine will be made must look roughly like this:

```
...
main :: IO ()
main = do
    args <- getArgs
    ...
```

The quine maker searches for the first occurence of `args <- getArgs` as its entry point, hence one can break it in a variety of ways. Examples:
- Not using `do`-notation for the main, or not using `args <- getArgs` in the main.
- Using `args <- getArgs` multiple times
- Using a comment or string in front of the main containing `args <- getArgs`.

In principle all these problems can be overcome, however this would require implementing quite a bit of the Haskell parser itself.

