{-

Test file I/O

-}
import System.IO

{-

Open a file in read mode, then print out the contents.

openFile :: FilePath -> IOMode -> IO Handle

the FilePath type is just String, type FilePath = String
the IOMode type has the following definition∷

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

hGetContents :: Handle -> IO String

It's very similar to the getContents function, the only difference is that
it takes a handle while getContents reads from stdin.

Both functions are lazy, meaning that they won't read the contents from a
handle or stdin and store everything into memory before proceeding to the
next step, rather they will read the content into a buffer and the buffer
will get flushed by the operating system, usually one line at a time.

Therefore, the hGetContents function is like a pipe that connects the file
and the output function putStr.

-

main = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
-}

{-

Or, we can use withFile to make the form shorter, so that we don't have to
close the file explicitly.

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

where (Handle -> IO r) is a function that returns an I/O action, i.e., usually
somthing wrapped in a "do" block.
-}
main = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents )
