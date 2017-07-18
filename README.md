# CL-LZMA

This is a CFFI wrapper around a LZMA foreign library, compiled straight out of the official LZMA SDK C binary.

There is no portable Lisp LZMA (de)compressor because:
  * the algorithm is complicated
  * the LZMA SDK is very obfuscated code
  * there is hardly any documentation for LZMA algorithm
  * there is no known workforce that can rewrite this in Lisp.

## TODO
  * write a proper README, since the code seems to be working `on my machine`

## Compiling
Right now, only a x64 Linux binary is compiled. To compile your own:
  1. Install the official LZMA SDK from Igor Pavlov.
  2. Modify /C/Util/Lzma/makefile.gcc, add -fPIC to CFLAGS.
  3. Issue make, which will build all object files.
  4. Build the shared library file. On Linux, this is `$ gcc -shared -O2 -Wall -D_7ZIP_ST -fPIC -o lzma.so *.o` - on other platforms, I bet you can figure it out.
  5. Put the file anywhere you want and load it with CFFI.

## Credits and inspirations
This is based on https://gist.github.com/phoe/8cfdcb4f34e0584c703f1751072a7813 - a CFFI wrapper that I quickly hacked up.

This is based on https://gist.github.com/Treeki/f431a2ff44aff984590a97a5c09f6f28 which is a C interface to the LZMA SDK.
