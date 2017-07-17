# CL-LZMA

There is no portable Lisp LZMA (de)compressor because the algorithm is complicated, the LZMA SDK is very obfuscated code, there is hardly any documentation for LZMA algorithm and there is no known workforce that can rewrite this in Lisp.

See https://gist.github.com/phoe/8cfdcb4f34e0584c703f1751072a7813 for a CFFI wrapper that I quickly hacked up.
