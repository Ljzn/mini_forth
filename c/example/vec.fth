: vec "" ; ( -- bytes )

: <> cat ; ( bytes x -- bytesx )

: at swap dup rot split nip 1 split drop ; ( bytes index -- bytes x )

: main vec <<66>> <> <<105>> <> <<116>> <> <<99>> <> <<111>> <> <<105>> <> <<110>> <> 3 at ; ( -- bytes x)