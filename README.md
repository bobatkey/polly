# A simple language for services

To build:

 1. Install [opam](https://opam.ocaml.org)

 2. Clone this repository, and `cd` into the clone.

 3. Create a local OPAM switch:

         opam switch create . 4.07.1

 4. Install dependencies:

         opam install dune lwt cohttp ptime yojson

 5. Build it and run it:

         dune exec -- src/main.exe test.polly id1=3 id2=5
