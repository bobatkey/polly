# A simple language for services

To build:

 1. Install [opam](https://opam.ocaml.org)

 2. Clone this repository, and `cd` into the clone.

 3. Create a local OPAM switch:

         opam switch create . 4.07.1

 4. Set up paths:

          eval $(opam env)

 5. Install the package:

         opam install .

 6. Run it:

         polly test.polly id1=4 id2=67
