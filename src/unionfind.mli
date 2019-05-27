type 'a equiv_class

val make_class : 'a -> 'a equiv_class

val find : 'a equiv_class -> 'a

val union : 'a equiv_class -> 'a equiv_class -> unit

val equiv : 'a equiv_class -> 'a equiv_class -> bool

val set : 'a equiv_class -> 'a -> unit
