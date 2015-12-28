
val verbose : string -> unit
val debug : string -> unit
val info  : string -> unit
val error : string -> unit

(** log_level :
  *   0 : silent
  *   1 : error only
  *   2 : error, info (default)
  *   3 : error, info, debug
  *   4 : error, info, debug, verbose
  *)
val set_log_level : int -> unit

val init : (module Backends.Interface) -> unit
