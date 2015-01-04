
module type Interface = sig

  val http_request : host:string -> port:int -> meth:string -> body:string -> callback:(string -> unit) -> unit

end

