open! Core

val save_dot : emit_bind_edges:bool -> Out_channel.t -> Node.Packed.t list -> unit
val save_dot_to_file : emit_bind_edges:bool -> string -> Node.Packed.t list -> unit
