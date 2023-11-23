open! Core

val save_dot : Out_channel.t -> Node.Packed.t list -> unit
val save_dot_to_file : string -> Node.Packed.t list -> unit
