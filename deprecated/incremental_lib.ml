module Incremental_intf = struct
  module type S = Incremental.S
end
[@@deprecated "[since 2018-06] Use [Incremental.S]."]

module Incremental = Incremental
[@@deprecated "[since 2018-06] Use the [Incremental] library directly."]
