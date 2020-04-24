module Model : sig
  type t
  include Incr_dom.App_intf.Model with type t := t
end

module Action : sig
  type t = 
    | Increment of int
    | Decrement of int
    | Reset of int
    | Set of int * int
    | New
    | Delete_one of int
    | Delete_all
    | Select_one of int
    | Select_all
  [@@deriving sexp_of]
end

val initial_model : Model.t

include
  Incr_dom.App_intf.S
  with module Model := Model
   and module Action := Action
   and type State.t = unit