open Core_kernel
open Async_kernel
open Incr_dom

(** The model of our App *)
module Model = struct
  type t = (int * bool) list
  let cutoff t1 t2 = phys_equal t1 t2
end

(** The imperative state of our app, not used here *)
module State = struct
  type t = unit
end

(** The Actions dispatched by our app *)
module Action = struct 
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

(** The initial model *)
let initial_model = []

(** Startup function *)
let on_startup ~schedule_action:_ _model = Deferred.unit

(** Update a model depending on an action *)
let apply_action model action _state ~schedule_action:_ =
  match action with
  | Action.Increment i -> List.mapi ~f:(fun j ((m, b) as s) -> if i = j then m + 1, b else s) model
  | Action.Decrement i -> List.mapi ~f:(fun j ((m, b) as s) -> if i = j then m - 1, b else s) model
  | Action.Reset i -> List.mapi ~f:(fun j ((_, b) as s) -> if i = j then 0, b else s) model
  | Action.Set (i, v) -> Printf.printf "%d <- %d \n" i v; model
  | Action.New -> (0, false)::model
  | Action.Select_one i -> List.mapi ~f:(fun j ((v, b) as s) -> if i = j then (v, not b) else s) model
  | Action.Select_all -> List.map ~f:(fun (v, _) -> (v, true)) model
  | Action.Delete_one i -> List.filteri ~f:(fun j _ -> i <> j) model
  | Action.Delete_all -> List.filter ~f:(fun x -> not (snd x)) model


(** Get the view from the model *)
let view model ~inject =
  let open Vdom in
  let on_click_incr i = Attr.on_click (fun _ -> Action.Increment i |> inject)
  and on_click_decr i = Attr.on_click (fun _ -> Action.Decrement i |> inject)
  and on_click_reset i = Attr.on_click (fun _ -> Action.Reset i |> inject)
  and on_click_new = Attr.on_click (fun _ -> Action.New |> inject)
  and on_click_del1 i = Attr.on_click (fun _ -> Action.Delete_one i |> inject)
  and on_click_del = Attr.on_click (fun _ -> Action.Delete_all |> inject)
  and on_click_slct1 i = Attr.on_click (fun _ -> Action.Select_one i |> inject)
  and on_click_slct = Attr.on_click (fun _ -> Action.Select_all |> inject)
  in
  let view_counter i (v, b) =
    let attr = if b then [Attr.class_ "selected"] else [] in
    Node.(div [] [
        button [on_click_incr i] [text "+"];
        button [on_click_decr i] [text "-"];
        div [Attr.class_ "display"] [text (Int.to_string v)];
        button [on_click_reset i] [text "reset"];
        button (attr @ [on_click_slct1 i]) [text (if b then "unselect" else "select")];
        button [on_click_del1 i] [text "delete"]
      ])
  in
  Node.(body [] [
      div [] [
        button [on_click_new] [text "new"];
        button [on_click_slct] [text "select all"];
        button [on_click_del] [text "delete"];
      ];
      div [] (List.mapi ~f:view_counter model)
    ])

(** Create an incremental component *)
let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map model = model in
  let apply_action = apply_action model in
  let view = view model ~inject in
  Component.create ~apply_action model view



