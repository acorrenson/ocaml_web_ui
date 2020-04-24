open Incr_dom
module App = Myapp.App

let () =
  Start_app.start
    (module App)
    ~bind_to_element_with_id:"app"
    ~initial_model:App.initial_model