let () =
  print_endline "boo";
  Eio_main.run @@ fun env -> Ghola.Handlers.run env
;;
