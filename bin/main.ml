let () =
  print_endline "boo";
  Eio_main.run @@ fun env -> Ghola.Window_manager.run env
;;
