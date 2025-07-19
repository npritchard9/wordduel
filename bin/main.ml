open Eio.Std

let handle_client flow _addr =
  traceln "Server: got connection from client";
  Eio.Flow.copy_string "Hello from server" flow;
  Eio.Flow.shutdown flow `Send;
  traceln "Server received: %S" (Eio.Flow.read_all flow)

let run_client ~net ~addr =
  Switch.run ~name:"client" @@ fun sw ->
  traceln "Client: connecting to server";
  let flow = Eio.Net.connect ~sw net addr in
  (* Read all data until end-of-stream (shutdown): *)
  traceln "Client: received %S" (Eio.Flow.read_all flow);
  Eio.Flow.copy_string "Hello from client" flow;
  Eio.Flow.shutdown flow `Send

let run_server socket =
  Eio.Net.run_server socket handle_client
    ~on_error:(traceln "Error handling connection: %a" Fmt.exn)

let main ~net ~addr =
  Switch.run ~name:"main" @@ fun sw ->
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Fiber.fork ~sw (fun () -> run_server server);
  run_client ~net ~addr

let () =
  Eio_main.run @@ fun env ->
  main ~net:(Eio.Stdenv.net env) ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
