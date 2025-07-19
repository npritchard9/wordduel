(* open Core *)

(* open Eio.Std *)
open Lib

(* let handle_client flow _addr =
  traceln "Server: got connection from client";
  let reader = Eio.Buf_read.of_flow flow ~max_size:1024 in
  let len = Eio.Buf_read.BE.uint32 reader |> Int32.to_int_exn in
  let content = Eio.Buf_read.take len reader in
  traceln "Server received %S" content

let run_client ~net ~addr =
  Switch.run ~name:"client" @@ fun sw ->
  traceln "Client: connecting to server";
  let flow = Eio.Net.connect ~sw net addr in
  let msg = Client.create "hi" |> Client.serialize in
  let cs = Cstruct.of_bigarray msg in
  Eio.Flow.write flow [ cs ]

let run_server socket =
  Eio.Net.run_server socket handle_client
    ~on_error:(traceln "Error handling connection: %a" Fmt.exn)

let main ~net ~addr =
  Switch.run ~name:"main" @@ fun sw ->
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Fiber.fork_daemon ~sw (fun () -> run_server server);
  run_client ~net ~addr *)

let () =
  (* Eio_main.run @@ fun env ->
  main ~net:(Eio.Stdenv.net env) ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)) *)
  Server.run ()
