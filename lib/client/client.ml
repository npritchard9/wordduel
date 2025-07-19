open Core
open Eio.Std

type message_headers = { content_length : int }
type message = { headers : message_headers; content : string }

let create content =
  { headers = { content_length = String.length content }; content }

let serialize msg =
  let total_len = 4 + msg.headers.content_length in
  let buf = Bigstring.create total_len in
  Bigstring.set_int32_be_exn buf ~pos:0 msg.headers.content_length;
  Bigstring.From_string.blit ~src:msg.content ~src_pos:0 ~dst:buf ~dst_pos:4
    ~len:msg.headers.content_length;
  buf

let run_client ~net ~addr =
  Switch.run ~name:"client" @@ fun sw ->
  traceln "Client: connecting to server";
  let flow = Eio.Net.connect ~sw net addr in
  let rec loop () =
    let buf = Cstruct.create 5 in
    Eio.Flow.read_exact flow buf;
    let word = Cstruct.to_string buf in
    traceln "Client received %S" word;
    traceln "Type a message to send (or 'q' to exit):";
    Out_channel.flush Out_channel.stdout;
    match In_channel.input_line In_channel.stdin with
    | Some "q" -> traceln "Client exiting..."
    | Some line ->
        let msg = create line |> serialize in
        let cs = Cstruct.of_bigarray msg in
        Eio.Flow.write flow [ cs ];
        loop ()
    | None -> traceln "EOF, closing client..."
  in
  loop ()

let run () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  run_client ~net ~addr
