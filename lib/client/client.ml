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

let read_message reader =
  let len = Eio.Buf_read.BE.uint32 reader |> Int32.to_int_exn in
  let msg = Eio.Buf_read.take len reader in
  traceln "Server: %s" msg;
  if String.is_prefix msg ~prefix:"GAME_OVER" then None else Some msg

let run_client ~net ~addr =
  Switch.run ~name:"client" @@ fun sw ->
  traceln "Client: connecting to server";
  let flow = Eio.Net.connect ~sw net addr in
  let reader = Eio.Buf_read.of_flow flow ~max_size:1024 in

  let rec loop () =
    match read_message reader with
    | None -> ()
    | Some msg ->
        if String.is_prefix msg ~prefix:"YOUR_TURN" then
          let rec get_guess () =
            match In_channel.input_line In_channel.stdin with
            | Some "q" ->
                traceln "Client exiting...";
                None
            | Some line when String.length line <> 5 ->
                traceln "Invalid guess, please enter a 5-letter word.";
                get_guess ()
            | Some line -> Some line
            | None ->
                traceln "EOF, closing client...";
                None
          in
          match get_guess () with
          | Some guess -> (
              let msg = create guess |> serialize in
              let cs = Cstruct.of_bigarray msg in
              Eio.Flow.write flow [ cs ];

              match read_message reader with None -> () | Some _ -> loop ())
          | None -> ()
        else loop ()
  in
  loop ()

let run () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  run_client ~net ~addr
