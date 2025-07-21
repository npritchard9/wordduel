open Core
open Eio.Std

let max_guesses = 5

let get_file_size ~cwd =
  let file = Eio.Path.(cwd / "words.txt") in
  let stat = Eio.Path.stat ~follow:false file in
  (file, stat.size)

let get_total_words file_size =
  Optint.Int63.div file_size (Optint.Int63.of_int 6)

let get_offset num_words =
  Optint.Int63.to_int num_words
  |> Random.int |> Int.( * ) 6 |> Optint.Int63.of_int

let read_word path offset =
  Eio.Path.with_open_in path @@ fun file ->
  Eio.File.seek file offset `Set |> ignore;
  let buf = Cstruct.create 5 in
  Eio.Flow.read_exact file buf;
  Cstruct.to_string buf

let get_word ~cwd =
  let file, size = get_file_size ~cwd in
  get_total_words size |> get_offset |> read_word file

let check_guess guess answer =
  let rec loop i acc =
    if i >= 5 then acc
    else
      let c = String.nget guess i in
      match String.nget answer i |> Char.equal c with
      | true -> loop (i + 1) (acc ^ "2")
      | false ->
          let misplaced = if String.contains answer c then "1" else "0" in
          loop (i + 1) (acc ^ misplaced)
  in
  loop 0 ""

let handle_client cwd flow _addr =
  traceln "Server: got connection from client";
  let reader = Eio.Buf_read.of_flow flow ~max_size:1024 in
  let word = get_word ~cwd in
  traceln "word is %s" word;
  let rec loop guesses =
    try
      let len = Eio.Buf_read.BE.uint32 reader |> Int32.to_int_exn in
      let guess = Eio.Buf_read.take len reader in
      traceln "Server received %S" guess;
      if guesses = max_guesses || String.equal guess word then
        Eio.Flow.copy_string word flow
      else
        let correctness = check_guess guess word in
        Eio.Flow.copy_string correctness flow;
        loop (guesses + 1)
    with
    | End_of_file -> traceln "End of input"
    | exn -> traceln "Error: %s" (Exn.to_string exn)
  in
  loop 1

let run_server socket cwd =
  Eio.Net.run_server socket (handle_client cwd)
    ~on_error:(traceln "Error handling connection: %a" Fmt.exn)

let main ~net ~addr ~cwd =
  Switch.run ~name:"main" @@ fun sw ->
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  Fiber.fork ~sw (fun () -> run_server server cwd)

let run () =
  Random.self_init ();
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let cwd = Eio.Stdenv.cwd env in
  main ~net ~addr ~cwd
