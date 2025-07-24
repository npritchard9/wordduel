open Core
open Eio.Std

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

module Player = struct
  type t = {
    name : string;
    flow : Eio.Flow.two_way_ty r;
    reader : Eio.Buf_read.t;
  }

  let create name flow =
    { name; flow; reader = Eio.Buf_read.of_flow flow ~max_size:1024 }

  let send p msg =
    let msg = Client.create msg |> Client.serialize in
    let cs = Cstruct.of_bigarray msg in
    Eio.Flow.write p.flow [ cs ]

  let recv p =
    let len = Eio.Buf_read.BE.uint32 p.reader |> Int32.to_int_exn in
    Eio.Buf_read.take len p.reader
end

module Game = struct
  type t = {
    p1 : Player.t;
    p2 : Player.t;
    word : string;
    max_guesses : int;
    mutable turn : int;
  }

  let create ~cwd p1 p2 =
    let word = get_word ~cwd in
    traceln "word is %s" word;
    { p1; p2; word; max_guesses = 5; turn = 0 }
end

let check_guess ~guess ~answer =
  let counts =
    String.fold answer ~init:Char.Map.empty ~f:(fun acc c ->
        Map.update acc c ~f:(fun v -> Option.value v ~default:0 + 1))
  in
  let bytes = Bytes.init 5 ~f:(fun _ -> '0') in
  let rec set_exact_matches i counts =
    if i >= 5 then counts
    else
      let c = String.nget guess i in
      match String.nget answer i |> Char.equal c with
      | true ->
          Bytes.set bytes i '2';
          Map.update counts c ~f:(fun v -> Option.value v ~default:1 - 1)
          |> set_exact_matches (i + 1)
      | false -> set_exact_matches (i + 1) counts
  in
  let rec set_partial_matches i counts =
    if i >= 5 then ()
    else
      let c = String.nget guess i in
      match (String.contains answer c, Map.find counts c) with
      | true, Some v when v > 0 ->
          Bytes.set bytes i '1';
          Map.update counts c ~f:(fun v -> Option.value v ~default:1 - 1)
          |> set_partial_matches (i + 1)
      | _ -> set_partial_matches (i + 1) counts
  in
  set_exact_matches 0 counts |> set_partial_matches 0;
  Bytes.to_string bytes

let handle_player player =
  try
    let guess = Player.recv player in
    traceln "Server received %S" guess;
    Some guess
  with
  | End_of_file ->
      traceln "%s disconnected" player.name;
      None
  | exn ->
      traceln "Error on %s: : %s" player.name (Exn.to_string exn);
      None

let run_server ~sw socket stream =
  let rec loop () =
    Eio.Net.accept_fork socket ~sw
      (fun player _addr ->
        traceln "Server: got connection from client";
        Eio.Stream.add stream (player :> Eio.Flow.two_way_ty r);
        Fiber.await_cancel ())
      ~on_error:(traceln "Error handling connection: %a" Fmt.exn);
    loop ()
  in
  loop ()

let run_game ~cwd flow1 flow2 =
  let p1 = Player.create "Player 1" flow1 in
  let p2 = Player.create "Player 2" flow2 in
  let game = Game.create ~cwd p1 p2 in
  (* Player.send p1 "Game starting";
  Player.send p2 "Game starting"; *)
  traceln "Server starting game";
  let rec game_loop () =
    if game.turn >= (game.max_guesses * 2) - 1 then (
      Player.send p1 ("Game over! The word was: " ^ game.word);
      Player.send p2 ("Game over! The word was: " ^ game.word))
    else
      let active_player, waiting_player =
        if game.turn land 1 = 0 then (p1, p2) else (p2, p1)
      in
      Player.send active_player "Your turn";
      match handle_player active_player with
      | Some guess ->
          let res = check_guess ~guess ~answer:game.word in
          if String.equal guess game.word then (
            Player.send active_player game.word;
            Player.send waiting_player game.word)
          else (
            Player.send active_player res;
            game.turn <- game.turn + 1;
            (* Player.send p2 res; *)
            game_loop ())
      | None -> traceln "did not receive a guess"
  in
  game_loop ()

let matchmaker ~sw ~cwd stream =
  let rec loop () =
    traceln "making matches";
    let p1 = Eio.Stream.take stream in
    traceln "got first player";
    let p2 = Eio.Stream.take stream in
    traceln "got second player";
    Fiber.fork ~sw (fun () ->
        run_game ~cwd p1 p2;
        traceln "Matchmaker: Game finished. Flows closed.");
    loop ()
  in
  loop ()

let main ~net ~addr ~cwd =
  Switch.run ~name:"main" @@ fun sw ->
  let stream = Eio.Stream.create 8 in
  Fiber.fork ~sw (fun () -> matchmaker ~sw ~cwd stream);
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:8 addr in
  Fiber.fork ~sw (fun () -> run_server ~sw socket stream);
  Fiber.await_cancel ()

let run () =
  Random.self_init ();
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let cwd = Eio.Stdenv.cwd env in
  main ~net ~addr ~cwd
