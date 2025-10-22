open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Safe
open Yojson.Safe.Util

(* ==================== *)
(* Configuration *)
(* ==================== *)

let rpc_url = "https://goerli.infura.io/v3/YOUR_INFURA_PROJECT_ID"  (* Testnet *)
let wallet_address = "0xYOUR_WALLET_ADDRESS"

(* ==================== *)
(* Helper Functions *)
(* ==================== *)

let json_rpc_request method_name params =
  let body = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String method_name);
    ("params", `List params);
    ("id", `Int 1)
  ] |> Yojson.Safe.to_string in
  Client.post
    ~headers:(Header.init_with "Content-Type" "application/json")
    ~body:(Cohttp_lwt.Body.of_string body)
    (Uri.of_string rpc_url)
  >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body_str ->
  Yojson.Safe.from_string body_str

let int_of_hex hex =
  int_of_string (if String.sub hex 0 2 = "0x" then String.sub hex 2 (String.length hex - 2) else hex)

let fetch_block_number () =
  json_rpc_request "eth_blockNumber" [] >|= fun json ->
  let hex = json |> member "result" |> to_string in
  let block_num = int_of_hex hex in
  Printf.printf "Current block: %d\n" block_num

let fetch_balance () =
  let params = [`String wallet_address; `String "latest"] in
  json_rpc_request "eth_getBalance" params >|= fun json ->
  let hex = json |> member "result" |> to_string in
  let balance = int_of_hex hex in
  Printf.printf "ETH Balance: %.6f ETH\n" ((float_of_int balance) /. 1e18)

type position = {
  symbol: string;
  entry_price: float;
  amount: float;
}

let mock_positions = [
  {symbol="ETHUSDT"; entry_price=1200.0; amount=0.5};
  {symbol="BTCUSDT"; entry_price=30000.0; amount=0.1};
]

let check_positions () =
  List.iter (fun pos ->
    let current_price = pos.entry_price *. 1.05 in
    let pnl = (current_price -. pos.entry_price) /. pos.entry_price *. 100.0 in
    Printf.printf "Position %s: %.2f%% PnL\n" pos.symbol pnl
  ) mock_positions

(* ==================== *)
(* REPL *)
(* ==================== *)

let rec repl () =
  print_string "web3> ";
  let input = read_line () in
  match String.split_on_char ' ' input with
  | ["block"] -> fetch_block_number () >>= repl
  | ["balance"] -> fetch_balance () >>= repl
  | ["positions"] -> check_positions (); repl ()
  | ["exit"] -> print_endline "Exiting Web3 terminal."; Lwt.return_unit
  | ["buy"; symbol; amount] ->
      Printf.printf "Buying %s %s (placeholder)\n" amount symbol;
      repl ()
  | ["sell"; symbol; amount] ->
      Printf.printf "Selling %s %s (placeholder)\n" amount symbol;
      repl ()
  | ["liquidate"; symbol] ->
      Printf.printf "Liquidating %s (placeholder)\n" symbol;
      repl ()
  | _ ->
      print_endline "Commands: block | balance | positions | buy SYMBOL AMOUNT | sell SYMBOL AMOUNT | liquidate SYMBOL | exit";
      repl ()

let () =
  Lwt_main.run (repl ())
