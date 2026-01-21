open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Safe
open Yojson.Safe.Util

(* Configuration *)
let rpc_url = ref "https://goerli.infura.io/v3/YOUR_INFURA_PROJECT_ID"
let wallet_address = ref "0xYOUR_WALLET_ADDRESS"

let load_config () =
  try
    let ic = open_in "config.json" in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let json = Yojson.Safe.from_string content in
    rpc_url := json |> member "rpc_url" |> to_string;
    wallet_address := json |> member "wallet_address" |> to_string;
    Printf.printf "Configuration loaded.\n"
  with _ -> Printf.printf "Warning: Could not load config.json, using defaults.\n"

(* Helper Functions *)

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
    (Uri.of_string !rpc_url)
  >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body_str ->
  Yojson.Safe.from_string body_str

let int_of_hex hex =
  int_of_string ("0x" ^ (if String.sub hex 0 2 = "0x" then String.sub hex 2 (String.length hex - 2) else hex))

let fetch_block_number () =
  json_rpc_request "eth_blockNumber" [] >|= fun json ->
  try
    let hex = json |> member "result" |> to_string in
    let block_num = int_of_hex hex in
    Printf.printf "Current block: %d\n" block_num
  with _ -> Printf.printf "Error fetching block number\n"

let fetch_balance () =
  let params = [`String !wallet_address; `String "latest"] in
  json_rpc_request "eth_getBalance" params >|= fun json ->
  try
    let hex = json |> member "result" |> to_string in
    let balance = int_of_hex hex in
    Printf.printf "ETH Balance: %.6f ETH\n" ((float_of_int balance) /. 1e18)
  with _ -> Printf.printf "Error fetching balance\n"

type position = {
  symbol: string;
  entry_price: float;
  amount: float;
}

let mock_positions = ref [
  {symbol="ETHUSDT"; entry_price=1200.0; amount=0.5};
  {symbol="BTCUSDT"; entry_price=30000.0; amount=0.1};
]

let check_positions () =
  List.iter (fun pos ->
    let current_price = pos.entry_price *. 1.05 in
    let pnl = (current_price -. pos.entry_price) /. pos.entry_price *. 100.0 in
    Printf.printf "Position %s: %.2f%% PnL\n" pos.symbol pnl
  ) !mock_positions

(* Additional functions *)

let fetch_gas_price () =
  json_rpc_request "eth_gasPrice" [] >|= fun json ->
  try
    let hex = json |> member "result" |> to_string in
    let gas_price = int_of_hex hex in
    Printf.printf "Gas Price: %d wei\n" gas_price
  with _ -> Printf.printf "Error fetching gas price\n"

let fetch_transaction_count () =
  let params = [`String !wallet_address; `String "latest"] in
  json_rpc_request "eth_getTransactionCount" params >|= fun json ->
  try
    let hex = json |> member "result" |> to_string in
    let count = int_of_hex hex in
    Printf.printf "Transaction Count: %d\n" count
  with _ -> Printf.printf "Error fetching transaction count\n"

let rec watch_blocks () =
  fetch_block_number () >>= fun () ->
  Lwt_unix.sleep 10.0 >>= fun () ->
  watch_blocks ()

let fetch_price symbol =
  let url = Printf.sprintf "https://api.coingecko.com/api/v3/simple/price?ids=%s&vs_currencies=usd" (String.lowercase_ascii symbol) in
  Client.get (Uri.of_string url) >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body_str ->
  let json = Yojson.Safe.from_string body_str in
  try
    let price = json |> member (String.lowercase_ascii symbol) |> member "usd" |> to_float in
    price
  with _ -> 0.0

let check_positions () =
  Lwt_list.iter_p (fun pos ->
    fetch_price "ethereum" >|= fun eth_price ->
    let current_price = if pos.symbol = "ETHUSDT" then eth_price else pos.entry_price *. 1.05 in
    let pnl = (current_price -. pos.entry_price) /. pos.entry_price *. 100.0 in
    Printf.printf "Position %s: %.2f%% PnL (Price: %.2f)\n" pos.symbol pnl current_price
  ) !mock_positions