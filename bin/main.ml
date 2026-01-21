open Web3_terminal

(* ==================== *)
(* REPL *)
(* ==================== *)

let rec repl () =
  print_string "web3> ";
  let input = read_line () in
  match String.split_on_char ' ' input with
  | ["block"] -> fetch_block_number () >>= repl
  | ["balance"] -> fetch_balance () >>= repl
  | ["gas"] -> fetch_gas_price () >>= repl
  | ["txcount"] -> fetch_transaction_count () >>= repl
  | ["positions"] -> check_positions () >>= repl
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
  | ["send"; to_addr; value] ->
      Printf.printf "Sending %s ETH to %s (placeholder)\n" value to_addr;
      repl ()
  | ["watch"; "block"] ->
      Lwt.async watch_blocks;
      print_endline "Started watching block number.";
      repl ()
  | _ ->
      print_endline "Commands: block | balance | gas | txcount | positions | buy SYMBOL AMOUNT | sell SYMBOL AMOUNT | liquidate SYMBOL | send TO VALUE | watch block | exit";
      repl ()

let () =
  load_config ();
  Lwt_main.run (repl ())
