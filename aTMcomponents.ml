type id = int

type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;;

type account_spec = {name : string; id : id; balance : int} ;;

(* initialize accts -- Establishes a database of accounts, each with a
   name, aribtrary id, and balance. The names and balances are
   initialized as per the `accts` provided. *)

let registrants = ref []

let initialize (lst : account_spec list) : unit =
  List.iter (fun x -> registrants := [x] @ !registrants) lst ;;

let acquire_id : unit -> id =
  fun () -> (print_string("Enter customer id: ");
  read_int ()) ;;

let acquire_amount : unit -> int =
  fun () -> (print_string("Enter amount: ");
  read_int ()) ;;


let acquire_act : unit -> action =
  print_string("Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit: =");
  match read_line () with
  | "B" -> fun () -> Balance
  | "-" -> fun () -> Withdraw (acquire_amount ())
  | "+" -> fun () -> Deposit (acquire_amount ())
  | "=" -> fun () -> Finished
  | "X" -> fun () -> Next
  | _ -> raise (Invalid_argument ("Invalid input")) ;;

let get_balance (id : id) : int =
  (List.find (fun x -> x.id = id) !registrants).balance ;;

let get_name (id : id) : string =
  (List.find (fun x -> x.id = id) !registrants).name ;;

let update_balance (id : id) (n : int) : unit =
  let (el, lst) = List.partition (fun x -> x.id = id) !registrants in
  let e = List.hd el in
  if el = [] then raise Not_found
  else registrants := lst @ [{name = e.name; id = e.id; balance = n}] ;;

let present_message (str : string) : unit =
  print_string str;
  print_newline () ;;

let deliver_cash (n : int) : unit =
  print_string ("Here's your cash: ");
  for i = 0 to n / 20 do
    print_string ("[20 @ 20]")
  done;
  Printf.printf "and %i more" (n - n/20) ;;



