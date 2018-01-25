open Postgresql
open Batteries 
open Thrift
open Service_types

let with_connection f =
  try 
    let c = new connection ~host:"localhost" ~port:"5432" ~dbname:"tdb"
    ~user:"tuser" ~password:"tpass" () in
    f c;
  with _ -> print_endline "ERROR"
;;

type tconnection = {
  trans: Transport.t ;
  proto: Thrift.Protocol.t ;
  audio: Service.client ;
}

exception Die;;
let sod = function
  Some v -> v
  | None -> raise Die;;

let tconnect ~host port =
  let tx = new TSocket.t host port in
  let proto = new TBinaryProtocol.t tx in
  let service = new Service.client proto proto in
  tx#opn;
  { trans = tx; proto = proto; service = service}
;;

let fetch_results c =
  match c#get_result with
  | None -> print_endline "No results"; assert false
  | Some r -> r
;;

let main () =
  print_endline "Hi";
  let tc = tconnect ~host:"127.0.0.1" 9090 in
  let resp = tc.service#structCall "echo" in
  print_endline (BatPervasives.dump resp);
  tc.trans#close;

  let c = new connection ~host:"localhost" ~port:"5432" ~dbname:"tdb"
  ~user:"tuser" ~password:"tpass" () in
  c#send_query "SELECT * FROM ttable LIMIT 1";
  let r = fetch_results c in
  let tupes = r#get_all in
  flush stdout;
  print_endline (BatPervasives.dump tupes);
  print_endline "done"


let _ = 
  try main () with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)
