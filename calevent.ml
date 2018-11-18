open Core

type weekDay =
| Monday
| Tuesday
| Wednesday
| Thursday
| Friday
| Saturday
| Sunday

type eventFrequency = 
| Once of Time.t 
| Daily of Time.Ofday.t
| Monthly of int
| Weekly of weekDay
| Yearly of Time.t

type contact =
{
    name : string; 
    email : string;
    number : string
}


type calEvent = 
{
    name : string;
    message : string;
    frequency : eventFrequency
}

let createFrequency sfreq = None


(* Format: name,message,frequency *)
let createEvent line = 
    let lst = List.to_array ( String.split line ',' ) in
    match (Array.length lst) with
    | l when l < 3 -> None
    | _ -> 
            match createFrequency (lst.(2)) with
            | None -> None
            | Some freq -> Some { name = lst.(0); message = lst.(1); frequency = freq }




