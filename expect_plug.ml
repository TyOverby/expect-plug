open! Core

module Profile = struct
  type t =
    | Prod
    | Beta
  [@@deriving sexp]

  let to_string = function
    | Prod -> "prod"
    | Beta -> "dev"
  ;;
end

type t =
  { root : string
  ; profile : Profile.t
  ; plugins : string list
  }
[@@deriving sexp]

let find_most_recent t plugin =
  let plugin_to_path t plugin = t.root ^/ Profile.to_string t.profile ^/ plugin in
  let version_comparator s1 s2 =
    let to_int_list a = a |> String.split ~on:'.' |> List.map ~f:int_of_string in
    List.compare Int.compare (to_int_list s1) (to_int_list s2)
  in
  let is_directory s =
    match Sys.is_directory s with
    | `Yes -> true
    | _ -> false
  in
  let plugin_root = plugin_to_path t plugin in
  plugin_root
  |> Sys.readdir
  |> Array.to_list
  |> List.sort ~compare:version_comparator
  |> List.map ~f:(( ^/ ) plugin_root)
  |> List.filter ~f:is_directory
  |> List.last_exn
;;

let parse_config () =
  In_channel.stdin |> In_channel.input_all |> Sexp.of_string |> t_of_sexp
;;

let print_settings config =
  let construct_rtp path = sprintf {|set rtp += "%s"|} path in
  config.plugins
  |> List.map ~f:(find_most_recent config)
  |> List.map ~f:construct_rtp
  |> List.iter ~f:print_endline
;;

let () =
  let config = parse_config () in
  print_settings config
;;
