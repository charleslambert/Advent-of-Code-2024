open Base

let split_on ~del =
  String.Search_pattern.split_on (String.Search_pattern.create del)
;;

