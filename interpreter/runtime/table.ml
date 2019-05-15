open Types

type size = int32
type index = int32

type elem = ..
type elem += Uninitialized

type table' = elem array
type table =
  {mutable content : table'; max : size option; elem_type : elem_type}
type t = table

exception Bounds
exception SizeOverflow
exception SizeLimit

let within_limits size = function
  | None -> true
  | Some max -> Concreteness.was_concrete (I32.le_u size max)

let create size =
  try Lib.Array32.make size Uninitialized
  with Invalid_argument _ -> raise Out_of_memory

let alloc (TableType ({min; max}, elem_type)) =
  let mint = I32.of_bits min in
  let maxt = match max with None -> None | Some v -> Some (I32.of_bits v) in
  assert (within_limits mint maxt);
  {content = create min; max; elem_type}

let size tab =
  Lib.Array32.length tab.content

let type_of tab =
  TableType ({min = size tab; max = tab.max}, tab.elem_type)

let grow tab delta =
  let old_size = size tab in
  let new_size = Int32.add old_size delta in
  if Concreteness.was_concrete (I32.gt_u (I32.of_bits old_size) (I32.of_bits new_size)) then raise SizeOverflow else
  if not (within_limits (I32.of_bits new_size) (match tab.max with None -> None | Some v -> Some (I32.of_bits v))) then raise SizeLimit else
  let after = create new_size in
  Array.blit tab.content 0 after 0 (Array.length tab.content);
  tab.content <- after

let load tab i =
  try Lib.Array32.get tab.content i with Invalid_argument _ -> raise Bounds

let store tab i v =
  try Lib.Array32.set tab.content i v with Invalid_argument _ -> raise Bounds

let blit tab offset elems =
  let data = Array.of_list elems in
  try Lib.Array32.blit data 0l tab.content offset (Lib.Array32.length data)
  with Invalid_argument _ -> raise Bounds
