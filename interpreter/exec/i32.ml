(* WebAssembly-compatible i32 implementation *)

include Int.Make
  (struct
    include Int32
    (* let to_int32 x = x
     * let of_int32 x = x *)
    let bitwidth = 32
  end)
