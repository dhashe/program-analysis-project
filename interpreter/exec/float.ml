module type RepType =
sig
  type t

  val pos_nan : t
  val neg_nan : t
  val bits_of_float : float -> t
  val float_of_bits : t -> float
  val of_string : string -> t
  val to_string : t -> string
  val to_hex_string : t -> string

  val lognot : t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t

  val min_int : t
  val max_int : t

  val zero : t
  val bare_nan : t
end

module type S =
sig
  type t
  type bits
  val pos_nan : t
  val neg_nan : t
  val of_float : float -> t
  val to_float : t -> float
  val of_string : string -> t
  val to_string : t -> string
  val of_bits : bits -> t
  val to_bits : t -> bits
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val sqrt : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val ceil : t -> t
  val floor : t -> t
  val trunc : t -> t
  val nearest : t -> t
  val abs : t -> t
  val neg : t -> t
  val copysign : t -> t -> t
  val eq : t -> t -> bool Concreteness.concreteness
  val ne : t -> t -> bool Concreteness.concreteness
  val lt : t -> t -> bool Concreteness.concreteness
  val le : t -> t -> bool Concreteness.concreteness
  val gt : t -> t -> bool Concreteness.concreteness
  val ge : t -> t -> bool Concreteness.concreteness
  val zero : t

  val is_concrete : t -> bool
end

module Make (Rep : RepType) : S with type bits = Rep.t =
struct
  module C = Concreteness

  type t = Rep.t Concreteness.concreteness
  type bits = Rep.t

  let pos_inf = Rep.bits_of_float (1.0 /. 0.0)
  let neg_inf = Rep.bits_of_float (-. (1.0 /. 0.0))
  let pos_nan = C.Concrete Rep.pos_nan
  let neg_nan = C.Concrete Rep.neg_nan
  let bare_nan = Rep.bare_nan

  let of_float x = C.Concrete (Rep.bits_of_float x)
  let to_float x = match x with
      C.Concrete y -> Rep.float_of_bits y
    | C.Symbolic _ -> failwith "Float symbolic"

  let of_bits x = C.Concrete x
  let to_bits x = match x with
      C.Concrete y -> y
    | C.Symbolic _ -> failwith "Float symbolic"

  let is_inf x = x = pos_inf || x = neg_inf
  let is_nan x = let xf = Rep.float_of_bits x in xf <> xf

  (*
   * When the result of an arithmetic operation is NaN, the most significant
   * bit of the significand field is set.
   *)
  let canonicalize_nan x = Rep.logor x Rep.pos_nan

  (*
   * When the result of a binary operation is NaN, the resulting NaN is computed
   * from one of the NaN inputs, if there is one. If both are NaN, one is
   * selected nondeterminstically. If neither, we use a default NaN value.
   *)
  let determine_binary_nan x y =
    (*
     * TODO: There are two nondeterministic things we could do here. When both
     * x and y are NaN, we can nondeterministically pick which to return. And
     * when neither is NaN, we can nondeterministically pick whether to return
     * pos_nan or neg_nan.
     *)
    let nan =
      if is_nan x then x else
      if is_nan y then y else Rep.pos_nan
    in canonicalize_nan nan

  (*
   * When the result of a unary operation is NaN, the resulting NaN is computed
   * from one of the NaN input, if there it is NaN. Otherwise, we use a default
   * NaN value.
   *)
  let determine_unary_nan x =
    (*
     * TODO: There is one nondeterministic thing we could do here. When the
     * operand is not NaN, we can nondeterministically pick whether to return
     * pos_nan or neg_nan.
     *)
    let nan = if is_nan x then x else Rep.pos_nan in
    canonicalize_nan nan

  let fp_sort = if (Rep.to_hex_string Rep.bare_nan) = "0x7f800000l"
    then (Z3.FloatingPoint.mk_sort_32 C.ctx)
    else (Z3.FloatingPoint.mk_sort_64 C.ctx)

  let fp_round = Z3.FloatingPoint.RoundingMode.mk_rne C.ctx

  let to_fp_const x = (Z3.FloatingPoint.mk_numeral_s C.ctx (Rep.to_string x) fp_sort)
  let cased_binop concrete_func symbolic_func x y =
    match (x,y) with (C.Concrete x', C.Concrete y') -> C.Concrete (concrete_func x' y')
                   | (C.Concrete x', C.Symbolic y') -> C.Symbolic (symbolic_func (to_fp_const x') y')
                   | (C.Symbolic x', C.Concrete y') -> C.Symbolic (symbolic_func x' (to_fp_const y'))
                   | (C.Symbolic x', C.Symbolic y') -> C.Symbolic (symbolic_func x' y')

  let cased_unop concrete_func symbolic_func x =
    match x with C.Concrete x' -> C.Concrete (concrete_func x')
               | C.Symbolic x' -> C.Symbolic (symbolic_func x')

  let binary x op y =
    let xf = to_float (C.Concrete x) in
    let yf = to_float (C.Concrete y) in
    let t = op xf yf in
    if t = t then C.was_concrete (of_float t) else determine_binary_nan x y

  let unary op x =
    let t = op (to_float (C.Concrete x)) in
    if t = t then C.was_concrete (of_float t) else determine_unary_nan x

  let zero = of_float 0.0

  let add' x y = binary x (+.)  y
  let sub' x y = binary x (-.)  y
  let mul' x y = binary x ( *.) y
  let div' x y = binary x (/.)  y
  let add'' x y = Z3.FloatingPoint.mk_add C.ctx fp_round x y
  let sub'' x y = Z3.FloatingPoint.mk_sub C.ctx fp_round x y
  let mul'' x y = Z3.FloatingPoint.mk_mul C.ctx fp_round x y
  let div'' x y = Z3.FloatingPoint.mk_div C.ctx fp_round x y

  let add x y = cased_binop add' add'' x y
  let sub x y = cased_binop sub' sub'' x y
  let mul x y = cased_binop sub' sub'' x y
  let div x y = cased_binop sub' sub'' x y

  let sqrt' x =  unary Pervasives.sqrt  x
  let ceil'  x = unary Pervasives.ceil  x
  let floor' x = unary Pervasives.floor x

  let sqrt'' x = Z3.FloatingPoint.mk_sqrt C.ctx fp_round x
  (* TODO: The below might not handle positive/negative zero correctly *)
  let ceil'' x = Z3.FloatingPoint.mk_round_to_integral C.ctx fp_round
      (Z3.FloatingPoint.mk_add C.ctx fp_round x (to_fp_const (Rep.of_string "0.5")))
  let floor'' x = Z3.FloatingPoint.mk_round_to_integral C.ctx fp_round
      (Z3.FloatingPoint.mk_sub C.ctx fp_round x (to_fp_const (Rep.of_string "0.5")))

  let sqrt x = cased_unop sqrt' sqrt'' x
  let ceil x = cased_unop ceil' ceil'' x
  let floor x = cased_unop floor' floor'' x

  let trunc' x =
    let xf = to_float (C.Concrete x) in
    (* preserve the sign of zero *)
    if xf = 0.0 then x else
    (* trunc is either ceil or floor depending on which one is toward zero *)
    let f = if xf < 0.0 then Pervasives.ceil xf else Pervasives.floor xf in
    let result = C.was_concrete (of_float f) in
    if is_nan result then determine_unary_nan result else result
  let trunc'' x = failwith "TODO"
  let trunc x = cased_unop trunc' trunc'' x

  let nearest' x =
    let xf = to_float (C.Concrete x) in
    (* preserve the sign of zero *)
    if xf = 0.0 then x else
    (* nearest is either ceil or floor depending on which is nearest or even *)
    let u = Pervasives.ceil xf in
    let d = Pervasives.floor xf in
    let um = abs_float (xf -. u) in
    let dm = abs_float (xf -. d) in
    let u_or_d =
      um < dm ||
      um = dm && let h = u /. 2. in Pervasives.floor h = h
    in
    let f = if u_or_d then u else d in
    let result = C.was_concrete (of_float f) in
    if is_nan result then determine_unary_nan result else result
  let nearest'' x = Z3.FloatingPoint.mk_round_to_integral C.ctx fp_round x
  let nearest x = cased_unop nearest' nearest'' x

  let min' x y =
    let xf = to_float (C.Concrete x) in
    let yf = to_float (C.Concrete y) in
    (* min -0 0 is -0 *)
    if xf = yf then Rep.logor x y else
    if xf < yf then x else
    if xf > yf then y else
    determine_binary_nan x y
  let min'' x y = Z3.FloatingPoint.mk_min C.ctx x y
  let min x y = cased_binop min' min'' x y

  let max' x y =
    let xf = to_float (C.Concrete x) in
    let yf = to_float (C.Concrete y) in
    (* max -0 0 is 0 *)
    if xf = yf then Rep.logand x y else
    if xf > yf then x else
    if xf < yf then y else
    determine_binary_nan x y
  let max'' x y = Z3.FloatingPoint.mk_max C.ctx x y
  let max x y = cased_binop max' max'' x y

  (* abs, neg, and copysign are purely bitwise operations, even on NaN values *)
  let abs' x =
    Rep.logand x Rep.max_int

  let neg' x =
    Rep.logxor x Rep.min_int

  let copysign' x y =
    Rep.logor (abs' x) (Rep.logand y Rep.min_int)

  let abs'' x = Z3.FloatingPoint.mk_abs C.ctx x
  let neg'' x = Z3.FloatingPoint.mk_neg C.ctx x
  let copysign'' x y = failwith "TODO"

  let abs x = cased_unop abs' abs'' x
  let neg x = cased_unop neg' neg'' x
  let copysign x y = cased_binop copysign' copysign'' x y

  let eq' x y = (to_float (C.Concrete x) = to_float (C.Concrete y))
  let ne' x y = (to_float (C.Concrete x) <> to_float (C.Concrete y))
  let lt' x y = (to_float (C.Concrete x) < to_float (C.Concrete y))
  let gt' x y = (to_float (C.Concrete x) > to_float (C.Concrete y))
  let le' x y = (to_float (C.Concrete x) <= to_float (C.Concrete y))
  let ge' x y = (to_float (C.Concrete x) >= to_float (C.Concrete y))

  let eq'' x y = Z3.FloatingPoint.mk_eq C.ctx x y
  let ne'' x y = Z3.Boolean.mk_not C.ctx (Z3.FloatingPoint.mk_eq C.ctx x y)
  let lt'' x y = Z3.FloatingPoint.mk_lt C.ctx x y
  let gt'' x y = Z3.FloatingPoint.mk_gt C.ctx x y
  let le'' x y = Z3.FloatingPoint.mk_leq C.ctx x y
  let ge'' x y = Z3.FloatingPoint.mk_gt C.ctx x y

  let eq x y = cased_binop eq' eq'' x y
  let ne x y = cased_binop ne' ne'' x y
  let lt x y = cased_binop lt' lt'' x y
  let gt x y = cased_binop gt' gt'' x y
  let le x y = cased_binop le' le'' x y
  let ge x y = cased_binop ge' ge'' x y

  let of_signless_string s =
    if s = "inf" then
      pos_inf
    else if s = "nan" then
      (C.was_concrete pos_nan)
    else if String.length s > 6 && String.sub s 0 6 = "nan:0x" then
      let x = Rep.of_string (String.sub s 4 (String.length s - 4)) in
      if x = Rep.zero then
        raise (Failure "nan payload must not be zero")
      else if Rep.logand x bare_nan <> Rep.zero then
        raise (Failure "nan payload must not overlap with exponent bits")
      else if x < Rep.zero then
        raise (Failure "nan payload must not overlap with sign bit")
      else
        Rep.logor x bare_nan
    else
      (* TODO: once we update past 4.02, replace buffer hack with this
      let s' = String.concat "" (String.split_on_char '_' s) in
      *)
      let buf = Buffer.create (String.length s) in
      for i = 0 to String.length s - 1 do
        if s.[i] <> '_' then Buffer.add_char buf s.[i]
      done;
      let s' = Buffer.contents buf in
      let x = C.was_concrete (of_float (float_of_string s')) in
      if is_inf x then failwith "of_string" else x

  let of_string s = C.Concrete (
    if s = "" then
      failwith "of_string"
    else if s.[0] = '+' || s.[0] = '-' then
      let x = of_signless_string (String.sub s 1 (String.length s - 1)) in
      if s.[0] = '+' then x else neg' x
    else
      of_signless_string s
  )

  let to_string s = match s with
      C.Concrete x -> (
        (if x < Rep.zero then "-" else "") ^
        if is_nan x then
          "nan:0x" ^ Rep.to_hex_string (Rep.logand (abs' x) (Rep.lognot bare_nan))
        else
          (* TODO: use sprintf "%h" once we have upgraded to OCaml 4.03 *)
          string_of_float (to_float (C.Concrete (abs' x)))
      )
    | C.Symbolic x -> "[[" ^ Z3.Expr.to_string x ^ "]]"

  let is_concrete x = match x with C.Concrete _ -> true | C.Symbolic _ -> false
end
