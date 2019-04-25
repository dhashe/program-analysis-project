module type RepType =
sig
  type t

  val zero : t
  val one : t
  val minus_one : t
  val max_int : t
  val min_int : t

  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t (* raises Division_by_zero *)
  val rem : t -> t -> t (* raises Division_by_zero *)

  val logand : t -> t -> t
  val lognot : t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t

  val of_int : int -> t
  val to_int : t -> int
  val to_string : t -> string

  (* val to_int32 : t -> int32
   * val of_int32 : int32 -> t
   * val to_float : t -> float
   * val of_float : float -> t *)

  val bitwidth : int
end

module type S =
sig
  type t
  type bits

  val of_bits : bits -> t
  val to_bits : t -> bits

  val zero : t
  val one : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div_s : t -> t -> t (* raises IntegerDivideByZero, IntegerOverflow *)
  val div_u : t -> t -> t (* raises IntegerDivideByZero *)
  val rem_s : t -> t -> t (* raises IntegerDivideByZero *)
  val rem_u : t -> t -> t (* raises IntegerDivideByZero *)
  val and_ : t -> t -> t
  val or_ : t -> t -> t
  val xor : t -> t -> t
  val shl : t -> t -> t
  val shr_s : t -> t -> t
  val shr_u : t -> t -> t
  val rotl : t -> t -> t
  val rotr : t -> t -> t
  val clz : t -> t
  val ctz : t -> t
  val popcnt : t -> t
  val eqz : t -> bool
  val eq : t -> t -> bool
  val ne : t -> t -> bool
  val lt_s : t -> t -> bool
  val lt_u : t -> t -> bool
  val le_s : t -> t -> bool
  val le_u : t -> t -> bool
  val gt_s : t -> t -> bool
  val gt_u : t -> t -> bool
  val ge_s : t -> t -> bool
  val ge_u : t -> t -> bool

  val of_int_s : int -> t
  val of_int_u : int -> t
  val of_string_s : string -> t
  val of_string_u : string -> t
  val of_string : string -> t
  val to_string_s : t -> string
  val to_string_u : t -> string

  val is_concrete : t -> bool
end

module Make (Rep : RepType) : S with type bits = Rep.t and type t = Rep.t Concreteness.concreteness =
struct
  module C = Concreteness

  (*
   * Unsigned comparison in terms of signed comparison.
   *)
  let cmp_u x op y =
    op (Rep.add x Rep.min_int) (Rep.add y Rep.min_int)

  (*
   * Unsigned division and remainder in terms of signed division; algorithm from
   * Hacker's Delight, Second Edition, by Henry S. Warren, Jr., section 9-3
   * "Unsigned Short Division from Signed Division".
   *)
  let divrem_u n d =
    if d = Rep.zero then raise Numeric_error.IntegerDivideByZero else
    let t = Rep.shift_right d (Rep.bitwidth - 1) in
    let n' = Rep.logand n (Rep.lognot t) in
    let q = Rep.shift_left (Rep.div (Rep.shift_right_logical n' 1) d) 1 in
    let r = Rep.sub n (Rep.mul q d) in
    if cmp_u r (<) d then
      q, r
    else
      Rep.add q Rep.one, Rep.sub r d

  type t = Rep.t C.concreteness
  type bits = Rep.t

  let of_bits x = C.Concrete x
  let to_bits = function C.Concrete x -> x | C.Symbolic x -> failwith "Symbolic"

  let zero = C.Concrete Rep.zero
  let one = C.Concrete Rep.one
  let ten = C.Concrete (Rep.of_int 10)

  let check_binop_concrete binop x y = match (x,y) with
      (C.Concrete x', C.Concrete y') -> C.Concrete (binop x' y')
    | _ -> failwith "TODO"
  let check_unop_concrete unop x = match x with
      C.Concrete x' -> C.Concrete (unop x')
    | _ -> failwith "TODO"
  let check_relop_concrete relop x y = match (x,y) with
      (C.Concrete x', C.Concrete y') -> relop x' y'
    | _ -> failwith "TODO"
  let check_unrelop_concrete unop x = match x with
      C.Concrete x' -> unop x'
    | _ -> failwith "TODO"

  (* add, sub, and mul are sign-agnostic and do not trap on overflow. *)
  let add = check_binop_concrete Rep.add
  let sub = check_binop_concrete Rep.sub
  let mul = check_binop_concrete Rep.mul

  (* result is truncated toward zero *)
  let div_s' x y =
    if y = Rep.zero then
      raise Numeric_error.IntegerDivideByZero
    else if x = Rep.min_int && y = Rep.minus_one then
      raise Numeric_error.IntegerOverflow
    else
      Rep.div x y
  let div_s = check_binop_concrete div_s'

  (* result is floored (which is the same as truncating for unsigned values) *)
  let div_u' x y =
    let q, r = divrem_u x y in q
  let div_u = check_binop_concrete div_u'

  (* result has the sign of the dividend *)
  let rem_s' x y =
    if y = Rep.zero then
      raise Numeric_error.IntegerDivideByZero
    else
      Rep.rem x y
  let rem_s = check_binop_concrete rem_s'

  let rem_u' x y =
    let q, r = divrem_u x y in r
  let rem_u = check_binop_concrete rem_u'

  let and_ = check_binop_concrete Rep.logand
  let or_ = check_binop_concrete Rep.logor
  let xor = check_binop_concrete Rep.logxor

  (* WebAssembly's shifts mask the shift count according to the bitwidth. *)
  let shift' f x y =
    f x (Rep.to_int (Rep.logand y (Rep.of_int (Rep.bitwidth - 1))))
  let shift f = check_binop_concrete (shift' f)

  let shl x y =
    shift Rep.shift_left x y

  let shr_s x y =
    shift Rep.shift_right x y

  let shr_u x y =
    shift Rep.shift_right_logical x y

  (* We must mask the count to implement rotates via shifts. *)
  let clamp_rotate_count n =
    Rep.to_int (Rep.logand n (Rep.of_int (Rep.bitwidth - 1)))

  let rotl' x y =
    let n = clamp_rotate_count y in
    Rep.logor (Rep.shift_left x n) (Rep.shift_right_logical x (Rep.bitwidth - n))
  let rotl = check_binop_concrete rotl'

  let rotr' x y =
    let n = clamp_rotate_count y in
    Rep.logor (Rep.shift_right_logical x n) (Rep.shift_left x (Rep.bitwidth - n))
  let rotr = check_binop_concrete rotr'

  (* clz is defined for all values, including all-zeros. *)
  let clz' x =
    let rec loop acc n =
      if n = Rep.zero then
        Rep.bitwidth
      else if Rep.logand n (Rep.shift_left Rep.one (Rep.bitwidth - 1)) = Rep.zero then
        loop (1 + acc) (Rep.shift_left n 1)
      else
        acc
    in Rep.of_int (loop 0 x)
  let clz = check_unop_concrete clz'

  (* ctz is defined for all values, including all-zeros. *)
  let ctz' x =
    let rec loop acc n =
      if n = Rep.zero then
        Rep.bitwidth
      else if Rep.logand n Rep.one = Rep.one then
        acc
      else
        loop (1 + acc) (Rep.shift_right_logical n 1)
    in Rep.of_int (loop 0 x)
  let ctz = check_unop_concrete ctz'

  let popcnt' x =
    let rec loop acc i n =
      if n = Rep.zero then
        acc
      else
        let acc' = if Rep.logand n Rep.one = Rep.one then acc + 1 else acc in
        loop acc' (i - 1) (Rep.shift_right_logical n 1)
    in Rep.of_int (loop 0 Rep.bitwidth x)
  let popcnt = check_unop_concrete popcnt'

  let eqz' x = x = Rep.zero
  let eqz = function C.Concrete x -> eqz' x
                   | C.Symbolic _ -> failwith "TODO"

  let eq' x y = x = y
  let ne' x y = x <> y
  let lt_s' x y = x < y
  let lt_u' x y = cmp_u x (<) y
  let le_s' x y = x <= y
  let le_u' x y = cmp_u x (<=) y
  let gt_s' x y = x > y
  let gt_u' x y = cmp_u x (>) y
  let ge_s' x y = x >= y
  let ge_u' x y = cmp_u x (>=) y

  let eq = check_relop_concrete eq'
  let ne = check_relop_concrete ne'
  let lt_s = check_relop_concrete lt_s'
  let lt_u = check_relop_concrete lt_u'
  let le_s = check_relop_concrete le_s'
  let le_u = check_relop_concrete le_u'
  let gt_s = check_relop_concrete gt_s'
  let gt_u = check_relop_concrete gt_u'
  let ge_s = check_relop_concrete ge_s'
  let ge_u = check_relop_concrete ge_u'

  let of_int_s i = C.Concrete (Rep.of_int i)
  let of_int_u i = and_ (C.Concrete (Rep.of_int i)) (or_ (shl (C.Concrete (Rep.of_int max_int)) one) one)

  let to_string_s = check_unrelop_concrete Rep.to_string
  let to_string_u' i =
    if i >= Rep.zero then
      Rep.to_string i
    else
      Rep.to_string (match (div_u (C.Concrete i) ten) with C.Concrete x -> x | _ -> failwith "RIP") ^ to_string_s (rem_u (C.Concrete i) ten)
  let to_string_u = function C.Concrete x -> to_string_u' x
                   | C.Symbolic _ -> failwith "TODO"

  (* String conversion that allows leading signs and unsigned values *)

  let require b = if not b then failwith "of_string"

  let dec_digit = function
    | '0' .. '9' as c -> Char.code c - Char.code '0'
    | _ -> failwith "of_string"

  let hex_digit = function
    | '0' .. '9' as c ->  Char.code c - Char.code '0'
    | 'a' .. 'f' as c ->  0xa + Char.code c - Char.code 'a'
    | 'A' .. 'F' as c ->  0xa + Char.code c - Char.code 'A'
    | _ ->  failwith "of_string"

  let max_upper, max_lower = divrem_u Rep.minus_one (Rep.of_int 10)

  let of_string s =
    let open Rep in
    let len = String.length s in
    let rec parse_hex i num =
      if i = len then num else
      if s.[i] = '_' then parse_hex (i + 1) num else
      let digit = of_int (hex_digit s.[i]) in
      require (le_u num (shr_u (C.Concrete minus_one) (C.Concrete (of_int 4))));
      parse_hex (i + 1) (C.Concrete (logor (shift_left (to_bits num) 4) digit))
    in
    let rec parse_dec i num =
      if i = len then num else
      if s.[i] = '_' then parse_dec (i + 1) num else
      let digit = of_int (dec_digit s.[i]) in
      require (lt_u num (C.Concrete max_upper) || num = (C.Concrete max_upper) && le_u (C.Concrete digit) (C.Concrete max_lower));
      parse_dec (i + 1) (C.Concrete (add (mul (to_bits num) (to_bits ten)) digit))
    in
    let parse_int i =
      require (len - i > 0);
      if i + 2 <= len && s.[i] = '0' && s.[i + 1] = 'x'
      then parse_hex (i + 2) (C.Concrete zero)
      else parse_dec i (C.Concrete zero)
    in
    require (len > 0);
    match s.[0] with
    | '+' -> parse_int 1
    | '-' ->
      let n = parse_int 1 in
      require (ge_s (C.Concrete (sub (to_bits n) one)) (C.Concrete minus_one));
      (check_unop_concrete Rep.neg) n
    | _ -> parse_int 0

  let of_string_s s =
    let n = of_string s in
    require (s.[0] = '-' || ge_s n (C.Concrete Rep.zero));
    n

  let of_string_u s =
    let n = of_string s in
    require (s.[0] != '+' && s.[0] != '-');
    n

  let is_concrete x = true
end
