(** @author TODO Ansh Godha (ag759) *)

(** [index c] is the 0-based index of [c] in the alphabet.
    Requires: [c] is an uppercase letter in A..Z. *)
let index (c:char) : int =
  (Char.code c) - 65

(** [+%] is a newly defined modulus operator that returns the remainder of 
    a / b if both a and b are positive. If a is negative, then the operator performs
    a compuation that 'loops' around the number scale that contains numbers from 0
    to b-1. For example, 28 +% 26 = 2, but -16 % 26 = 10
    Requires: a >= -b*)
let (+%) a b = 
  if (a < 0) then ((a+b) mod b) else (a mod b)

(** [map_r_to_l wiring top_letter input_pos] is the left-hand output position
    at which current would appear when current enters at right-hand input
    position [input_pos] to a rotor whose wiring specification is given by
    [wiring].  The orientation of the rotor is given by [top_letter], 
    which is the top letter appearing to the operator in the rotor's 
    present orientation.
    Requires: 
    	[wiring] is a valid wiring specification, 
    	[top_letter] is in A..Z, and 
    	[input_pos] is in 0..25. *)
let map_r_to_l (wiring:string) (top_letter:char) (input_pos:int) : int =
  let offset = index top_letter in
  let right_contact = (input_pos + offset) +% 26 in
  let mid = index wiring.[right_contact] in
  (mid - offset) +% 26

(** [map_l_to_r] computes the same function as [map_r_to_l], except 
    for current flowing left to right. *)
let map_l_to_r (wiring:string) (top_letter:char) (input_pos:int) : int =
  let offset = index top_letter in
  let left_contact = (input_pos + offset) +% 26 in
  let mid = Char.chr (left_contact + 65) |> String.index wiring in
  (mid - offset) +% 26

(** [map_refl wiring input_pos] is the output position at which current would 
    appear when current enters at input position [input_pos] to a reflector 
    whose wiring specification is given by [wiring].
    Requires: 
    	[wiring] is a valid reflector specification, and 
      [input_pos] is in 0..25. *)
let map_refl (wiring:string) (input_pos:int) : int =
  map_r_to_l wiring 'A' input_pos

(** [map_plug plugs c] is the letter to which [c] is transformed
    by the plugboard [plugs].
    Requires:
      [plugs] is a valid plugboard, and
      [c] is in A..Z. *)
let rec map_plug (plugs:(char*char) list) (c:char) =
  match plugs with
  | [] -> c
  | h :: t -> if (fst h = c) then snd h 
    else if (snd h = c) then fst h 
    else map_plug t c

(** [rotor] represents an Enigma rotor. *)
type rotor = {
  (** A valid rotor wiring specification. *)
  wiring : string;
  (** The turnover of the rotor, which must be an uppercase letter.
      This field will not be used in the assignment until you 
      implement stepping in the excellent scope. *)
  turnover : char;
}

(** [oriented_rotor] represents a rotor that is installed on the spindle hence 
    has a top letter. *)
type oriented_rotor = {
  (** The rotor. *)
  rotor : rotor;
  (** The top letter showing on the rotor. *)
  top_letter : char;
}

(** [config] represents the configuration of an Enigma machine. *)
type config = {
  (** A valid reflector wiring specification. *)
  refl : string;
  (** The rotors as they are installed on the spindle from left to right.
      There may be any number of elements in this list: 0, 1, 2, 3, 4, 5, etc.
      The order of elements in list represents the order in which the rotors
      are installed on the spindle, **from left to right**. So, the head of the
      list is the leftmost rotor on the spindle, and the last element of the
      list is the rightmost rotor on the spindle.  *)
  rotors : oriented_rotor list;
  (** A valid plugboard. The order of characters in the pairs does not matter,
      and the order of pairs in the list does not matter. *)
  plugboard : (char*char) list;
}

(**IMP: rev_orl is the reversed  oriented rotor list*)
let rec map_rotors_r_to_l (rev_orl: oriented_rotor list) (input_index: int) : int =
  match rev_orl with
  | [] -> input_index
  | rotor::t -> map_r_to_l (rotor.rotor.wiring) (rotor.top_letter) input_index |> map_rotors_r_to_l t

let rec map_rotors_l_to_r (orl: oriented_rotor list) (input_index: int) : int =
  match orl with
  | [] -> input_index
  | rotor::t -> map_l_to_r (rotor.rotor.wiring) (rotor.top_letter) input_index |> map_rotors_r_to_l t

(** [cipher_char config c] is the letter to which the Enigma machine 
    ciphers input [c] when it is in configuration [config].
    Requires:
      [config] is a valid configuration, and
      [c] is in A..Z. *)
let cipher_char (config:config) (c:char) : char =
  let finalIndex = c |> map_plug config.plugboard |> index |> map_rotors_r_to_l (List.rev config.rotors) |> map_refl config.refl |> map_rotors_l_to_r config.rotors in
  let ltr = Char.chr (finalIndex + 65) in 
  map_plug config.plugboard ltr

(** [step config] is the new configuration to which the Enigma machine 
    transitions when it steps beginning in configuration [config].
    Requires: [config] is a valid configuration. *)
let step (config:config) : config =
  failwith "Unimplemented"

(** [cipher config s] is the string to which [s] enciphers
    when the Enigma machine begins in configuration [config].
    Requires:
      [config] is a valid configuration, and
      [s] contains only uppercase letters. *)
let rec cipher (config:config) (s:string) : string =
  failwith "Unimplemented"

(* TODO: set the value below *)
let hours_worked = -1
