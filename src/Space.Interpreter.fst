module Space.Interpreter

(** Space interpreter - fetch/execute cycle *)

open Space.Types
open Space.Stack
open Space.Universe
open Space.World
open Space.Instruction

(** Machine state *)
noeq type machine = {
  mworld: world;
  inst_ptr: ip;              (* Instruction pointer *)
  return_stack: list ip;     (* Return addresses *)
  running: bool;
  error: option string;
}

(** Initial machine state *)
let initial_machine : machine = {
  mworld = empty_world;
  inst_ptr = 0uL;
  return_stack = [];
  running = false;
  error = None;
}

(** Machine with error *)
let machine_error (m: machine) (msg: string) : machine =
  { m with running = false; error = Some msg }

(** Halt machine *)
let halt (m: machine) : machine =
  { m with running = false }

(** Advance instruction pointer *)
let advance_ip (m: machine) : machine =
  { m with inst_ptr = FStar.UInt64.add_mod m.inst_ptr 1uL }

(** Jump to address *)
let jump (m: machine) (target: ip) : machine =
  { m with inst_ptr = target }

(** Call: push return address and jump *)
let call (m: machine) (target: ip) : machine =
  let return_addr = FStar.UInt64.add_mod m.inst_ptr 1uL in
  { m with
    inst_ptr = target;
    return_stack = return_addr :: m.return_stack }

(** Return: pop return address and jump *)
let do_return (m: machine) : machine =
  match m.return_stack with
  | [] -> machine_error m "return stack underflow"
  | addr :: rest -> { m with inst_ptr = addr; return_stack = rest }

(** Get current universe from machine *)
let current_universe (m: machine) : option universe =
  get_current m.mworld

(** Update current universe in machine *)
let update_current (m: machine) (u: universe) : machine =
  { m with mworld = update_universe m.mworld u }
