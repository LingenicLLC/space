open Prims
type text_handle = Prims.nat
type text_table =
  {
  texts: Space_Text_Types.text Prims.list ;
  next_handle: text_handle }
let __proj__Mktext_table__item__texts (projectee : text_table) :
  Space_Text_Types.text Prims.list=
  match projectee with | { texts; next_handle;_} -> texts
let __proj__Mktext_table__item__next_handle (projectee : text_table) :
  text_handle= match projectee with | { texts; next_handle;_} -> next_handle
let empty_text_table : text_table=
  { texts = []; next_handle = Prims.int_zero }
let add_text (tt : text_table) (t : Space_Text_Types.text) :
  (text_table * text_handle)=
  let h = tt.next_handle in
  ({
     texts = (FStar_List_Tot_Base.append tt.texts [t]);
     next_handle = (h + Prims.int_one)
   }, h)
let rec get_text_aux (ts : Space_Text_Types.text Prims.list) (h : Prims.nat)
  : Space_Text_Types.text FStar_Pervasives_Native.option=
  match ts with
  | [] -> FStar_Pervasives_Native.None
  | t::rest ->
      if h = Prims.int_zero
      then FStar_Pervasives_Native.Some t
      else get_text_aux rest (h - Prims.int_one)
let get_text (tt : text_table) (h : text_handle) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  if h >= tt.next_handle
  then FStar_Pervasives_Native.None
  else get_text_aux tt.texts h
let rec update_text_aux (ts : Space_Text_Types.text Prims.list)
  (h : Prims.nat) (t : Space_Text_Types.text) :
  Space_Text_Types.text Prims.list=
  match ts with
  | [] -> []
  | old::rest ->
      if h = Prims.int_zero
      then t :: rest
      else old :: (update_text_aux rest (h - Prims.int_one) t)
let update_text (tt : text_table) (h : text_handle)
  (t : Space_Text_Types.text) : text_table=
  { texts = (update_text_aux tt.texts h t); next_handle = (tt.next_handle) }
type text_warp_handle = Prims.nat
type text_warp_table =
  {
  warps: Space_Text_Warp.text_warp Prims.list ;
  next_handle1: text_warp_handle }
let __proj__Mktext_warp_table__item__warps (projectee : text_warp_table) :
  Space_Text_Warp.text_warp Prims.list=
  match projectee with | { warps; next_handle1 = next_handle;_} -> warps
let __proj__Mktext_warp_table__item__next_handle
  (projectee : text_warp_table) : text_warp_handle=
  match projectee with
  | { warps; next_handle1 = next_handle;_} -> next_handle
let empty_text_warp_table : text_warp_table=
  { warps = []; next_handle1 = Prims.int_zero }
let add_text_warp (twt : text_warp_table) (tw : Space_Text_Warp.text_warp) :
  (text_warp_table * text_warp_handle)=
  let h = twt.next_handle1 in
  ({
     warps = (FStar_List_Tot_Base.append twt.warps [tw]);
     next_handle1 = (h + Prims.int_one)
   }, h)
let rec get_text_warp_aux (tws : Space_Text_Warp.text_warp Prims.list)
  (h : Prims.nat) : Space_Text_Warp.text_warp FStar_Pervasives_Native.option=
  match tws with
  | [] -> FStar_Pervasives_Native.None
  | tw::rest ->
      if h = Prims.int_zero
      then FStar_Pervasives_Native.Some tw
      else get_text_warp_aux rest (h - Prims.int_one)
let get_text_warp (twt : text_warp_table) (h : text_warp_handle) :
  Space_Text_Warp.text_warp FStar_Pervasives_Native.option=
  if h >= twt.next_handle1
  then FStar_Pervasives_Native.None
  else get_text_warp_aux twt.warps h
let rec update_text_warp_aux (tws : Space_Text_Warp.text_warp Prims.list)
  (h : Prims.nat) (tw : Space_Text_Warp.text_warp) :
  Space_Text_Warp.text_warp Prims.list=
  match tws with
  | [] -> []
  | old::rest ->
      if h = Prims.int_zero
      then tw :: rest
      else old :: (update_text_warp_aux rest (h - Prims.int_one) tw)
let update_text_warp (twt : text_warp_table) (h : text_warp_handle)
  (tw : Space_Text_Warp.text_warp) : text_warp_table=
  {
    warps = (update_text_warp_aux twt.warps h tw);
    next_handle1 = (twt.next_handle1)
  }
let get_implicit_text_warp (twt : text_warp_table) :
  (Space_Text_Warp.text_warp * text_warp_handle)
    FStar_Pervasives_Native.option=
  if twt.next_handle1 = Prims.int_zero
  then FStar_Pervasives_Native.None
  else
    (let h = twt.next_handle1 - Prims.int_one in
     match get_text_warp twt h with
     | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
     | FStar_Pervasives_Native.Some tw ->
         FStar_Pervasives_Native.Some (tw, h))
type grapheme_handle = Prims.nat
type grapheme_table =
  {
  graphemes: Space_Text_Types.grapheme Prims.list ;
  next_handle2: grapheme_handle }
let __proj__Mkgrapheme_table__item__graphemes (projectee : grapheme_table) :
  Space_Text_Types.grapheme Prims.list=
  match projectee with
  | { graphemes; next_handle2 = next_handle;_} -> graphemes
let __proj__Mkgrapheme_table__item__next_handle (projectee : grapheme_table)
  : grapheme_handle=
  match projectee with
  | { graphemes; next_handle2 = next_handle;_} -> next_handle
let empty_grapheme_table : grapheme_table=
  { graphemes = []; next_handle2 = Prims.int_zero }
let add_grapheme (gt : grapheme_table) (g : Space_Text_Types.grapheme) :
  (grapheme_table * grapheme_handle)=
  let h = gt.next_handle2 in
  ({
     graphemes = (FStar_List_Tot_Base.append gt.graphemes [g]);
     next_handle2 = (h + Prims.int_one)
   }, h)
let rec get_grapheme_aux (gs : Space_Text_Types.grapheme Prims.list)
  (h : Prims.nat) : Space_Text_Types.grapheme FStar_Pervasives_Native.option=
  match gs with
  | [] -> FStar_Pervasives_Native.None
  | g::rest ->
      if h = Prims.int_zero
      then FStar_Pervasives_Native.Some g
      else get_grapheme_aux rest (h - Prims.int_one)
let get_grapheme (gt : grapheme_table) (h : grapheme_handle) :
  Space_Text_Types.grapheme FStar_Pervasives_Native.option=
  if h >= gt.next_handle2
  then FStar_Pervasives_Native.None
  else get_grapheme_aux gt.graphemes h
type bytes_metadata_entry = {
  base_addr: Prims.nat ;
  byte_length: Prims.nat }
let __proj__Mkbytes_metadata_entry__item__base_addr
  (projectee : bytes_metadata_entry) : Prims.nat=
  match projectee with | { base_addr; byte_length;_} -> base_addr
let __proj__Mkbytes_metadata_entry__item__byte_length
  (projectee : bytes_metadata_entry) : Prims.nat=
  match projectee with | { base_addr; byte_length;_} -> byte_length
type bytes_metadata = {
  entries: bytes_metadata_entry Prims.list }
let __proj__Mkbytes_metadata__item__entries (projectee : bytes_metadata) :
  bytes_metadata_entry Prims.list=
  match projectee with | { entries;_} -> entries
let empty_bytes_metadata : bytes_metadata= { entries = [] }
let record_bytes_alloc (bm : bytes_metadata) (addr : Prims.nat)
  (len : Prims.nat) : bytes_metadata=
  { entries = ({ base_addr = addr; byte_length = len } :: (bm.entries)) }
let rec lookup_bytes_len (entries : bytes_metadata_entry Prims.list)
  (addr : Prims.nat) : Prims.nat FStar_Pervasives_Native.option=
  match entries with
  | [] -> FStar_Pervasives_Native.None
  | e::rest ->
      if e.base_addr = addr
      then FStar_Pervasives_Native.Some (e.byte_length)
      else lookup_bytes_len rest addr
let get_bytes_len (bm : bytes_metadata) (addr : Prims.nat) :
  Prims.nat FStar_Pervasives_Native.option= lookup_bytes_len bm.entries addr
type machine =
  {
  mworld: Space_World.world ;
  borrows: Space_Borrow.borrow_state ;
  warps1: Space_Warp.warp_table ;
  texts1: text_table ;
  text_warps: text_warp_table ;
  graphemes1: grapheme_table ;
  bytes_meta: bytes_metadata ;
  output: Prims.nat Prims.list ;
  input: Prims.nat Prims.list ;
  inst_ptr: Space_Instruction.ip ;
  return_stack: Space_Instruction.ip Prims.list ;
  running: Prims.bool ;
  error: Prims.string FStar_Pervasives_Native.option }
let __proj__Mkmachine__item__mworld (projectee : machine) :
  Space_World.world=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> mworld
let __proj__Mkmachine__item__borrows (projectee : machine) :
  Space_Borrow.borrow_state=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> borrows
let __proj__Mkmachine__item__warps (projectee : machine) :
  Space_Warp.warp_table=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> warps
let __proj__Mkmachine__item__texts (projectee : machine) : text_table=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> texts
let __proj__Mkmachine__item__text_warps (projectee : machine) :
  text_warp_table=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> text_warps
let __proj__Mkmachine__item__graphemes (projectee : machine) :
  grapheme_table=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> graphemes
let __proj__Mkmachine__item__bytes_meta (projectee : machine) :
  bytes_metadata=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> bytes_meta
let __proj__Mkmachine__item__output (projectee : machine) :
  Prims.nat Prims.list=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> output
let __proj__Mkmachine__item__input (projectee : machine) :
  Prims.nat Prims.list=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> input
let __proj__Mkmachine__item__inst_ptr (projectee : machine) :
  Space_Instruction.ip=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> inst_ptr
let __proj__Mkmachine__item__return_stack (projectee : machine) :
  Space_Instruction.ip Prims.list=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> return_stack
let __proj__Mkmachine__item__running (projectee : machine) : Prims.bool=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> running
let __proj__Mkmachine__item__error (projectee : machine) :
  Prims.string FStar_Pervasives_Native.option=
  match projectee with
  | { mworld; borrows; warps1 = warps; texts1 = texts; text_warps;
      graphemes1 = graphemes; bytes_meta; output; input; inst_ptr;
      return_stack; running; error;_} -> error
let initial_machine : machine=
  {
    mworld = Space_World.empty_world;
    borrows = Space_Borrow.empty_borrow_state;
    warps1 = Space_Warp.empty_warp_table;
    texts1 = empty_text_table;
    text_warps = empty_text_warp_table;
    graphemes1 = empty_grapheme_table;
    bytes_meta = empty_bytes_metadata;
    output = [];
    input = [];
    inst_ptr = Stdint.Uint64.zero;
    return_stack = [];
    running = false;
    error = FStar_Pervasives_Native.None
  }
let machine_error (m : machine) (msg : Prims.string) : machine=
  {
    mworld = (m.mworld);
    borrows = (m.borrows);
    warps1 = (m.warps1);
    texts1 = (m.texts1);
    text_warps = (m.text_warps);
    graphemes1 = (m.graphemes1);
    bytes_meta = (m.bytes_meta);
    output = (m.output);
    input = (m.input);
    inst_ptr = (m.inst_ptr);
    return_stack = (m.return_stack);
    running = false;
    error = (FStar_Pervasives_Native.Some msg)
  }
let halt (m : machine) : machine=
  {
    mworld = (m.mworld);
    borrows = (m.borrows);
    warps1 = (m.warps1);
    texts1 = (m.texts1);
    text_warps = (m.text_warps);
    graphemes1 = (m.graphemes1);
    bytes_meta = (m.bytes_meta);
    output = (m.output);
    input = (m.input);
    inst_ptr = (m.inst_ptr);
    return_stack = (m.return_stack);
    running = false;
    error = (m.error)
  }
let advance_ip (m : machine) : machine=
  {
    mworld = (m.mworld);
    borrows = (m.borrows);
    warps1 = (m.warps1);
    texts1 = (m.texts1);
    text_warps = (m.text_warps);
    graphemes1 = (m.graphemes1);
    bytes_meta = (m.bytes_meta);
    output = (m.output);
    input = (m.input);
    inst_ptr = (FStar_UInt64.add_mod m.inst_ptr Stdint.Uint64.one);
    return_stack = (m.return_stack);
    running = (m.running);
    error = (m.error)
  }
let jump (m : machine) (target : Space_Instruction.ip) : machine=
  {
    mworld = (m.mworld);
    borrows = (m.borrows);
    warps1 = (m.warps1);
    texts1 = (m.texts1);
    text_warps = (m.text_warps);
    graphemes1 = (m.graphemes1);
    bytes_meta = (m.bytes_meta);
    output = (m.output);
    input = (m.input);
    inst_ptr = target;
    return_stack = (m.return_stack);
    running = (m.running);
    error = (m.error)
  }
let call (m : machine) (target : Space_Instruction.ip) : machine=
  let return_addr = FStar_UInt64.add_mod m.inst_ptr Stdint.Uint64.one in
  {
    mworld = (m.mworld);
    borrows = (m.borrows);
    warps1 = (m.warps1);
    texts1 = (m.texts1);
    text_warps = (m.text_warps);
    graphemes1 = (m.graphemes1);
    bytes_meta = (m.bytes_meta);
    output = (m.output);
    input = (m.input);
    inst_ptr = target;
    return_stack = (return_addr :: (m.return_stack));
    running = (m.running);
    error = (m.error)
  }
let do_return (m : machine) : machine=
  match m.return_stack with
  | [] -> machine_error m "return stack underflow"
  | addr::rest ->
      {
        mworld = (m.mworld);
        borrows = (m.borrows);
        warps1 = (m.warps1);
        texts1 = (m.texts1);
        text_warps = (m.text_warps);
        graphemes1 = (m.graphemes1);
        bytes_meta = (m.bytes_meta);
        output = (m.output);
        input = (m.input);
        inst_ptr = addr;
        return_stack = rest;
        running = (m.running);
        error = (m.error)
      }
let current_universe (m : machine) :
  Space_Universe.universe FStar_Pervasives_Native.option=
  Space_World.get_current m.mworld
let update_current (m : machine) (u : Space_Universe.universe) : machine=
  {
    mworld = (Space_World.update_universe m.mworld u);
    borrows = (m.borrows);
    warps1 = (m.warps1);
    texts1 = (m.texts1);
    text_warps = (m.text_warps);
    graphemes1 = (m.graphemes1);
    bytes_meta = (m.bytes_meta);
    output = (m.output);
    input = (m.input);
    inst_ptr = (m.inst_ptr);
    return_stack = (m.return_stack);
    running = (m.running);
    error = (m.error)
  }
