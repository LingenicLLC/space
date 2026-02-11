open Prims
type normalization_form =
  | NFC 
  | NFD 
  | NFKC 
  | NFKD 
let uu___is_NFC (projectee : normalization_form) : Prims.bool=
  match projectee with | NFC -> true | uu___ -> false
let uu___is_NFD (projectee : normalization_form) : Prims.bool=
  match projectee with | NFD -> true | uu___ -> false
let uu___is_NFKC (projectee : normalization_form) : Prims.bool=
  match projectee with | NFKC -> true | uu___ -> false
let uu___is_NFKD (projectee : normalization_form) : Prims.bool=
  match projectee with | NFKD -> true | uu___ -> false
type combining_class = Prims.nat
type decomposition_type =
  | Canonical 
  | Compatibility 
let uu___is_Canonical (projectee : decomposition_type) : Prims.bool=
  match projectee with | Canonical -> true | uu___ -> false
let uu___is_Compatibility (projectee : decomposition_type) : Prims.bool=
  match projectee with | Compatibility -> true | uu___ -> false
type decomposition_entry =
  {
  codepoint: Prims.nat ;
  decomp_type: decomposition_type ;
  decomposition: Prims.nat Prims.list }
let __proj__Mkdecomposition_entry__item__codepoint
  (projectee : decomposition_entry) : Prims.nat=
  match projectee with
  | { codepoint; decomp_type; decomposition;_} -> codepoint
let __proj__Mkdecomposition_entry__item__decomp_type
  (projectee : decomposition_entry) : decomposition_type=
  match projectee with
  | { codepoint; decomp_type; decomposition;_} -> decomp_type
let __proj__Mkdecomposition_entry__item__decomposition
  (projectee : decomposition_entry) : Prims.nat Prims.list=
  match projectee with
  | { codepoint; decomp_type; decomposition;_} -> decomposition
type composition_entry =
  {
  first: Prims.nat ;
  second: Prims.nat ;
  composed: Prims.nat }
let __proj__Mkcomposition_entry__item__first (projectee : composition_entry)
  : Prims.nat= match projectee with | { first; second; composed;_} -> first
let __proj__Mkcomposition_entry__item__second (projectee : composition_entry)
  : Prims.nat= match projectee with | { first; second; composed;_} -> second
let __proj__Mkcomposition_entry__item__composed
  (projectee : composition_entry) : Prims.nat=
  match projectee with | { first; second; composed;_} -> composed
type quick_check =
  | QC_Yes 
  | QC_No 
  | QC_Maybe 
let uu___is_QC_Yes (projectee : quick_check) : Prims.bool=
  match projectee with | QC_Yes -> true | uu___ -> false
let uu___is_QC_No (projectee : quick_check) : Prims.bool=
  match projectee with | QC_No -> true | uu___ -> false
let uu___is_QC_Maybe (projectee : quick_check) : Prims.bool=
  match projectee with | QC_Maybe -> true | uu___ -> false
let get_combining_class (cp : Prims.nat) : combining_class=
  let ccc =
    Space_Text_UCD_Types.lookup_ccc cp
      Space_Text_UCD_CCC.combining_class_table in
  if ccc <= (Prims.of_int (254)) then ccc else Prims.int_zero
let is_starter (cp : Prims.nat) : Prims.bool=
  (get_combining_class cp) = Prims.int_zero
let get_canonical_decomposition (cp : Prims.nat) :
  Prims.nat Prims.list FStar_Pervasives_Native.option=
  Space_Text_UCD_Types.lookup_decomp cp
    Space_Text_UCD_Decomp.canonical_decomposition_table
let get_compatibility_decomposition (cp : Prims.nat) :
  Prims.nat Prims.list FStar_Pervasives_Native.option=
  match get_canonical_decomposition cp with
  | FStar_Pervasives_Native.Some d -> FStar_Pervasives_Native.Some d
  | FStar_Pervasives_Native.None ->
      if cp = (Prims.of_int (0x00BC))
      then
        FStar_Pervasives_Native.Some
          [(Prims.of_int (0x0031));
          (Prims.of_int (0x2044));
          (Prims.of_int (0x0034))]
      else
        if cp = (Prims.of_int (0x00BD))
        then
          FStar_Pervasives_Native.Some
            [(Prims.of_int (0x0031));
            (Prims.of_int (0x2044));
            (Prims.of_int (0x0032))]
        else
          if cp = (Prims.of_int (0x00BE))
          then
            FStar_Pervasives_Native.Some
              [(Prims.of_int (0x0033));
              (Prims.of_int (0x2044));
              (Prims.of_int (0x0034))]
          else
            if cp = (Prims.of_int (0x2126))
            then FStar_Pervasives_Native.Some [(Prims.of_int (0x03A9))]
            else
              if cp = (Prims.of_int (0x212A))
              then FStar_Pervasives_Native.Some [(Prims.of_int (0x004B))]
              else
                if cp = (Prims.of_int (0x212B))
                then FStar_Pervasives_Native.Some [(Prims.of_int (0x00C5))]
                else
                  if
                    (cp >= (Prims.of_int (0xFF21))) &&
                      (cp <= (Prims.of_int (0xFF3A)))
                  then
                    FStar_Pervasives_Native.Some
                      [(cp - (Prims.of_int (0xFF21))) +
                         (Prims.of_int (0x0041))]
                  else
                    if
                      (cp >= (Prims.of_int (0xFF41))) &&
                        (cp <= (Prims.of_int (0xFF5A)))
                    then
                      FStar_Pervasives_Native.Some
                        [(cp - (Prims.of_int (0xFF41))) +
                           (Prims.of_int (0x0061))]
                    else FStar_Pervasives_Native.None
let get_composition (first : Prims.nat) (second : Prims.nat) :
  Prims.nat FStar_Pervasives_Native.option=
  Space_Text_UCD_Comp.try_compose first second
let decompose_one (cp : Prims.nat) (compat : Prims.bool) :
  Prims.nat Prims.list=
  let decomp =
    if compat
    then get_compatibility_decomposition cp
    else get_canonical_decomposition cp in
  match decomp with
  | FStar_Pervasives_Native.None -> [cp]
  | FStar_Pervasives_Native.Some cps -> cps
let rec decompose_pass (cps : Prims.nat Prims.list) (compat : Prims.bool)
  (fuel : Prims.nat) : Prims.nat Prims.list=
  if fuel = Prims.int_zero
  then cps
  else
    (let fuel' = fuel - Prims.int_one in
     let expanded =
       FStar_List_Tot_Base.concatMap (fun cp -> decompose_one cp compat) cps in
     if
       (FStar_List_Tot_Base.length expanded) =
         (FStar_List_Tot_Base.length cps)
     then cps
     else decompose_pass expanded compat fuel')
let full_decomposition (cp : Prims.nat) (compat : Prims.bool)
  (fuel : Prims.nat) : Prims.nat Prims.list= decompose_pass [cp] compat fuel
let decompose_codepoints (cps : Prims.nat Prims.list) (compat : Prims.bool) :
  Prims.nat Prims.list=
  FStar_List_Tot_Base.concatMap
    (fun cp -> full_decomposition cp compat (Prims.of_int (10))) cps
let rec insert_by_ccc (cp : Prims.nat) (sorted : Prims.nat Prims.list) :
  Prims.nat Prims.list=
  match sorted with
  | [] -> [cp]
  | x::rest ->
      let cp_ccc = get_combining_class cp in
      let x_ccc = get_combining_class x in
      if cp_ccc < x_ccc
      then cp :: x :: rest
      else x :: (insert_by_ccc cp rest)
let rec sort_by_ccc (cps : Prims.nat Prims.list) : Prims.nat Prims.list=
  match cps with | [] -> [] | cp::rest -> insert_by_ccc cp (sort_by_ccc rest)
let rec canonical_order_aux (cps : Prims.nat Prims.list)
  (acc : Prims.nat Prims.list) : Prims.nat Prims.list=
  match cps with
  | [] -> sort_by_ccc acc
  | cp::rest ->
      if
        (is_starter cp) &&
          ((FStar_List_Tot_Base.length acc) > Prims.int_zero)
      then
        FStar_List_Tot_Base.op_At (sort_by_ccc acc)
          (canonical_order_aux rest [cp])
      else canonical_order_aux rest (FStar_List_Tot_Base.op_At acc [cp])
let canonical_order (cps : Prims.nat Prims.list) : Prims.nat Prims.list=
  canonical_order_aux cps []
let rec compose_pass_aux (cps : Prims.nat Prims.list) (fuel : Prims.nat) :
  Prims.nat Prims.list=
  if fuel = Prims.int_zero
  then cps
  else
    (let fuel' = fuel - Prims.int_one in
     match cps with
     | [] -> []
     | cp::[] -> [cp]
     | first::second::rest ->
         (match get_composition first second with
          | FStar_Pervasives_Native.Some composed ->
              compose_pass_aux (composed :: rest) fuel'
          | FStar_Pervasives_Native.None -> first ::
              (compose_pass_aux (second :: rest) fuel')))
let compose_pass (cps : Prims.nat Prims.list) : Prims.nat Prims.list=
  compose_pass_aux cps ((FStar_List_Tot_Base.length cps) + Prims.int_one)
let rec compose_full (cps : Prims.nat Prims.list) (fuel : Prims.nat) :
  Prims.nat Prims.list=
  if fuel = Prims.int_zero
  then cps
  else
    (let fuel' = fuel - Prims.int_one in
     let composed = compose_pass cps in
     if
       (FStar_List_Tot_Base.length composed) =
         (FStar_List_Tot_Base.length cps)
     then cps
     else compose_full composed fuel')
let rec bytes_to_codepoints (bytes : FStar_UInt8.t Prims.list) :
  Prims.nat Prims.list=
  match bytes with
  | [] -> []
  | b0::rest ->
      let len = Space_Text_UTF8.sequence_length b0 in
      if len = Prims.int_zero
      then bytes_to_codepoints rest
      else
        if len = Prims.int_one
        then (FStar_UInt8.v b0) :: (bytes_to_codepoints rest)
        else
          if len = (Prims.of_int (2))
          then
            (match rest with
             | b1::rest' -> (Space_Text_UTF8.decode_codepoint_2 b0 b1) ::
                 (bytes_to_codepoints rest')
             | uu___2 -> [])
          else
            if len = (Prims.of_int (3))
            then
              (match rest with
               | b1::b2::rest' ->
                   (Space_Text_UTF8.decode_codepoint_3 b0 b1 b2) ::
                   (bytes_to_codepoints rest')
               | uu___3 -> [])
            else
              (match rest with
               | b1::b2::b3::rest' ->
                   (Space_Text_UTF8.decode_codepoint_4 b0 b1 b2 b3) ::
                   (bytes_to_codepoints rest')
               | uu___4 -> [])
let encode_codepoint (cp : Prims.nat) : FStar_UInt8.t Prims.list=
  if cp <= (Prims.of_int (0x7F))
  then [FStar_UInt8.uint_to_t cp]
  else
    if cp <= (Prims.of_int (0x7FF))
    then
      [FStar_UInt8.uint_to_t
         ((Prims.of_int (0xC0)) + (cp / (Prims.of_int (64))));
      FStar_UInt8.uint_to_t
        ((Prims.of_int (0x80)) + ((mod) cp (Prims.of_int (64))))]
    else
      if cp <= (Prims.parse_int "0xFFFF")
      then
        [FStar_UInt8.uint_to_t
           ((Prims.of_int (0xE0)) + (cp / (Prims.of_int (4096))));
        FStar_UInt8.uint_to_t
          ((Prims.of_int (0x80)) +
             ((mod) (cp / (Prims.of_int (64))) (Prims.of_int (64))));
        FStar_UInt8.uint_to_t
          ((Prims.of_int (0x80)) + ((mod) cp (Prims.of_int (64))))]
      else
        if cp <= (Prims.parse_int "0x10FFFF")
        then
          [FStar_UInt8.uint_to_t
             ((Prims.of_int (0xF0)) + (cp / (Prims.parse_int "262144")));
          FStar_UInt8.uint_to_t
            ((Prims.of_int (0x80)) +
               ((mod) (cp / (Prims.of_int (4096))) (Prims.of_int (64))));
          FStar_UInt8.uint_to_t
            ((Prims.of_int (0x80)) +
               ((mod) (cp / (Prims.of_int (64))) (Prims.of_int (64))));
          FStar_UInt8.uint_to_t
            ((Prims.of_int (0x80)) + ((mod) cp (Prims.of_int (64))))]
        else []
let codepoints_to_bytes (cps : Prims.nat Prims.list) :
  FStar_UInt8.t Prims.list=
  FStar_List_Tot_Base.concatMap encode_codepoint cps
let normalize_nfd (t : Space_Text_Types.text) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  let cps = bytes_to_codepoints t.Space_Text_Types.data in
  let decomposed = decompose_codepoints cps false in
  let ordered = canonical_order decomposed in
  let bytes = codepoints_to_bytes ordered in
  Space_Text_Create.text_from_bytes bytes
let normalize_nfc (t : Space_Text_Types.text) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  let cps = bytes_to_codepoints t.Space_Text_Types.data in
  let decomposed = decompose_codepoints cps false in
  let ordered = canonical_order decomposed in
  let composed = compose_full ordered (Prims.of_int (10)) in
  let bytes = codepoints_to_bytes composed in
  Space_Text_Create.text_from_bytes bytes
let normalize_nfkd (t : Space_Text_Types.text) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  let cps = bytes_to_codepoints t.Space_Text_Types.data in
  let decomposed = decompose_codepoints cps true in
  let ordered = canonical_order decomposed in
  let bytes = codepoints_to_bytes ordered in
  Space_Text_Create.text_from_bytes bytes
let normalize_nfkc (t : Space_Text_Types.text) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  let cps = bytes_to_codepoints t.Space_Text_Types.data in
  let decomposed = decompose_codepoints cps true in
  let ordered = canonical_order decomposed in
  let composed = compose_full ordered (Prims.of_int (10)) in
  let bytes = codepoints_to_bytes composed in
  Space_Text_Create.text_from_bytes bytes
let text_normalize (t : Space_Text_Types.text) (form : normalization_form) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  match form with
  | NFC -> normalize_nfc t
  | NFD -> normalize_nfd t
  | NFKC -> normalize_nfkc t
  | NFKD -> normalize_nfkd t
let is_normalized (t : Space_Text_Types.text) (form : normalization_form) :
  Prims.bool=
  match text_normalize t form with
  | FStar_Pervasives_Native.None -> false
  | FStar_Pervasives_Native.Some normalized ->
      ((t.Space_Text_Types.header).Space_Text_Types.byte_length =
         (normalized.Space_Text_Types.header).Space_Text_Types.byte_length)
        && (t.Space_Text_Types.data = normalized.Space_Text_Types.data)
