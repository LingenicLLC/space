open Prims
type case_type =
  | Uppercase 
  | Lowercase 
  | Titlecase 
let uu___is_Uppercase (projectee : case_type) : Prims.bool=
  match projectee with | Uppercase -> true | uu___ -> false
let uu___is_Lowercase (projectee : case_type) : Prims.bool=
  match projectee with | Lowercase -> true | uu___ -> false
let uu___is_Titlecase (projectee : case_type) : Prims.bool=
  match projectee with | Titlecase -> true | uu___ -> false
type simple_case_mapping =
  {
  codepoint: Prims.nat ;
  uppercase: Prims.nat ;
  lowercase: Prims.nat ;
  titlecase: Prims.nat }
let __proj__Mksimple_case_mapping__item__codepoint
  (projectee : simple_case_mapping) : Prims.nat=
  match projectee with
  | { codepoint; uppercase; lowercase; titlecase;_} -> codepoint
let __proj__Mksimple_case_mapping__item__uppercase
  (projectee : simple_case_mapping) : Prims.nat=
  match projectee with
  | { codepoint; uppercase; lowercase; titlecase;_} -> uppercase
let __proj__Mksimple_case_mapping__item__lowercase
  (projectee : simple_case_mapping) : Prims.nat=
  match projectee with
  | { codepoint; uppercase; lowercase; titlecase;_} -> lowercase
let __proj__Mksimple_case_mapping__item__titlecase
  (projectee : simple_case_mapping) : Prims.nat=
  match projectee with
  | { codepoint; uppercase; lowercase; titlecase;_} -> titlecase
type special_case_mapping =
  {
  codepoint1: Prims.nat ;
  condition: Prims.string ;
  uppercase1: Prims.nat Prims.list ;
  lowercase1: Prims.nat Prims.list ;
  titlecase1: Prims.nat Prims.list }
let __proj__Mkspecial_case_mapping__item__codepoint
  (projectee : special_case_mapping) : Prims.nat=
  match projectee with
  | { codepoint1 = codepoint; condition; uppercase1 = uppercase;
      lowercase1 = lowercase; titlecase1 = titlecase;_} -> codepoint
let __proj__Mkspecial_case_mapping__item__condition
  (projectee : special_case_mapping) : Prims.string=
  match projectee with
  | { codepoint1 = codepoint; condition; uppercase1 = uppercase;
      lowercase1 = lowercase; titlecase1 = titlecase;_} -> condition
let __proj__Mkspecial_case_mapping__item__uppercase
  (projectee : special_case_mapping) : Prims.nat Prims.list=
  match projectee with
  | { codepoint1 = codepoint; condition; uppercase1 = uppercase;
      lowercase1 = lowercase; titlecase1 = titlecase;_} -> uppercase
let __proj__Mkspecial_case_mapping__item__lowercase
  (projectee : special_case_mapping) : Prims.nat Prims.list=
  match projectee with
  | { codepoint1 = codepoint; condition; uppercase1 = uppercase;
      lowercase1 = lowercase; titlecase1 = titlecase;_} -> lowercase
let __proj__Mkspecial_case_mapping__item__titlecase
  (projectee : special_case_mapping) : Prims.nat Prims.list=
  match projectee with
  | { codepoint1 = codepoint; condition; uppercase1 = uppercase;
      lowercase1 = lowercase; titlecase1 = titlecase;_} -> titlecase
let simple_uppercase (cp : Prims.nat) : Prims.nat=
  match Space_Text_UCD_Types.lookup_mapping cp
          Space_Text_UCD_Case.uppercase_mappings
  with
  | FStar_Pervasives_Native.Some mapped -> mapped
  | FStar_Pervasives_Native.None -> cp
let simple_lowercase (cp : Prims.nat) : Prims.nat=
  match Space_Text_UCD_Types.lookup_mapping cp
          Space_Text_UCD_Case.lowercase_mappings
  with
  | FStar_Pervasives_Native.Some mapped -> mapped
  | FStar_Pervasives_Native.None -> cp
let simple_titlecase (cp : Prims.nat) : Prims.nat=
  if cp = (Prims.of_int (0x01C6))
  then (Prims.of_int (0x01C5))
  else
    if cp = (Prims.of_int (0x01C9))
    then (Prims.of_int (0x01C8))
    else
      if cp = (Prims.of_int (0x01CC))
      then (Prims.of_int (0x01CB))
      else
        if cp = (Prims.of_int (0x01F3))
        then (Prims.of_int (0x01F2))
        else simple_uppercase cp
let has_case (cp : Prims.nat) : Prims.bool=
  ((simple_uppercase cp) <> cp) || ((simple_lowercase cp) <> cp)
let is_uppercase (cp : Prims.nat) : Prims.bool=
  (has_case cp) && ((simple_uppercase cp) = cp)
let is_lowercase (cp : Prims.nat) : Prims.bool=
  (has_case cp) && ((simple_lowercase cp) = cp)
let rec lookup_special_casing (cp : Prims.nat)
  (table : Space_Text_UCD_Types.special_case_entry Prims.list) :
  Space_Text_UCD_Types.special_case_entry FStar_Pervasives_Native.option=
  match table with
  | [] -> FStar_Pervasives_Native.None
  | entry::rest ->
      if entry.Space_Text_UCD_Types.codepoint4 = cp
      then FStar_Pervasives_Native.Some entry
      else lookup_special_casing cp rest
let full_uppercase (cp : Prims.nat) : Prims.nat Prims.list=
  match lookup_special_casing cp Space_Text_UCD_Case.special_casing_table
  with
  | FStar_Pervasives_Native.Some entry ->
      if Prims.uu___is_Cons entry.Space_Text_UCD_Types.upper
      then entry.Space_Text_UCD_Types.upper
      else [simple_uppercase cp]
  | FStar_Pervasives_Native.None -> [simple_uppercase cp]
let full_lowercase (cp : Prims.nat) : Prims.nat Prims.list=
  match lookup_special_casing cp Space_Text_UCD_Case.special_casing_table
  with
  | FStar_Pervasives_Native.Some entry ->
      if Prims.uu___is_Cons entry.Space_Text_UCD_Types.lower
      then entry.Space_Text_UCD_Types.lower
      else [simple_lowercase cp]
  | FStar_Pervasives_Native.None -> [simple_lowercase cp]
let full_titlecase (cp : Prims.nat) : Prims.nat Prims.list=
  match lookup_special_casing cp Space_Text_UCD_Case.special_casing_table
  with
  | FStar_Pervasives_Native.Some entry ->
      if Prims.uu___is_Cons entry.Space_Text_UCD_Types.title
      then entry.Space_Text_UCD_Types.title
      else [simple_titlecase cp]
  | FStar_Pervasives_Native.None -> [simple_titlecase cp]
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
let map_case (cps : Prims.nat Prims.list)
  (f : Prims.nat -> Prims.nat Prims.list) : Prims.nat Prims.list=
  FStar_List_Tot_Base.concatMap f cps
let text_to_upper (t : Space_Text_Types.text) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  let cps = bytes_to_codepoints t.Space_Text_Types.data in
  let upper_cps = map_case cps full_uppercase in
  let bytes = codepoints_to_bytes upper_cps in
  Space_Text_Create.text_from_bytes bytes
let text_to_lower (t : Space_Text_Types.text) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  let cps = bytes_to_codepoints t.Space_Text_Types.data in
  let lower_cps = map_case cps full_lowercase in
  let bytes = codepoints_to_bytes lower_cps in
  Space_Text_Create.text_from_bytes bytes
let is_word_boundary (cp : Prims.nat) : Prims.bool=
  (((((((((((((((((((((((((((((((((cp = (Prims.of_int (0x20))) ||
                                    (cp = (Prims.of_int (0x09))))
                                   || (cp = (Prims.of_int (0x0A))))
                                  || (cp = (Prims.of_int (0x0D))))
                                 || (cp = (Prims.of_int (0x0B))))
                                || (cp = (Prims.of_int (0x0C))))
                               || (cp = (Prims.of_int (0xA0))))
                              || (cp = (Prims.of_int (0x2007))))
                             || (cp = (Prims.of_int (0x202F))))
                            ||
                            ((cp >= (Prims.of_int (0x2000))) &&
                               (cp <= (Prims.of_int (0x200A)))))
                           || (cp = (Prims.of_int (0x2028))))
                          || (cp = (Prims.of_int (0x2029))))
                         || (cp = (Prims.of_int (0x2D))))
                        || (cp = (Prims.of_int (0x2014))))
                       || (cp = (Prims.of_int (0x2013))))
                      || (cp = (Prims.of_int (0x27))))
                     || (cp = (Prims.of_int (0x2019))))
                    || (cp = (Prims.of_int (0x2E))))
                   || (cp = (Prims.of_int (0x2C))))
                  || (cp = (Prims.of_int (0x3B))))
                 || (cp = (Prims.of_int (0x3A))))
                || (cp = (Prims.of_int (0x21))))
               || (cp = (Prims.of_int (0x3F))))
              || (cp = (Prims.of_int (0x28))))
             || (cp = (Prims.of_int (0x29))))
            || (cp = (Prims.of_int (0x5B))))
           || (cp = (Prims.of_int (0x5D))))
          || (cp = (Prims.of_int (0x7B))))
         || (cp = (Prims.of_int (0x7D))))
        || (cp = (Prims.of_int (0x22))))
       || (cp = (Prims.of_int (0x201C))))
      || (cp = (Prims.of_int (0x201D))))
     || (cp = (Prims.of_int (0x2F))))
    || (cp = (Prims.of_int (0x5C)))
let is_word_continue (cp : Prims.nat) : Prims.bool=
  (((has_case cp) || (cp = (Prims.of_int (0x27)))) ||
     (cp = (Prims.of_int (0x2019))))
    || ((cp >= (Prims.of_int (0x30))) && (cp <= (Prims.of_int (0x39))))
let text_to_title (t : Space_Text_Types.text) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  let cps = bytes_to_codepoints t.Space_Text_Types.data in
  let rec titlecase_aux cps1 at_word_start =
    match cps1 with
    | [] -> []
    | cp::rest ->
        let is_cased = has_case cp in
        let is_boundary = is_word_boundary cp in
        let continues_word = is_word_continue cp in
        if at_word_start && is_cased
        then
          FStar_List_Tot_Base.op_At (full_titlecase cp)
            (titlecase_aux rest false)
        else
          if is_boundary
          then FStar_List_Tot_Base.op_At [cp] (titlecase_aux rest true)
          else
            if is_cased
            then
              FStar_List_Tot_Base.op_At (full_lowercase cp)
                (titlecase_aux rest false)
            else
              if continues_word
              then FStar_List_Tot_Base.op_At [cp] (titlecase_aux rest false)
              else
                FStar_List_Tot_Base.op_At [cp]
                  (titlecase_aux rest at_word_start) in
  let title_cps = titlecase_aux cps true in
  let bytes = codepoints_to_bytes title_cps in
  Space_Text_Create.text_from_bytes bytes
let casefold_codepoint (cp : Prims.nat) : Prims.nat Prims.list=
  if cp = (Prims.of_int (0x00DF))
  then [(Prims.of_int (0x0073)); (Prims.of_int (0x0073))]
  else
    if cp = (Prims.of_int (0x0130))
    then [(Prims.of_int (0x0069)); (Prims.of_int (0x0307))]
    else
      if cp = (Prims.of_int (0x0149))
      then [(Prims.of_int (0x02BC)); (Prims.of_int (0x006E))]
      else
        if cp = (Prims.of_int (0x01F0))
        then [(Prims.of_int (0x006A)); (Prims.of_int (0x030C))]
        else
          if cp = (Prims.of_int (0x0390))
          then
            [(Prims.of_int (0x03B9));
            (Prims.of_int (0x0308));
            (Prims.of_int (0x0301))]
          else
            if cp = (Prims.of_int (0x03B0))
            then
              [(Prims.of_int (0x03C5));
              (Prims.of_int (0x0308));
              (Prims.of_int (0x0301))]
            else
              if cp = (Prims.of_int (0x1E96))
              then [(Prims.of_int (0x0068)); (Prims.of_int (0x0331))]
              else
                if cp = (Prims.of_int (0x1E97))
                then [(Prims.of_int (0x0074)); (Prims.of_int (0x0308))]
                else
                  if cp = (Prims.of_int (0x1E98))
                  then [(Prims.of_int (0x0077)); (Prims.of_int (0x030A))]
                  else
                    if cp = (Prims.of_int (0x1E99))
                    then [(Prims.of_int (0x0079)); (Prims.of_int (0x030A))]
                    else
                      if cp = (Prims.of_int (0x1E9A))
                      then [(Prims.of_int (0x0061)); (Prims.of_int (0x02BE))]
                      else
                        if cp = (Prims.of_int (0x1E9E))
                        then
                          [(Prims.of_int (0x0073)); (Prims.of_int (0x0073))]
                        else
                          if cp = (Prims.of_int (0x1F50))
                          then
                            [(Prims.of_int (0x03C5));
                            (Prims.of_int (0x0313))]
                          else
                            if cp = (Prims.of_int (0x1F52))
                            then
                              [(Prims.of_int (0x03C5));
                              (Prims.of_int (0x0313));
                              (Prims.of_int (0x0300))]
                            else
                              if cp = (Prims.of_int (0x1F54))
                              then
                                [(Prims.of_int (0x03C5));
                                (Prims.of_int (0x0313));
                                (Prims.of_int (0x0301))]
                              else
                                if cp = (Prims.of_int (0x1F56))
                                then
                                  [(Prims.of_int (0x03C5));
                                  (Prims.of_int (0x0313));
                                  (Prims.of_int (0x0342))]
                                else
                                  if cp = (Prims.of_int (0x1F80))
                                  then
                                    [(Prims.of_int (0x1F00));
                                    (Prims.of_int (0x03B9))]
                                  else
                                    if cp = (Prims.of_int (0x1F81))
                                    then
                                      [(Prims.of_int (0x1F01));
                                      (Prims.of_int (0x03B9))]
                                    else
                                      if cp = (Prims.of_int (0x1F82))
                                      then
                                        [(Prims.of_int (0x1F02));
                                        (Prims.of_int (0x03B9))]
                                      else
                                        if cp = (Prims.of_int (0x1F83))
                                        then
                                          [(Prims.of_int (0x1F03));
                                          (Prims.of_int (0x03B9))]
                                        else
                                          if cp = (Prims.of_int (0x1F84))
                                          then
                                            [(Prims.of_int (0x1F04));
                                            (Prims.of_int (0x03B9))]
                                          else
                                            if cp = (Prims.of_int (0x1F85))
                                            then
                                              [(Prims.of_int (0x1F05));
                                              (Prims.of_int (0x03B9))]
                                            else
                                              if cp = (Prims.of_int (0x1F86))
                                              then
                                                [(Prims.of_int (0x1F06));
                                                (Prims.of_int (0x03B9))]
                                              else
                                                if
                                                  cp =
                                                    (Prims.of_int (0x1F87))
                                                then
                                                  [(Prims.of_int (0x1F07));
                                                  (Prims.of_int (0x03B9))]
                                                else
                                                  if
                                                    cp =
                                                      (Prims.of_int (0x1F88))
                                                  then
                                                    [(Prims.of_int (0x1F00));
                                                    (Prims.of_int (0x03B9))]
                                                  else
                                                    if
                                                      cp =
                                                        (Prims.of_int (0x1F89))
                                                    then
                                                      [(Prims.of_int (0x1F01));
                                                      (Prims.of_int (0x03B9))]
                                                    else
                                                      if
                                                        cp =
                                                          (Prims.of_int (0x1F8A))
                                                      then
                                                        [(Prims.of_int (0x1F02));
                                                        (Prims.of_int (0x03B9))]
                                                      else
                                                        if
                                                          cp =
                                                            (Prims.of_int (0x1F8B))
                                                        then
                                                          [(Prims.of_int (0x1F03));
                                                          (Prims.of_int (0x03B9))]
                                                        else
                                                          if
                                                            cp =
                                                              (Prims.of_int (0x1F8C))
                                                          then
                                                            [(Prims.of_int (0x1F04));
                                                            (Prims.of_int (0x03B9))]
                                                          else
                                                            if
                                                              cp =
                                                                (Prims.of_int (0x1F8D))
                                                            then
                                                              [(Prims.of_int (0x1F05));
                                                              (Prims.of_int (0x03B9))]
                                                            else
                                                              if
                                                                cp =
                                                                  (Prims.of_int (0x1F8E))
                                                              then
                                                                [(Prims.of_int (0x1F06));
                                                                (Prims.of_int (0x03B9))]
                                                              else
                                                                if
                                                                  cp =
                                                                    (Prims.of_int (0x1F8F))
                                                                then
                                                                  [(Prims.of_int (0x1F07));
                                                                  (Prims.of_int (0x03B9))]
                                                                else
                                                                  if
                                                                    cp =
                                                                    (Prims.of_int (0x1FB2))
                                                                  then
                                                                    [(Prims.of_int (0x1F70));
                                                                    (Prims.of_int (0x03B9))]
                                                                  else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FB3))
                                                                    then
                                                                    [(Prims.of_int (0x03B1));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FB4))
                                                                    then
                                                                    [(Prims.of_int (0x03AC));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FB6))
                                                                    then
                                                                    [(Prims.of_int (0x03B1));
                                                                    (Prims.of_int (0x0342))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FB7))
                                                                    then
                                                                    [(Prims.of_int (0x03B1));
                                                                    (Prims.of_int (0x0342));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FBC))
                                                                    then
                                                                    [(Prims.of_int (0x03B1));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FC2))
                                                                    then
                                                                    [(Prims.of_int (0x1F74));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FC3))
                                                                    then
                                                                    [(Prims.of_int (0x03B7));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FC4))
                                                                    then
                                                                    [(Prims.of_int (0x03AE));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FC6))
                                                                    then
                                                                    [(Prims.of_int (0x03B7));
                                                                    (Prims.of_int (0x0342))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FC7))
                                                                    then
                                                                    [(Prims.of_int (0x03B7));
                                                                    (Prims.of_int (0x0342));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FCC))
                                                                    then
                                                                    [(Prims.of_int (0x03B7));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FD2))
                                                                    then
                                                                    [(Prims.of_int (0x03B9));
                                                                    (Prims.of_int (0x0308));
                                                                    (Prims.of_int (0x0300))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FD3))
                                                                    then
                                                                    [(Prims.of_int (0x03B9));
                                                                    (Prims.of_int (0x0308));
                                                                    (Prims.of_int (0x0301))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FD6))
                                                                    then
                                                                    [(Prims.of_int (0x03B9));
                                                                    (Prims.of_int (0x0342))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FD7))
                                                                    then
                                                                    [(Prims.of_int (0x03B9));
                                                                    (Prims.of_int (0x0308));
                                                                    (Prims.of_int (0x0342))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FE2))
                                                                    then
                                                                    [(Prims.of_int (0x03C5));
                                                                    (Prims.of_int (0x0308));
                                                                    (Prims.of_int (0x0300))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FE3))
                                                                    then
                                                                    [(Prims.of_int (0x03C5));
                                                                    (Prims.of_int (0x0308));
                                                                    (Prims.of_int (0x0301))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FE4))
                                                                    then
                                                                    [(Prims.of_int (0x03C1));
                                                                    (Prims.of_int (0x0313))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FE6))
                                                                    then
                                                                    [(Prims.of_int (0x03C5));
                                                                    (Prims.of_int (0x0342))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FE7))
                                                                    then
                                                                    [(Prims.of_int (0x03C5));
                                                                    (Prims.of_int (0x0308));
                                                                    (Prims.of_int (0x0342))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FF2))
                                                                    then
                                                                    [(Prims.of_int (0x1F7C));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FF3))
                                                                    then
                                                                    [(Prims.of_int (0x03C9));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FF4))
                                                                    then
                                                                    [(Prims.of_int (0x03CE));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FF6))
                                                                    then
                                                                    [(Prims.of_int (0x03C9));
                                                                    (Prims.of_int (0x0342))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FF7))
                                                                    then
                                                                    [(Prims.of_int (0x03C9));
                                                                    (Prims.of_int (0x0342));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x1FFC))
                                                                    then
                                                                    [(Prims.of_int (0x03C9));
                                                                    (Prims.of_int (0x03B9))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0xFB00))
                                                                    then
                                                                    [(Prims.of_int (0x0066));
                                                                    (Prims.of_int (0x0066))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0xFB01))
                                                                    then
                                                                    [(Prims.of_int (0x0066));
                                                                    (Prims.of_int (0x0069))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0xFB02))
                                                                    then
                                                                    [(Prims.of_int (0x0066));
                                                                    (Prims.of_int (0x006C))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0xFB03))
                                                                    then
                                                                    [(Prims.of_int (0x0066));
                                                                    (Prims.of_int (0x0066));
                                                                    (Prims.of_int (0x0069))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0xFB04))
                                                                    then
                                                                    [(Prims.of_int (0x0066));
                                                                    (Prims.of_int (0x0066));
                                                                    (Prims.of_int (0x006C))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0xFB05))
                                                                    then
                                                                    [(Prims.of_int (0x0073));
                                                                    (Prims.of_int (0x0074))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0xFB06))
                                                                    then
                                                                    [(Prims.of_int (0x0073));
                                                                    (Prims.of_int (0x0074))]
                                                                    else
                                                                    if
                                                                    cp =
                                                                    (Prims.of_int (0x017F))
                                                                    then
                                                                    [(Prims.of_int (0x0073))]
                                                                    else
                                                                    [
                                                                    simple_lowercase
                                                                    cp]
let text_casefold (t : Space_Text_Types.text) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  let cps = bytes_to_codepoints t.Space_Text_Types.data in
  let folded = FStar_List_Tot_Base.concatMap casefold_codepoint cps in
  let bytes = codepoints_to_bytes folded in
  Space_Text_Create.text_from_bytes bytes
let text_equal_ignore_case (t1 : Space_Text_Types.text)
  (t2 : Space_Text_Types.text) : Prims.bool=
  match ((text_casefold t1), (text_casefold t2)) with
  | (FStar_Pervasives_Native.Some f1, FStar_Pervasives_Native.Some f2) ->
      ((f1.Space_Text_Types.header).Space_Text_Types.byte_length =
         (f2.Space_Text_Types.header).Space_Text_Types.byte_length)
        && (f1.Space_Text_Types.data = f2.Space_Text_Types.data)
  | uu___ -> false
let text_is_upper (t : Space_Text_Types.text) : Prims.bool=
  let cps = bytes_to_codepoints t.Space_Text_Types.data in
  FStar_List_Tot_Base.for_all
    (fun cp -> (Prims.op_Negation (has_case cp)) || (is_uppercase cp)) cps
let text_is_lower (t : Space_Text_Types.text) : Prims.bool=
  let cps = bytes_to_codepoints t.Space_Text_Types.data in
  FStar_List_Tot_Base.for_all
    (fun cp -> (Prims.op_Negation (has_case cp)) || (is_lowercase cp)) cps
let text_capitalize (t : Space_Text_Types.text) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  let cps = bytes_to_codepoints t.Space_Text_Types.data in
  match cps with
  | [] -> FStar_Pervasives_Native.Some t
  | first::rest ->
      let cap_cps =
        FStar_List_Tot_Base.op_At (full_titlecase first)
          (map_case rest full_lowercase) in
      let bytes = codepoints_to_bytes cap_cps in
      Space_Text_Create.text_from_bytes bytes
let text_swapcase (t : Space_Text_Types.text) :
  Space_Text_Types.text FStar_Pervasives_Native.option=
  let cps = bytes_to_codepoints t.Space_Text_Types.data in
  let swap cp =
    if is_uppercase cp
    then full_lowercase cp
    else if is_lowercase cp then full_uppercase cp else [cp] in
  let swapped = map_case cps swap in
  let bytes = codepoints_to_bytes swapped in
  Space_Text_Create.text_from_bytes bytes
