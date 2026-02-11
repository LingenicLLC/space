open Prims
type grapheme_break_property =
  | GBP_Other 
  | GBP_CR 
  | GBP_LF 
  | GBP_Control 
  | GBP_Extend 
  | GBP_ZWJ 
  | GBP_Regional_Indicator 
  | GBP_Prepend 
  | GBP_SpacingMark 
  | GBP_L 
  | GBP_V 
  | GBP_T 
  | GBP_LV 
  | GBP_LVT 
  | GBP_Extended_Pictographic 
let uu___is_GBP_Other (projectee : grapheme_break_property) : Prims.bool=
  match projectee with | GBP_Other -> true | uu___ -> false
let uu___is_GBP_CR (projectee : grapheme_break_property) : Prims.bool=
  match projectee with | GBP_CR -> true | uu___ -> false
let uu___is_GBP_LF (projectee : grapheme_break_property) : Prims.bool=
  match projectee with | GBP_LF -> true | uu___ -> false
let uu___is_GBP_Control (projectee : grapheme_break_property) : Prims.bool=
  match projectee with | GBP_Control -> true | uu___ -> false
let uu___is_GBP_Extend (projectee : grapheme_break_property) : Prims.bool=
  match projectee with | GBP_Extend -> true | uu___ -> false
let uu___is_GBP_ZWJ (projectee : grapheme_break_property) : Prims.bool=
  match projectee with | GBP_ZWJ -> true | uu___ -> false
let uu___is_GBP_Regional_Indicator (projectee : grapheme_break_property) :
  Prims.bool=
  match projectee with | GBP_Regional_Indicator -> true | uu___ -> false
let uu___is_GBP_Prepend (projectee : grapheme_break_property) : Prims.bool=
  match projectee with | GBP_Prepend -> true | uu___ -> false
let uu___is_GBP_SpacingMark (projectee : grapheme_break_property) :
  Prims.bool= match projectee with | GBP_SpacingMark -> true | uu___ -> false
let uu___is_GBP_L (projectee : grapheme_break_property) : Prims.bool=
  match projectee with | GBP_L -> true | uu___ -> false
let uu___is_GBP_V (projectee : grapheme_break_property) : Prims.bool=
  match projectee with | GBP_V -> true | uu___ -> false
let uu___is_GBP_T (projectee : grapheme_break_property) : Prims.bool=
  match projectee with | GBP_T -> true | uu___ -> false
let uu___is_GBP_LV (projectee : grapheme_break_property) : Prims.bool=
  match projectee with | GBP_LV -> true | uu___ -> false
let uu___is_GBP_LVT (projectee : grapheme_break_property) : Prims.bool=
  match projectee with | GBP_LVT -> true | uu___ -> false
let uu___is_GBP_Extended_Pictographic (projectee : grapheme_break_property) :
  Prims.bool=
  match projectee with | GBP_Extended_Pictographic -> true | uu___ -> false
type break_state =
  {
  prev_gbp: grapheme_break_property ;
  in_emoji_seq: Prims.bool ;
  ri_count: Prims.nat }
let __proj__Mkbreak_state__item__prev_gbp (projectee : break_state) :
  grapheme_break_property=
  match projectee with | { prev_gbp; in_emoji_seq; ri_count;_} -> prev_gbp
let __proj__Mkbreak_state__item__in_emoji_seq (projectee : break_state) :
  Prims.bool=
  match projectee with
  | { prev_gbp; in_emoji_seq; ri_count;_} -> in_emoji_seq
let __proj__Mkbreak_state__item__ri_count (projectee : break_state) :
  Prims.nat=
  match projectee with | { prev_gbp; in_emoji_seq; ri_count;_} -> ri_count
let initial_state : break_state=
  { prev_gbp = GBP_Other; in_emoji_seq = false; ri_count = Prims.int_zero }
let rec has_ccc_aux (table : Space_Text_UCD_Types.ccc_entry Prims.list)
  (cp : Prims.nat) : Prims.bool=
  match table with
  | [] -> false
  | e::rest ->
      if e.Space_Text_UCD_Types.codepoint2 = cp
      then e.Space_Text_UCD_Types.ccc > Prims.int_zero
      else has_ccc_aux rest cp
let has_combining_class (cp : Prims.nat) : Prims.bool=
  has_ccc_aux Space_Text_UCD_CCC.combining_class_table cp
let is_extend (cp : Prims.nat) : Prims.bool=
  (((((((((((((((((((((((((cp >= (Prims.of_int (0x0300))) &&
                            (cp <= (Prims.of_int (0x036F))))
                           ||
                           ((cp >= (Prims.of_int (0x1AB0))) &&
                              (cp <= (Prims.of_int (0x1AFF)))))
                          ||
                          ((cp >= (Prims.of_int (0x1DC0))) &&
                             (cp <= (Prims.of_int (0x1DFF)))))
                         ||
                         ((cp >= (Prims.of_int (0x20D0))) &&
                            (cp <= (Prims.of_int (0x20FF)))))
                        ||
                        ((cp >= (Prims.of_int (0xFE20))) &&
                           (cp <= (Prims.of_int (0xFE2F)))))
                       ||
                       ((cp >= (Prims.of_int (0x064B))) &&
                          (cp <= (Prims.of_int (0x065F)))))
                      ||
                      ((cp >= (Prims.of_int (0x0591))) &&
                         (cp <= (Prims.of_int (0x05BD)))))
                     || (cp = (Prims.of_int (0x05BF))))
                    ||
                    ((cp >= (Prims.of_int (0x05C1))) &&
                       (cp <= (Prims.of_int (0x05C2)))))
                   || (cp = (Prims.of_int (0x05C4))))
                  || (cp = (Prims.of_int (0x05C5))))
                 || (cp = (Prims.of_int (0x05C7))))
                ||
                ((cp >= (Prims.of_int (0x0901))) &&
                   (cp <= (Prims.of_int (0x0903)))))
               ||
               ((cp >= (Prims.of_int (0x093A))) &&
                  (cp <= (Prims.of_int (0x094F)))))
              ||
              ((cp >= (Prims.of_int (0x0951))) &&
                 (cp <= (Prims.of_int (0x0957)))))
             ||
             ((cp >= (Prims.of_int (0x0962))) &&
                (cp <= (Prims.of_int (0x0963)))))
            ||
            ((cp >= (Prims.of_int (0x0E31))) &&
               (cp <= (Prims.of_int (0x0E3A)))))
           ||
           ((cp >= (Prims.of_int (0x0E47))) &&
              (cp <= (Prims.of_int (0x0E4E)))))
          ||
          ((cp >= (Prims.of_int (0xFE00))) && (cp <= (Prims.of_int (0xFE0F)))))
         ||
         ((cp >= (Prims.parse_int "0xE0100")) &&
            (cp <= (Prims.parse_int "0xE01EF"))))
        ||
        ((cp >= (Prims.parse_int "0x1F3FB")) &&
           (cp <= (Prims.parse_int "0x1F3FF"))))
       || (cp = (Prims.of_int (0x034F))))
      || (cp = (Prims.of_int (0x200C))))
     || (cp = (Prims.of_int (0x200D))))
    || (has_combining_class cp)
let is_extended_pictographic (cp : Prims.nat) : Prims.bool=
  (((((((((((((((((((((((cp >= (Prims.parse_int "0x1F300")) &&
                          (cp <= (Prims.parse_int "0x1F5FF")))
                         ||
                         ((cp >= (Prims.parse_int "0x1F600")) &&
                            (cp <= (Prims.parse_int "0x1F64F"))))
                        ||
                        ((cp >= (Prims.parse_int "0x1F680")) &&
                           (cp <= (Prims.parse_int "0x1F6FF"))))
                       ||
                       ((cp >= (Prims.parse_int "0x1F900")) &&
                          (cp <= (Prims.parse_int "0x1F9FF"))))
                      ||
                      ((cp >= (Prims.parse_int "0x1FA00")) &&
                         (cp <= (Prims.parse_int "0x1FA6F"))))
                     ||
                     ((cp >= (Prims.parse_int "0x1FA70")) &&
                        (cp <= (Prims.parse_int "0x1FAFF"))))
                    ||
                    ((cp >= (Prims.of_int (0x2700))) &&
                       (cp <= (Prims.of_int (0x27BF)))))
                   ||
                   ((cp >= (Prims.of_int (0x2600))) &&
                      (cp <= (Prims.of_int (0x26FF)))))
                  || (cp = (Prims.of_int (0x2764))))
                 || (cp = (Prims.of_int (0x2763))))
                || (cp = (Prims.of_int (0x2665))))
               || (cp = (Prims.of_int (0x2666))))
              || (cp = (Prims.of_int (0x2660))))
             || (cp = (Prims.of_int (0x2663))))
            || (cp = (Prims.of_int (0x2615))))
           || (cp = (Prims.of_int (0x231A))))
          || (cp = (Prims.of_int (0x231B))))
         || (cp = (Prims.of_int (0x23E9))))
        || (cp = (Prims.of_int (0x23EA))))
       || (cp = (Prims.of_int (0x23F0))))
      || (cp = (Prims.of_int (0x23F3))))
     || (cp = (Prims.of_int (0x2328))))
    || (cp = (Prims.of_int (0x260E)))
let is_spacing_mark (cp : Prims.nat) : Prims.bool=
  ((((((cp >= (Prims.of_int (0x0E40))) && (cp <= (Prims.of_int (0x0E44)))) ||
        ((cp >= (Prims.of_int (0x0EC0))) && (cp <= (Prims.of_int (0x0EC4)))))
       ||
       ((cp >= (Prims.of_int (0x0F3E))) && (cp <= (Prims.of_int (0x0F3F)))))
      || ((cp >= (Prims.of_int (0x1031))) && (cp <= (Prims.of_int (0x1031)))))
     || ((cp >= (Prims.of_int (0x09BE))) && (cp <= (Prims.of_int (0x09C4)))))
    || ((cp >= (Prims.of_int (0x093E))) && (cp <= (Prims.of_int (0x0940))))
let gbp_from_codepoint (cp : Prims.nat) : grapheme_break_property=
  if cp = (Prims.of_int (0x0D))
  then GBP_CR
  else
    if cp = (Prims.of_int (0x0A))
    then GBP_LF
    else
      if cp < (Prims.of_int (0x20))
      then GBP_Control
      else
        if (cp >= (Prims.of_int (0x7F))) && (cp <= (Prims.of_int (0x9F)))
        then GBP_Control
        else
          if (cp = (Prims.of_int (0x2028))) || (cp = (Prims.of_int (0x2029)))
          then GBP_Control
          else
            if cp = (Prims.of_int (0x200D))
            then GBP_ZWJ
            else
              if
                (cp >= (Prims.parse_int "0x1F1E6")) &&
                  (cp <= (Prims.parse_int "0x1F1FF"))
              then GBP_Regional_Indicator
              else
                if
                  (cp >= (Prims.of_int (0x1100))) &&
                    (cp <= (Prims.of_int (0x115F)))
                then GBP_L
                else
                  if
                    (cp >= (Prims.of_int (0xA960))) &&
                      (cp <= (Prims.of_int (0xA97C)))
                  then GBP_L
                  else
                    if
                      (cp >= (Prims.of_int (0x1160))) &&
                        (cp <= (Prims.of_int (0x11A7)))
                    then GBP_V
                    else
                      if
                        (cp >= (Prims.of_int (0xD7B0))) &&
                          (cp <= (Prims.of_int (0xD7C6)))
                      then GBP_V
                      else
                        if
                          (cp >= (Prims.of_int (0x11A8))) &&
                            (cp <= (Prims.of_int (0x11FF)))
                        then GBP_T
                        else
                          if
                            (cp >= (Prims.of_int (0xD7CB))) &&
                              (cp <= (Prims.of_int (0xD7FB)))
                          then GBP_T
                          else
                            if
                              (cp >= (Prims.of_int (0xAC00))) &&
                                (cp <= (Prims.of_int (0xD7A3)))
                            then
                              (let idx = cp - (Prims.of_int (0xAC00)) in
                               if
                                 ((mod) idx (Prims.of_int (28))) =
                                   Prims.int_zero
                               then GBP_LV
                               else GBP_LVT)
                            else
                              if is_extended_pictographic cp
                              then GBP_Extended_Pictographic
                              else
                                if is_spacing_mark cp
                                then GBP_SpacingMark
                                else
                                  if is_extend cp
                                  then GBP_Extend
                                  else
                                    if
                                      (((cp >= (Prims.of_int (0x0600))) &&
                                          (cp <= (Prims.of_int (0x0605))))
                                         || (cp = (Prims.of_int (0x06DD))))
                                        || (cp = (Prims.of_int (0x070F)))
                                    then GBP_Prepend
                                    else
                                      if
                                        ((cp = (Prims.of_int (0x0890))) ||
                                           (cp = (Prims.of_int (0x0891))))
                                          || (cp = (Prims.of_int (0x08E2)))
                                      then GBP_Prepend
                                      else
                                        if
                                          (cp = (Prims.parse_int "0x110BD"))
                                            ||
                                            (cp = (Prims.parse_int "0x110CD"))
                                        then GBP_Prepend
                                        else GBP_Other
let should_break (left : grapheme_break_property)
  (right : grapheme_break_property) (state : break_state) :
  (Prims.bool * break_state)=
  if (left = GBP_CR) && (right = GBP_LF)
  then
    (false,
      {
        prev_gbp = right;
        in_emoji_seq = (state.in_emoji_seq);
        ri_count = (state.ri_count)
      })
  else
    if ((left = GBP_Control) || (left = GBP_CR)) || (left = GBP_LF)
    then
      (true,
        { prev_gbp = right; in_emoji_seq = false; ri_count = Prims.int_zero })
    else
      if ((right = GBP_Control) || (right = GBP_CR)) || (right = GBP_LF)
      then
        (true,
          { prev_gbp = right; in_emoji_seq = false; ri_count = Prims.int_zero
          })
      else
        if
          (left = GBP_L) &&
            ((((right = GBP_L) || (right = GBP_V)) || (right = GBP_LV)) ||
               (right = GBP_LVT))
        then
          (false,
            {
              prev_gbp = right;
              in_emoji_seq = (state.in_emoji_seq);
              ri_count = (state.ri_count)
            })
        else
          if
            ((left = GBP_LV) || (left = GBP_V)) &&
              ((right = GBP_V) || (right = GBP_T))
          then
            (false,
              {
                prev_gbp = right;
                in_emoji_seq = (state.in_emoji_seq);
                ri_count = (state.ri_count)
              })
          else
            if ((left = GBP_LVT) || (left = GBP_T)) && (right = GBP_T)
            then
              (false,
                {
                  prev_gbp = right;
                  in_emoji_seq = (state.in_emoji_seq);
                  ri_count = (state.ri_count)
                })
            else
              if (right = GBP_Extend) || (right = GBP_ZWJ)
              then
                (false,
                  {
                    prev_gbp = right;
                    in_emoji_seq =
                      (state.in_emoji_seq ||
                         (left = GBP_Extended_Pictographic));
                    ri_count = (state.ri_count)
                  })
              else
                if right = GBP_SpacingMark
                then
                  (false,
                    {
                      prev_gbp = right;
                      in_emoji_seq = (state.in_emoji_seq);
                      ri_count = (state.ri_count)
                    })
                else
                  if left = GBP_Prepend
                  then
                    (false,
                      {
                        prev_gbp = right;
                        in_emoji_seq = (state.in_emoji_seq);
                        ri_count = (state.ri_count)
                      })
                  else
                    if
                      (state.in_emoji_seq && (left = GBP_ZWJ)) &&
                        (right = GBP_Extended_Pictographic)
                    then
                      (false,
                        {
                          prev_gbp = right;
                          in_emoji_seq = true;
                          ri_count = (state.ri_count)
                        })
                    else
                      if
                        (left = GBP_Regional_Indicator) &&
                          (right = GBP_Regional_Indicator)
                      then
                        (let new_count = state.ri_count + Prims.int_one in
                         if
                           ((mod) new_count (Prims.of_int (2))) =
                             Prims.int_zero
                         then
                           (true,
                             {
                               prev_gbp = right;
                               in_emoji_seq = (state.in_emoji_seq);
                               ri_count = Prims.int_zero
                             })
                         else
                           (false,
                             {
                               prev_gbp = right;
                               in_emoji_seq = (state.in_emoji_seq);
                               ri_count = new_count
                             }))
                      else
                        (true,
                          {
                            prev_gbp = right;
                            in_emoji_seq =
                              (right = GBP_Extended_Pictographic);
                            ri_count = Prims.int_zero
                          })
