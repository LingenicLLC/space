open Prims
type mapping_entry = Space_Text_UCD_Types.mapping_entry
type ccc_entry = Space_Text_UCD_Types.ccc_entry
type decomp_entry = Space_Text_UCD_Types.decomp_entry
type comp_entry = Space_Text_UCD_Types.comp_entry
let simple_uppercase (cp : Prims.nat) : Prims.nat=
  Space_Text_UCD_Case.get_uppercase cp
let simple_lowercase (cp : Prims.nat) : Prims.nat=
  Space_Text_UCD_Case.get_lowercase cp
let combining_class (cp : Prims.nat) : Prims.nat=
  Space_Text_UCD_CCC.get_ccc cp
let is_ccc_starter (cp : Prims.nat) : Prims.bool=
  Space_Text_UCD_CCC.is_starter cp
let canonical_decomposition (cp : Prims.nat) :
  Prims.nat Prims.list FStar_Pervasives_Native.option=
  Space_Text_UCD_Decomp.get_decomposition cp
let compose (first : Prims.nat) (second : Prims.nat) :
  Prims.nat FStar_Pervasives_Native.option=
  Space_Text_UCD_Comp.try_compose first second
let codepoint_has_case (cp : Prims.nat) : Prims.bool=
  Space_Text_UCD_Case.has_case cp
