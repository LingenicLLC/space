#light "off"
module Space_Text_UCD

type mapping_entry =
Space_Text_UCD_Types.mapping_entry


type ccc_entry =
Space_Text_UCD_Types.ccc_entry


type decomp_entry =
Space_Text_UCD_Types.decomp_entry


type comp_entry =
Space_Text_UCD_Types.comp_entry


let simple_uppercase : Prims.nat  ->  Prims.nat = (fun ( cp  :  Prims.nat ) -> (Space_Text_UCD_Case.get_uppercase cp))


let simple_lowercase : Prims.nat  ->  Prims.nat = (fun ( cp  :  Prims.nat ) -> (Space_Text_UCD_Case.get_lowercase cp))


let combining_class : Prims.nat  ->  Prims.nat = (fun ( cp  :  Prims.nat ) -> (Space_Text_UCD_CCC.get_ccc cp))


let is_ccc_starter : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> (Space_Text_UCD_CCC.is_starter cp))


let canonical_decomposition : Prims.nat  ->  Prims.nat Prims.list FStar_Pervasives_Native.option = (fun ( cp  :  Prims.nat ) -> (Space_Text_UCD_Decomp.get_decomposition cp))


let compose : Prims.nat  ->  Prims.nat  ->  Prims.nat FStar_Pervasives_Native.option = (fun ( first  :  Prims.nat ) ( second  :  Prims.nat ) -> (Space_Text_UCD_Comp.try_compose first second))


let codepoint_has_case : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> (Space_Text_UCD_Case.has_case cp))




