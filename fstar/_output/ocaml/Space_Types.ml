open Prims
type cell = FStar_UInt64.t
type universe_id = Prims.nat
type universe_name = Prims.string
type discipline =
  | Unrestricted 
  | Affine 
  | Linear 
let uu___is_Unrestricted (projectee : discipline) : Prims.bool=
  match projectee with | Unrestricted -> true | uu___ -> false
let uu___is_Affine (projectee : discipline) : Prims.bool=
  match projectee with | Affine -> true | uu___ -> false
let uu___is_Linear (projectee : discipline) : Prims.bool=
  match projectee with | Linear -> true | uu___ -> false
