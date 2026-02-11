#light "off"
module Space_Types

type cell =
FStar_UInt64.t


type universe_id =
Prims.nat


type universe_name =
Prims.string

type discipline =
| Unrestricted
| Affine
| Linear


let uu___is_Unrestricted : discipline  ->  Prims.bool = (fun ( projectee  :  discipline ) -> (match (projectee) with
| Unrestricted -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_Affine : discipline  ->  Prims.bool = (fun ( projectee  :  discipline ) -> (match (projectee) with
| Affine -> begin
true
end
| uu___ -> begin
false
end))


let uu___is_Linear : discipline  ->  Prims.bool = (fun ( projectee  :  discipline ) -> (match (projectee) with
| Linear -> begin
true
end
| uu___ -> begin
false
end))




