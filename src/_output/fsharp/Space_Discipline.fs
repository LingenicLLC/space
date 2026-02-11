#light "off"
module Space_Discipline

let can_copy : Space_Types.discipline  ->  Prims.bool = (fun ( d  :  Space_Types.discipline ) -> (match (d) with
| Space_Types.Unrestricted -> begin
true
end
| Space_Types.Affine -> begin
false
end
| Space_Types.Linear -> begin
false
end))


let can_drop : Space_Types.discipline  ->  Prims.bool = (fun ( d  :  Space_Types.discipline ) -> (match (d) with
| Space_Types.Unrestricted -> begin
true
end
| Space_Types.Affine -> begin
true
end
| Space_Types.Linear -> begin
false
end))


let requires_release : Space_Types.discipline  ->  Prims.bool = (fun ( d  :  Space_Types.discipline ) -> (match (d) with
| Space_Types.Unrestricted -> begin
false
end
| Space_Types.Affine -> begin
true
end
| Space_Types.Linear -> begin
false
end))


let self_destructs : Space_Types.discipline  ->  Prims.bool = (fun ( d  :  Space_Types.discipline ) -> (match (d) with
| Space_Types.Unrestricted -> begin
false
end
| Space_Types.Affine -> begin
false
end
| Space_Types.Linear -> begin
true
end))


let more_restrictive : Space_Types.discipline  ->  Space_Types.discipline  ->  Prims.bool = (fun ( d1  :  Space_Types.discipline ) ( d2  :  Space_Types.discipline ) -> (match (((d1), (d2))) with
| (Space_Types.Linear, uu___) -> begin
true
end
| (Space_Types.Affine, Space_Types.Unrestricted) -> begin
true
end
| (uu___, uu___1) -> begin
false
end))




