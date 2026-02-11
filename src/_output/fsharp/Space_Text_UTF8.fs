#light "off"
module Space_Text_UTF8

let is_ascii : FStar_UInt8.t  ->  Prims.bool = (fun ( b  :  FStar_UInt8.t ) -> ((FStar_UInt8.v b) <= (Prims.parse_int "0x7F")))


let is_continuation : FStar_UInt8.t  ->  Prims.bool = (fun ( b  :  FStar_UInt8.t ) -> (

let v = (FStar_UInt8.v b)
in ((v >= (Prims.parse_int "0x80")) && (v <= (Prims.parse_int "0xBF")))))


let is_lead_2 : FStar_UInt8.t  ->  Prims.bool = (fun ( b  :  FStar_UInt8.t ) -> (

let v = (FStar_UInt8.v b)
in ((v >= (Prims.parse_int "0xC2")) && (v <= (Prims.parse_int "0xDF")))))


let is_lead_3 : FStar_UInt8.t  ->  Prims.bool = (fun ( b  :  FStar_UInt8.t ) -> (

let v = (FStar_UInt8.v b)
in ((v >= (Prims.parse_int "0xE0")) && (v <= (Prims.parse_int "0xEF")))))


let is_lead_4 : FStar_UInt8.t  ->  Prims.bool = (fun ( b  :  FStar_UInt8.t ) -> (

let v = (FStar_UInt8.v b)
in ((v >= (Prims.parse_int "0xF0")) && (v <= (Prims.parse_int "0xF4")))))


let sequence_length : FStar_UInt8.t  ->  Prims.nat = (fun ( lead  :  FStar_UInt8.t ) -> (match ((is_ascii lead)) with
| true -> begin
(Prims.parse_int "1")
end
| uu___ -> begin
(match ((is_lead_2 lead)) with
| true -> begin
(Prims.parse_int "2")
end
| uu___1 -> begin
(match ((is_lead_3 lead)) with
| true -> begin
(Prims.parse_int "3")
end
| uu___2 -> begin
(match ((is_lead_4 lead)) with
| true -> begin
(Prims.parse_int "4")
end
| uu___3 -> begin
(Prims.parse_int "0")
end)
end)
end)
end))


let decode_codepoint_1 : FStar_UInt8.t  ->  Prims.nat = (fun ( b0  :  FStar_UInt8.t ) -> (FStar_UInt8.v b0))


let decode_codepoint_2 : FStar_UInt8.t  ->  FStar_UInt8.t  ->  Prims.nat = (fun ( b0  :  FStar_UInt8.t ) ( b1  :  FStar_UInt8.t ) -> (

let v0 = (FStar_UInt8.v b0)
in (

let v1 = (FStar_UInt8.v b1)
in (match (((v0 >= (Prims.parse_int "0xC0")) && (v1 >= (Prims.parse_int "0x80")))) with
| true -> begin
(((v0 - (Prims.parse_int "0xC0")) * (Prims.parse_int "64")) + (v1 - (Prims.parse_int "0x80")))
end
| uu___ -> begin
(Prims.parse_int "0")
end))))


let decode_codepoint_3 : FStar_UInt8.t  ->  FStar_UInt8.t  ->  FStar_UInt8.t  ->  Prims.nat = (fun ( b0  :  FStar_UInt8.t ) ( b1  :  FStar_UInt8.t ) ( b2  :  FStar_UInt8.t ) -> (

let v0 = (FStar_UInt8.v b0)
in (

let v1 = (FStar_UInt8.v b1)
in (

let v2 = (FStar_UInt8.v b2)
in (match ((((v0 >= (Prims.parse_int "0xE0")) && (v1 >= (Prims.parse_int "0x80"))) && (v2 >= (Prims.parse_int "0x80")))) with
| true -> begin
((((v0 - (Prims.parse_int "0xE0")) * (Prims.parse_int "4096")) + ((v1 - (Prims.parse_int "0x80")) * (Prims.parse_int "64"))) + (v2 - (Prims.parse_int "0x80")))
end
| uu___ -> begin
(Prims.parse_int "0")
end)))))


let decode_codepoint_4 : FStar_UInt8.t  ->  FStar_UInt8.t  ->  FStar_UInt8.t  ->  FStar_UInt8.t  ->  Prims.nat = (fun ( b0  :  FStar_UInt8.t ) ( b1  :  FStar_UInt8.t ) ( b2  :  FStar_UInt8.t ) ( b3  :  FStar_UInt8.t ) -> (

let v0 = (FStar_UInt8.v b0)
in (

let v1 = (FStar_UInt8.v b1)
in (

let v2 = (FStar_UInt8.v b2)
in (

let v3 = (FStar_UInt8.v b3)
in (match (((((v0 >= (Prims.parse_int "0xF0")) && (v1 >= (Prims.parse_int "0x80"))) && (v2 >= (Prims.parse_int "0x80"))) && (v3 >= (Prims.parse_int "0x80")))) with
| true -> begin
(((((v0 - (Prims.parse_int "0xF0")) * (Prims.parse_int "262144")) + ((v1 - (Prims.parse_int "0x80")) * (Prims.parse_int "4096"))) + ((v2 - (Prims.parse_int "0x80")) * (Prims.parse_int "64"))) + (v3 - (Prims.parse_int "0x80")))
end
| uu___ -> begin
(Prims.parse_int "0")
end))))))


let is_valid_codepoint : Prims.nat  ->  Prims.bool = (fun ( cp  :  Prims.nat ) -> ((cp <= (Prims.parse_int "0x10FFFF")) && (not (((cp >= (Prims.parse_int "0xD800")) && (cp <= (Prims.parse_int "0xDFFF")))))))


let rec is_valid_utf8 : FStar_UInt8.t Prims.list  ->  Prims.bool = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (match (bytes) with
| [] -> begin
true
end
| (b0)::rest -> begin
(

let len = (sequence_length b0)
in (match ((Prims.op_Equality len (Prims.parse_int "0"))) with
| true -> begin
false
end
| uu___ -> begin
(match ((Prims.op_Equality len (Prims.parse_int "1"))) with
| true -> begin
(is_valid_utf8 rest)
end
| uu___1 -> begin
(match ((Prims.op_Equality len (Prims.parse_int "2"))) with
| true -> begin
(match (rest) with
| (b1)::rest' -> begin
((is_continuation b1) && (is_valid_utf8 rest'))
end
| uu___2 -> begin
false
end)
end
| uu___2 -> begin
(match ((Prims.op_Equality len (Prims.parse_int "3"))) with
| true -> begin
(match (rest) with
| (b1)::(b2)::rest' -> begin
(((is_continuation b1) && (is_continuation b2)) && (is_valid_utf8 rest'))
end
| uu___3 -> begin
false
end)
end
| uu___3 -> begin
(match (rest) with
| (b1)::(b2)::(b3)::rest' -> begin
((((is_continuation b1) && (is_continuation b2)) && (is_continuation b3)) && (is_valid_utf8 rest'))
end
| uu___4 -> begin
false
end)
end)
end)
end)
end))
end))


let rec is_all_ascii : FStar_UInt8.t Prims.list  ->  Prims.bool = (fun ( bytes  :  FStar_UInt8.t Prims.list ) -> (match (bytes) with
| [] -> begin
true
end
| (b)::rest -> begin
((is_ascii b) && (is_all_ascii rest))
end))




