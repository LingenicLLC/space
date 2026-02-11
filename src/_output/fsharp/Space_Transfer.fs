#light "off"
module Space_Transfer

let transfer : Space_Universe.universe  ->  Space_Universe.universe  ->  (Space_Universe.universe * Space_Universe.universe) FStar_Pervasives_Native.option = (fun ( src  :  Space_Universe.universe ) ( dst  :  Space_Universe.universe ) -> (match ((not ((Space_Universe.is_live src)))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___ -> begin
(match ((not ((Space_Universe.is_live dst)))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___1 -> begin
(match ((not ((Space_Universe.has_space dst)))) with
| true -> begin
FStar_Pervasives_Native.None
end
| uu___2 -> begin
(match ((Space_Universe.universe_pop src)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (v, src') -> begin
(match ((Space_Universe.universe_push dst v)) with
| FStar_Pervasives_Native.None -> begin
FStar_Pervasives_Native.None
end
| FStar_Pervasives_Native.Some (dst') -> begin
FStar_Pervasives_Native.Some (((src'), (dst')))
end)
end)
end)
end)
end))

type transfer_result =
| TransferOk of (Space_Universe.universe * Space_Universe.universe)
| SourceDestroyed of Space_Universe.universe
| TransferFailed


let uu___is_TransferOk : transfer_result  ->  Prims.bool = (fun ( projectee  :  transfer_result ) -> (match (projectee) with
| TransferOk (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__TransferOk__item___0 : transfer_result  ->  (Space_Universe.universe * Space_Universe.universe) = (fun ( projectee  :  transfer_result ) -> (match (projectee) with
| TransferOk (_0) -> begin
_0
end))


let uu___is_SourceDestroyed : transfer_result  ->  Prims.bool = (fun ( projectee  :  transfer_result ) -> (match (projectee) with
| SourceDestroyed (_0) -> begin
true
end
| uu___ -> begin
false
end))


let __proj__SourceDestroyed__item___0 : transfer_result  ->  Space_Universe.universe = (fun ( projectee  :  transfer_result ) -> (match (projectee) with
| SourceDestroyed (_0) -> begin
_0
end))


let uu___is_TransferFailed : transfer_result  ->  Prims.bool = (fun ( projectee  :  transfer_result ) -> (match (projectee) with
| TransferFailed -> begin
true
end
| uu___ -> begin
false
end))


let transfer_checked : Space_Universe.universe  ->  Space_Universe.universe  ->  transfer_result = (fun ( src  :  Space_Universe.universe ) ( dst  :  Space_Universe.universe ) -> (match ((transfer src dst)) with
| FStar_Pervasives_Native.None -> begin
TransferFailed
end
| FStar_Pervasives_Native.Some (src', dst') -> begin
(match ((Space_Universe.should_self_destruct src')) with
| true -> begin
SourceDestroyed (dst')
end
| uu___ -> begin
TransferOk (((src'), (dst')))
end)
end))




