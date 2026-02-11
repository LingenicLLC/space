open Prims
let transfer (src : Space_Universe.universe) (dst : Space_Universe.universe)
  :
  (Space_Universe.universe * Space_Universe.universe)
    FStar_Pervasives_Native.option=
  if Prims.op_Negation (Space_Universe.is_live src)
  then FStar_Pervasives_Native.None
  else
    if Prims.op_Negation (Space_Universe.is_live dst)
    then FStar_Pervasives_Native.None
    else
      if Prims.op_Negation (Space_Universe.has_space dst)
      then FStar_Pervasives_Native.None
      else
        (match Space_Universe.universe_pop src with
         | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
         | FStar_Pervasives_Native.Some (v, src') ->
             (match Space_Universe.universe_push dst v with
              | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
              | FStar_Pervasives_Native.Some dst' ->
                  FStar_Pervasives_Native.Some (src', dst')))
type transfer_result =
  | TransferOk of (Space_Universe.universe * Space_Universe.universe) 
  | SourceDestroyed of Space_Universe.universe 
  | TransferFailed 
let uu___is_TransferOk (projectee : transfer_result) : Prims.bool=
  match projectee with | TransferOk _0 -> true | uu___ -> false
let __proj__TransferOk__item___0 (projectee : transfer_result) :
  (Space_Universe.universe * Space_Universe.universe)=
  match projectee with | TransferOk _0 -> _0
let uu___is_SourceDestroyed (projectee : transfer_result) : Prims.bool=
  match projectee with | SourceDestroyed _0 -> true | uu___ -> false
let __proj__SourceDestroyed__item___0 (projectee : transfer_result) :
  Space_Universe.universe= match projectee with | SourceDestroyed _0 -> _0
let uu___is_TransferFailed (projectee : transfer_result) : Prims.bool=
  match projectee with | TransferFailed -> true | uu___ -> false
let transfer_checked (src : Space_Universe.universe)
  (dst : Space_Universe.universe) : transfer_result=
  match transfer src dst with
  | FStar_Pervasives_Native.None -> TransferFailed
  | FStar_Pervasives_Native.Some (src', dst') ->
      if Space_Universe.should_self_destruct src'
      then SourceDestroyed dst'
      else TransferOk (src', dst')
