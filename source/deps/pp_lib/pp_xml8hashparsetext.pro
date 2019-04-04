function pp_xml8hashparsetext,ih,fold_case=f
compile_opt idl2,logical_predicate
if isa(ih,'list') then begin
  ret=list()
  h=ih
endif else begin
  ret=orderedhash(fold_case=f)
  h=list()
  h.add,ih
endelse
foreach hh,h do begin
  tmp=orderedhash(fold_case=f)
  foreach v,hh,k do begin
    tmp[k]=isa(v,'hash') ? (v.haskey("_text") ? v["_text"] : pp_xml8hashparsetext(v)) : v
  endforeach
  if isa(ih,'list') then ret.add,tmp else ret=tmp
endforeach
return,ret
end
