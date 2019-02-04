function pp_eosparse,hdfr,dict=dict
compile_opt idl2,logical_predicate
ret=keyword_set(dict) ? dictionary() : orderedhash()
val=eos_query(hdfr,info)
if ~val then return,ret
if ~info.num_swaths then return,ret
swn=strsplit(info.swath_names,',',/extract)
fid=eos_sw_open(hdfr)
foreach s,swn do begin
  sid=eos_sw_attach(fid,s)
  sval=eos_sw_query(hdfr,s,sinfo)
  if sinfo.num_attributes then begin
    attrs=strsplit(sinfo.attributes,',',/extract)
    ret[s]=orderedhash() & ret[s,"attributes"]=orderedhash() ;workaround for IDL 8.4
    foreach attr,attrs do begin
      status=eos_sw_readattr(sid,attr,datbuf)
      if (status eq 0) then ret[s,"attributes",attr]=datbuf
    endforeach
  endif
  !null=eos_sw_detach(sid)
endforeach
!null=eos_sw_close(fid)
return,ret
end
