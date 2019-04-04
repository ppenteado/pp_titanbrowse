function pp_wget_cached,url,rid=rid,clear=clear,_ref_extra=ex,verbose=verbose,gunzip=gunzip
compile_opt idl2,logical_predicate
rid=n_elements(rid) ? rid : 'pp_wget_cache'
tdir=filepath(getenv('USER')+'_'+rid+path_sep(),/tmp)
if ~file_test(tdir,/directory) then file_mkdir,tdir
tf=idl_validname(file_dirname(url),/convert_all)+'_'+file_basename(url)
t=tdir+tf
if (~file_test(t,/read)) || keyword_set(clear) then begin
  if keyword_set(verbose) then print,'Downloading ',url
  if !version.release ge '8.5' then begin
    t=wget(url,directory=tdir,filename=file_basename(t),_strict_extra=ex)
  endif else begin
    p=pp_wget(url,localdir=tdir)
    p.geturl
    file_move,tdir+path_sep()+file_basename(url),t
  endelse
  if keyword_set(gunzip) then begin
    file_gunzip,t,t+'_gunz',/delete
    file_move,t+'_gunz',t
  endif
endif else if keyword_set(verbose) then print,'Using cached ',url
return,t
end
