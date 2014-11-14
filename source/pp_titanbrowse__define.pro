; docformat = 'rst'
;+
; :Uses: pp_editablecube__define, pp_getcubeheadervalue, pp_extractfields, pp_setcubeheadervalue, pp_readcube__define,
;   pp_cubecollection__define, pp_titanbrowse_metadb__define, pp_titanbrowse_db__define
; 
; :Author: Paulo Penteado (pp.penteado@gmail.com), Nov/2009
; 
; :Version: 20141114
; 
;-

function pp_titanbrowse::init,mdbfiles,vis=vis
compile_opt idl2
ret=0
self.version='20141114'
;Defaults
vis=n_elements(vis) eq 1 ? vis : 0
channel=vis ? 'vis' : 'ir'
nfiles=n_elements(mdbfiles)
mdbfiles=nfiles gt 0 ? mdbfiles : file_search('covims_*_'+channel+'.sav',count=nfiles)
if (nfiles eq 0) then begin
  print,'pp_titanbrowse: No metadata files found or provided'
  return,0
endif 

;Initialize the database objects
podb=objarr(nfiles)
for i=0,nfiles-1 do begin
  print,'Reading from file "',mdbfiles[i],'" ('+strcompress(string(i+1,'/',nfiles),/rem)+')'
  podb[i]=obj_new('pp_titanbrowse_db',mdbfiles[i])
  if (~obj_valid(podb[i])) then begin ;Get out if one of the objects failed to initialize
    print,'pp_titanbrowse: Could not initialize database from "',mdbfiles[i],'"'
    ret=0
  endif else begin ;Test that all objects have the same type of data (same self.std)
    podb[i]->getproperty,std=std
    if (i eq 0) then begin
      ostd=std
      ret=1
    endif else begin
      tmp=(std.bands eq ostd.bands)&&(std.nback eq ostd.nback)&&(std.unit eq ostd.unit)
      tmp=tmp&&(std.type eq ostd.type)
      tmp=tmp&&(finite(*std.fill) ? *std.fill eq *ostd.fill : finite(*std.fill,/sign) eq finite(*ostd.fill,/sign))
      tmp=tmp&&array_equal(*std.bnames,*ostd.bnames)
      tmp=tmp&&array_equal(*std.tnames,*ostd.tnames)&&array_equal(*std.bunits,*ostd.bunits)
      if (~array_equal(*std.wavs,*ostd.wavs)) then begin ;Ignore differences due to the fix of the 5.108/5.1225 wavelength
        w=where(*std.wavs ne *ostd.wavs,nw)
        for j=0,nw-1 do begin
          wvs=[(*std.wavs)[w[j]],(*ostd.wavs)[w[j]]]
          tmp=tmp&&(strupcase(std.unit) eq 'MICROMETER')&&(max(wvs) eq 5.1225d0)&&(min(wvs) eq 5.108d0)
          if (tmp) then print,'pp_titanbrowse: Ignoring difference between 5.108 and 5.1225 '+string(181B)+'m'
        endfor
      endif
      if (~tmp) then begin  
        print,'pp_titanbrowse: Database in "',mdbfiles[i],'" is incompatible with that of the previous files'
        ret=0 
      endif else ret=1
    endelse
  endelse  
  if (ret eq 0) then begin ;Get out if something did not work
    obj_destroy,podb
    return,ret  
  endif
endfor
;Allocate the pointer arrays to the core and backplane vectors
self.dbvecs.core=ptr_new(ptrarr(std.bands,nfiles),/no_copy)
self.dbvecs.back=ptr_new(ptrarr(std.nback,nfiles),/no_copy)
;Trackers of current selection
self.nselcubes=ptr_new(lonarr(nfiles),/no_copy)
self.nselpixels=ptr_new(lonarr(nfiles),/no_copy)
self.selcubes=ptr_new(ptrarr(nfiles),/no_copy)
self.selpixels=ptr_new(ptrarr(nfiles),/no_copy)
self.selpixels_c=ptr_new(ptrarr(nfiles),/no_copy)
self.selpixels_xz=ptr_new(ptrarr(nfiles),/no_copy)
;Save things to self
self.podb=ptr_new(podb,/no_copy)
self.nfiles=nfiles
self.mdbfiles=ptr_new(mdbfiles,/no_copy)
self.std=ptr_new(ostd,/no_copy)
self.backindex=hash(*std.bnames,indgen(n_elements(*std.bnames)))

;Define type for cube information
cmd=*((*self.podb)[0]->getcmd())
tnames=tag_names(cmd.back_max) & nback=n_elements(tnames)
btmp=create_struct(tnames[0],0d0)
for i=1,nback-2 do btmp=create_struct(btmp,tnames[i],0d0)
btmp=create_struct(name='pp_titanbrowse_cmd_back',btmp,tnames[nback-1],0d0)
cmd={pp_titanbrowse_cmd,rev:'',seq:'',seq_title:'',$
 prod_id:'',start:'',stop:'',nat_start:'',$
 lines:0L,samples:0L,pixels:0L,surf_pixels:0L,$
 exposure:0d0,ir_mode:'',vis_mode:'',file:'',$
 back_max:btmp,back_min:btmp}
cubeinfo={pp_titanbrowse_cubeinfo,inherits pp_titanbrowse_cmd,dbfile:'',cubefile:'',dbind:0L,cubeind:0L}
;Define type for pixel data
pixdata={pp_titanbrowse_pixdata,core:dblarr((*self.std).bands),backplanes:btmp,cube:'',x:0L,z:0L}

return,ret
end

pro pp_titanbrowse::indexcubes
compile_opt idl2,logical_predicate
  self.cubehash=hash()
  for i=0,self.nfiles-1 do begin
    files=(*self.podb)[i]->filenames(ncubes=ncubes)
    (*self.podb)[i]->getproperty,pstart=pstart
    ps=*pstart
    foreach file,files,ifile do begin
      tmp={pp_titanbrowse_cubemd}
      tmp.dbindex=i
      tmp.pstart=ps[ifile]
      self.cubehash[file]=tmp
    endforeach
  endfor
end

pro pp_titanbrowse::selectcubes,iexpr,all=all,none=none,count=count,pixelsselected=pixsel,bytable=table
;Changes the current cube selection, to all cubes or no cubes, or filter the selection with the given expression.
;iexpr must already be in the internal format (expressions built with aliases must be parsed to make them valid here).
compile_opt idl2

bytable=n_elements(table)
if bytable gt 0 then begin
  l=pp_locate(table.cubefile)
  cube_names=(l.keys()).toarray()
  ci=self.getcubeinfo(cube_name=cube_names)
  dl=pp_locate(ci.dbind)
  self.selectcubes,/none
  pstart=hash()
  foreach dbi,dl,idbi do begin
    (*self.nselcubes)[idbi]=n_elements(dbi)
    (*self.selcubes)[idbi]=ptr_new(ci[dbi].cubeind)
    s=sort(ci[dbi].cubeind)
    citmp=(ci[dbi])[s]
    tmp=total(citmp.pixels,/cumulative,/integer)-citmp[0].pixels
    tmp2=hash()
    foreach cube,citmp,icube do tmp2[cube.cubeind]=tmp[icube]
    pstart[idbi]=tmp2 
  endforeach
  self.selectpixels,/none
  dbselpixs=hash(dl.keys())
  foreach cube,cube_names,icube do begin
    pixels=table[l[cube]]
    pixinds=pixels.x+ci[icube].samples*pixels.z
    ;(*self.podb)[ci[icube].dbind]->getproperty,pstart=pstart
    ;pixstosel=(*pstart)[ci[icube].cubeind]+pixinds
    pixstosel=(pstart[ci[icube].dbind])[ci[icube].cubeind]+pixinds
    dbselpixs[ci[icube].dbind]=[dbselpixs[ci[icube].dbind],pixstosel]
  endforeach
  whereres=hash()
  foreach dbselpix,dbselpixs,idbsel do begin
    pixtosel=dbselpix[sort(dbselpix)]
    whereres[idbsel]=pixtosel
  endforeach
  self.selectpixels,/all
  self.selectpixels,whereres=whereres
endif

if keyword_set(none) then begin ;If selection is to be cleared
  (*self.nselcubes)[*]=0L
  ptr_free,(*self.selcubes)
endif else if keyword_set(all) then begin ;If selecting everything
  for i=0,self.nfiles-1 do begin
    void=(*self.podb)[i]->filenames(ncubes=ncubes)
    (*self.nselcubes)[i]=ncubes
    ptr_free,(*self.selcubes)[i]
    (*self.selcubes)[i]=ptr_new(lindgen(ncubes))
  endfor
endif else if keyword_set(pixsel) then begin ;If selecting only the cubes with selected pixels
  for i=0,self.nfiles-1 do if ((*self.nselcubes)[i] gt 0) then begin
    ptr_free,(*self.selcubes)[i]
    (*self.nselcubes)[i]=0
    if (ptr_valid((*self.selpixels_c)[i])) then begin
      pixcube=*((*self.selpixels_c)[i])
      s=sort(pixcube) & pixcube=pixcube[s] & pixcube=pixcube[uniq(pixcube)]
      (*self.nselcubes)[i]=n_elements(pixcube)
      (*self.selcubes)[i]=ptr_new(pixcube,/no_copy)
    endif else (*self.nselcubes)[i]=0L
  endif
endif else if (n_elements(iexpr) eq 1) then begin ;If filtering from given expression
  expr=strtrim(iexpr,2)
  for i=0,self.nfiles-1 do begin
    catch,error_status
    if (error_status ne 0) then begin
      catch,/cancel
      print,'pp_titanbrowse::selectcubes: Expression evaluation error'
      break
    endif else if ((*self.nselcubes)[i] gt 0) then begin ;Skip filtering if there are already none selected
;Make sel and cmd to be used in expr
      sel=*((*self.selcubes)[i])
      pcmd=(*self.podb)[i]->getcmd()
;      cmd=temporary(*pcmd) ;Borrow instead of copy it, for efficiency
      cmd=(*pcmd)
      tmp=execute('nsel='+expr)
;Put cmd's contents back
;      *pcmd=temporary(cmd)
;Make the new selection
      if tmp then w=where(nsel[sel],count,/l64) else begin
        print,'pp_titanbrowse::selectcubes: Expression evaluation error'
        break
      endelse
      (*self.nselcubes)[i]=count
      ptr_free,(*self.selcubes)[i]
      if (count gt 0) then (*self.selcubes)[i]=ptr_new(sel[w])
    endif
  endfor
endif
count=total(*self.nselcubes,/integer)
;Clear pixel selections
if ~bytable then self->selectpixels,/none
end

pro pp_titanbrowse::selectpixels,iexpr,all=all,none=none,count=count,eval=eval,whereres=whereres
;Changes the current pixel selection, to all pixels or no pixels, or filter the selection with the given expression.
;iexpr must already be in the internal format (expressions built with aliases must be parsed to make them valid here).
compile_opt idl2
eval=keyword_set(eval)
if keyword_set(none) then begin ;If selection is to be cleared
  (*self.nselpixels)[*]=0L
  ptr_free,(*self.selpixels),(*self.selpixels_c),(*self.selpixels_xz)
endif else if keyword_set(all) then begin ;If selecting everything
  for i=0,self.nfiles-1 do if ((*self.nselcubes)[i] gt 0) then begin ;Skip files with no cubes selected
    sel=*((*self.selcubes)[i]) ;Indexes of selected cubes
;Get the needed metadata
    pcmd=(*self.podb)[i]->getcmd()
    pixels=(*pcmd).pixels
    npix=total(pixels[sel],/integer)
    selpixels=lonarr(npix)
    pixcube=lonarr(npix)
    pixxz=lonarr(2,npix)
;    tmp=long(total((*pcmd).pixels[sel],/cumul))-(*pcmd).pixels[sel] ;List start index for each cube's pixels
    ;tmp=(total(pixels,/cumul,/integer)-pixels)[sel] ;List start index for each cube's pixels
;Get the start location of each cube in the database vectors
     (*self.podb)[i]->getproperty,pstart=pstart
;Compute the proper indexes from each selected cube (counted relative to all pixels in the file, not just those that are selected)
    k=0
    for j=0,(*self.nselcubes)[i]-1 do begin
      dims=[(*pcmd).samples[sel[j]],(*pcmd).lines[sel[j]]]
      ;pixcube[tmp[j]]=replicate(sel[j],(*pcmd).pixels[sel[j]])
      pixcube[k]=replicate(sel[j],pixels[sel[j]])
      ;tmp2=lindgen((*pcmd).pixels[sel[j]])
      tmp2=lindgen(pixels[sel[j]])
      ;selpixels[tmp[j]]=tmp2+(*pstart)[sel[j]]
      selpixels[k]=tmp2+(*pstart)[sel[j]]
      ;pixxz[tmp[j]*2]=reform(array_indices(dims,tmp2,/dim),2*(*pcmcube_namesd).pixels[sel[j]])
      pixxz[k*2]=reform(array_indices(dims,tmp2,/dim),2*pixels[sel[j]])
      k+=pixels[sel[j]]
    endfor
;Save things into self
    ptr_free,(*self.selpixels)[i]
    (*self.nselpixels)[i]=npix
    (*self.selpixels)[i]=ptr_new(selpixels,/no_copy)
    ptr_free,(*self.selpixels_c)[i]
    (*self.selpixels_c)[i]=ptr_new(pixcube,/no_copy)
    ptr_free,(*self.selpixels_xz)[i]
    (*self.selpixels_xz)[i]=ptr_new(pixxz,/no_copy)
  endif
endif else if ((n_elements(iexpr) eq 1) or n_elements(whereres) gt 0) then begin ;If filtering from given expression
  if (n_elements(iexpr) eq 1) then expr=strtrim(iexpr,2)
  for i=0,self.nfiles-1 do if ((*self.nselpixels)[i] gt 0) then begin ;Skip files with no pixels selected
;Make sel and to be used in expr
    sel=*((*self.selpixels)[i]) ;Indexes of selected pixels
    if (n_elements(whereres) gt 0) then begin
      w=whereres[i]
      count=long(n_elements(w))
    endif else begin
      tmp=execute('nsel='+expr)
;Make the new selection
      if tmp then w=where(nsel,count,/l64) else begin
        print,'pp_titanbrowse::selectpixels: Expression evaluation error'
        break
      endelse
    endelse
    (*self.nselpixels)[i]=count
    ptr_free,(*self.selpixels)[i]
    if (count gt 0) then begin
      (*self.selpixels)[i]=ptr_new(sel[w])
      *((*self.selpixels_c)[i])=(*((*self.selpixels_c)[i]))[w]
      *((*self.selpixels_xz)[i])=(*((*self.selpixels_xz)[i]))[*,w]
    endif else ptr_free,(*self.selpixels_c)[i],(*self.selpixels_xz)[i]
  endif
endif
count=long(total(*self.nselpixels))
;Set update flag
self.update=1B
end

function pp_titanbrowse::evalexpr,iexpr,store=store,cube=cube
compile_opt idl2,logical_predicate
cube=keyword_set(cube)
expr=strtrim(iexpr,2)
if (~cube) then begin
  pd={pp_titanbrowse_pixdata}
  self.getproperty,nselpixels=nsp
  res=replicate({pp_titanbrowse_eval,val:!values.d_nan,pixdata:pd},nsp)
  icount=0LL
  sp=self.getselectedpixels()
  res.pixdata=sp
  for i=0,self.nfiles-1 do if ((*self.nselpixels)[i] gt 0) then begin ;Skip files with no pixels selected
    ;Make sel and to be used in expr
    sel=*((*self.selpixels)[i]) ;Indexes of selected pixels
    tmp=execute('nsel='+expr)
    if tmp then w=where(nsel,count,/l64) else begin
      print,'pp_titanbrowse::selectpixels: Expression evaluation error'
      break
    endelse  
  ;  (*self.nselpixels)[i]=count
  ;  ptr_free,(*self.selpixels)[i]
    if (count gt 0) then begin
      nc=(*self.nselpixels)[i]
      res[icount:icount+nc-1].val=nsel
  ;    (*self.selpixels)[i]=ptr_new(sel[w])
  ;    *((*self.selpixels_c)[i])=(*((*self.selpixels_c)[i]))[w]
  ;    *((*self.selpixels_xz)[i])=(*((*self.selpixels_xz)[i]))[*,w]
    endif
      icount+=nc
  endif
  if keyword_set(store) then self.evalres=ptr_new(res)
endif else begin
  ret=list()
  for i=0,self.nfiles-1 do begin
    catch,error_status
    if (error_status ne 0) then begin
      catch,/cancel
      print,'pp_titanbrowse::selectcubes: Expression evaluation error'
      break
    endif else if ((*self.nselcubes)[i] gt 0) then begin ;Skip filtering if there are already none selected
      ;Make sel and cmd to be used in expr
      sel=*((*self.selcubes)[i])
      pcmd=(*self.podb)[i]->getcmd()
      ;      cmd=temporary(*pcmd) ;Borrow instead of copy it, for efficiency
      cmd=(*pcmd)
      tmp=execute('nsel='+expr)
      ;Put cmd's contents back
      ;      *pcmd=temporary(cmd)
      ;Make the new selection
      if tmp then w=where(nsel[sel],count,/l64) else begin
        print,'pp_titanbrowse::selectcubes: Expression evaluation error'
        break
      endelse
      if (count gt 0) then begin
        nc=(*self.nselcubes)[i]
        ret.add,nsel[sel],/extract
        ;    (*self.selpixels)[i]=ptr_new(sel[w])
        ;    *((*self.selpixels_c)[i])=(*((*self.selpixels_c)[i]))[w]
        ;    *((*self.selpixels_xz)[i])=(*((*self.selpixels_xz)[i]))[*,w]
      endif      
;      (*self.nselcubes)[i]=count
;      ptr_free,(*self.selcubes)[i]
;      if (count gt 0) then (*self.selcubes)[i]=ptr_new(sel[w])
    endif
  endfor
  res=reform(ret.toarray())
  if keyword_set(store) then self.cubeevalres=ptr_new(res)
endelse
return,res

end

function pp_titanbrowse::parseexpr,iexpr,cubes=cubes,pixels=pixels
;Converts the occurences of pseudovariables in expr into the proper
;internal variable references. If cubes is not set, conversion is for
;pixel variable names, in which case the needed heap variables are loaded.
;Backplane names are like '_b_latitude_', and core band names are like '_c_71_'
compile_opt idl2

;Defaults
cubes=keyword_set(cubes) ? 1B : keyword_set(pixels) ? 0B : 1B
pixels=~cubes
ret=iexpr
if cubes then return,ret ;Parse cube pseudovariables
;Parse pixel pseudovariables
ret=iexpr
;Parse backplane pseudovariables
binds=hash()
while stregex(ret,'_b_([[:alnum:]_]+)_',/boolean) do begin
  smp=stregex(ret,'_b_([[:alnum:]_]+)_',/subexpr,length=sml)
  strbegin=strmid(ret,0,smp[0])
  strend=strmid(ret,smp[0]+sml[0])
  bind=(self.backindex)[strupcase(strmid(ret,smp[1],sml[1]))]
  binds[bind]=!null
  ret=strbegin+'((*((*self.podb)[i]->getbackplane('+strtrim(bind,2)+',/po)))[sel])'+strend
endwhile
binds=(binds.keys()).toarray()
if (binds ne !null) then binds=binds[sort(binds)]
;Parse core pseudovariables
cinds=hash()
while stregex(ret,'_c_([[:digit:]]+)_',/boolean) do begin
  smp=stregex(ret,'_c_([[:digit:]]+)_',/subexpr,length=sml)
  strbegin=strmid(ret,0,smp[0])
  strend=strmid(ret,smp[0]+sml[0])
  cind=fix(strmid(ret,smp[1],sml[1]))
  cinds[cind]=!null
  ret=strbegin+'((*((*self.podb)[i]->getband('+strtrim(cind,2)+',/po)))[sel])'+strend
endwhile
cinds=(cinds.keys()).toarray()
if (cinds ne !null) then cinds=cinds[sort(cinds)]


;;old parser, needed the variables to be separated by whitespace
;;split tokens by whitespace  
;tmp=strsplit(strtrim(iexpr,2),/extract,count=nt)
;;Parse backplane pseudovariables
;wb=where((strpos(tmp,'_b_') eq 0) and (strpos(tmp,'_',/reverse_search) eq (strlen(tmp)-1)),nwb)
;if (nwb gt 0) then begin
;  namesb=strmid(tmp[wb],3)
;  binds=intarr(nwb)
;  (*self.podb)[0]->getproperty,std=std
;  for i=0,nwb-1 do begin
;    namesb[i]=strupcase(strmid(namesb[i],0,strlen(namesb[i])-1))
;    binds[i]=(where(strpos(*std.bnames,namesb[i]) ne -1))[0]
;  endfor
;  btoks='(*((*self.podb)[i]->getbackplane('+strtrim(string(binds),2)+',/po)))[sel]'
;  ;btoks='(*((*self.podb)[i]->getbackplane('+strtrim(string(binds),2)+',/po)))'
;  tmp[wb]=btoks
;  binds=binds[sort(binds)]
;  binds=binds[uniq(binds)]
;endif
;;Parse core pseudovariables
;wc=where((strpos(tmp,'_c_') eq 0) and (strpos(tmp,'_',/reverse_search) eq (strlen(tmp)-1)),nwc)
;if (nwc gt 0) then begin
;  ctoks=strarr(nwc)
;  namesc=strmid(tmp[wc],3)
;  for i=0,nwc-1 do namesc[i]=strmid(namesc[i],0,strlen(namesc[i])-1)
;  cinds=fix(namesc)
;  ctoks='(*((*self.podb)[i]->getband('+namesc+',/po)))[sel]'
;  ;ctoks='(*((*self.podb)[i]->getband('+namesc+',/po)))'
;  tmp[wc]=ctoks
;  cinds=cinds[sort(cinds)]
;  cinds=cinds[uniq(cinds)]
;endif
;;Assemble the output from the pieces
;ret=strjoin(tmp,' ')

;Load the needed backplanes and core bands
for i=0,self.nfiles-1 do if ((*self.nselpixels)[i] gt 0) then begin
  (*self.podb)[i]->opendbsav
  for j=0,n_elements(cinds)-1 do (*self.podb)[i]->loadband,cinds[j]
  for j=0,n_elements(binds)-1 do (*self.podb)[i]->loadbackplane,binds[j]
  (*self.podb)[i]->opendbsav,/close
endif



return,ret
end

pro pp_titanbrowse::cleardbvecs
;Unloads all core and backplane vectors currently loaded.
compile_opt idl2
for i=0,nfiles-1 do (*self.podb)[i]->unload
end

pro pp_titanbrowse::getproperty,cubelist=cubelist,pixellist=pixellist,update=update,$
 mdbfiles=mdbfiles,odb=odb,std=std,nselcubes=nselcubes,nselpixels=nselpixels,version=version,$
 used_memory=used_memory,evalres=evalres,cubeevalres=cubeevalres,cubehash=cubehash
compile_opt idl2

if arg_present(cubelist) then begin
  ncubs=long(total(*self.nselcubes))
  if (ncubs gt 0) then begin
    cubelist=strarr(1L+ncubs)
    tmp=long(total(*self.nselcubes,/cumul))-*self.nselcubes
    finds=replicate(-1L,ncubs)
    cinds=replicate(-1L,ncubs)
    for i=0L,self.nfiles-1 do if ((*self.nselcubes)[i] gt 0) then begin
      finds[tmp[i]]=replicate(i,(*self.nselcubes)[i])
      cinds[tmp[i]]=*(*self.selcubes)[i]
;      cubelist[tmp[i]]=((*self.podb)[i]->filenames())[*(*self.selcubes)[i]]
    endif
    ci=self->getcubeinfo(file_index=finds,cube_index=cinds)
    lines=string(ci.lines) & samples=string(ci.samples) & pixels=string(ci.pixels) & surf_pixels=string(ci.surf_pixels)
    exposure=string(ci.exposure) & dbind=string(ci.dbind) & cubeind=string(ci.cubeind)
    seq_title=ci.seq_title
    w=where(strlen(seq_title) lt 25,nw)
    if (nw gt 0) then seq_title[w]=string(seq_title[w],format='(A-25)')
    tmp=[[ci.cubefile],[ci.rev],[ci.seq],[seq_title],[ci.prod_id],[ci.start],[ci.stop],[ci.nat_start],$
    [lines],[samples],[pixels],[surf_pixels],[exposure],[ci.ir_mode],[ci.vis_mode],[ci.dbfile],$
    [dbind],[cubeind]]
    cubelist[1]=strjoin(transpose(tmp),string(9B))
    pos=strsplit(cubelist[1],count=npos)
    tmp=string(' ',format='(A'+string(strlen(cubelist[1]))+')')
    names=tag_names(ci[0]) & names=[names[17],names[0:13],names[[16,18,19]]]
    for i=0,npos-1 do strput,tmp,names[i]+string(9B),pos[i]
    cubelist[0]=tmp
  endif else cubelist=''
endif

if (arg_present(pixellist)) then begin
  npix=long(total(*self.nselpixels))
  if (npix gt 0) then begin
    pixellist=strarr(1L+npix)
;    tmp=long(total(*self.nselpixels,/cumul))-*self.nselpixels
;    for i=0,self.nfiles-1 do if ((*self.nselpixels)[i] gt 0) then begin
;      tmp2=strjoin(string(*(*self.selpixels_xz)[i]))
;      pixellist[tmp[i]]=((*self.podb)[i]->filenames())[*(*self.selpixels_c)[i]]+' '+tmp2
;    endif
    pix=self->getselectedpixels()
    tmp=[[pix.cube],[string(pix.x)],[string(pix.z)],[string(pix.backplanes.lat_0)],[string(pix.backplanes.lon_0)]]
    pixellist[1]=strjoin(transpose(tmp),string(9B))
    pos=strsplit(pixellist[1],count=npos)
    tmp=string(' ',format='(A'+string(strlen(pixellist[1]))+')')
    names=['CUBE','X','Z','LAT_0','LON_0']
    for i=0,npos-1 do strput,tmp,names[i]+string(9B),pos[i]
    pixellist[0]=tmp
  endif else pixellist=''
endif

if (arg_present(update)) then update=self.update

if (arg_present(mdbfiles)) then mdbfiles=*self.mdbfiles

if (arg_present(odb)) then odb=*self.podb

if (arg_present(std)) then std=self.std

if (arg_present(nselcubes)) then nselcubes=long(total(*self.nselcubes))

if (arg_present(nselpixels)) then nselpixels=long(total(*self.nselpixels))

if (arg_present(version)) then version=self.version

if (arg_present(cubehash)) then cubehash=self.cubehash

if (arg_present(used_memory)) then begin
  used_memory=0ULL
  for i=0,self.nfiles-1 do begin
    ((*self.podb)[i]).getproperty,used_memory=um
    used_memory+=um
  endfor
endif

if arg_present(evalres) then evalres=self.evalres ? *self.evalres : !null
if arg_present(cubeevalres) then cubeevalres=self.cubeevalres ? *self.cubeevalres : !null

end

function pp_titanbrowse::getselectedpixels
compile_opt idl2
ret=-1
nsel=long(total(*self.nselpixels))
if (nsel gt 0L) then begin
  ret=replicate({pp_titanbrowse_pixdata},nsel)
  tn=tag_names(ret[0].backplanes)
  nt=n_elements(tn)
  k=0L
  for i=0,self.nfiles-1 do if ((*self.nselpixels)[i] gt 0) then begin
    cubes=*((*self.selpixels_c)[i]) ;indexes of tue cubes of the selected pixels
    pixxz=*((*self.selpixels_xz)[i]) ;x and z indexes of the selected pixels
    for j=0,n_elements(cubes)-1 do begin
      if ((j eq 0) || cubes[j] ne cubes[j-1]) then begin ;Get cube data if necessary
        cube=(*self.podb)[i]->getcube(cubes[j])
        cube->getproperty,core=core,backplanes=backplanes,file=file
      endif
      ret[k].core=core[pixxz[0,j],pixxz[1,j],*]
      ret[k].x=pixxz[0,j] & ret[k].z=pixxz[1,j]
      ret[k].cube=file
      for l=0,nt-1 do ret[k].backplanes.(l)=backplanes[pixxz[0,j],pixxz[1,j],l]
      k++
    endfor
  endif
endif
return,ret
end

function pp_titanbrowse::getselectedcubes
compile_opt idl2
ret=-1
nsel=long(total(*self.nselcubes))
if (nsel gt 0L) then begin
  ret=objarr(nsel)
  k=0L
  for i=0,self.nfiles-1 do if ((*self.nselcubes)[i] gt 0) then begin
    ret[k]=(*self.podb)[i]->getcube(*((*self.selcubes)[i]))
    k+=(*self.nselcubes)[i]
  endif
endif
return,ret
end

pro pp_titanbrowse::setproperty,update=update
compile_opt idl2
if (n_elements(update) eq 1) then self.update=update
end

function pp_titanbrowse::getcubeinfo,file_index=finds,cube_index=cinds,cube_name=cnames,cube_object=cobj
;Retrieve metadata for the cubes selected, by their names, or by db file and cube indexes
compile_opt idl2
ret=-1
nc=n_elements(cnames)
if (nc gt 0) then begin ;Selection by cube name (just prepare finds and cinds)
  finds=lonarr(nc) & cinds=lonarr(nc) &found=bytarr(nc)
  for i=0,self.nfiles-1 do if (total(found) lt nc) then begin
    names=(*self.podb)[i]->filenames()
    wtmp=where(~found,nwtmp)
    for j=0,nwtmp-1 do begin
      k=wtmp[j]
      w=where(strmatch(names,cnames[k]),nw)
      if nw gt 0 then begin & finds[k]=i & cinds[k]=w & found[k]=1 & endif
    endfor
  endif
  if (total(found) lt nc) then message,'Cube(s) not found: '+cnames[where(~found)]
endif
nf=n_elements(finds) & nc=n_elements(cinds)
if (nf gt 0)&&(nc gt 0)&&(nf eq nc) then begin ;Selection by file and cube indexes
  if arg_present(cobj) then cobj=objarr(nf) 
  ret=replicate({pp_titanbrowse_cubeinfo},nf)
  ret[*].dbfile=(*self.mdbfiles)[finds]
  ret[*].dbind=finds
  ret[*].cubeind=cinds
  for i=0,nf-1 do begin
    if ((i eq 0) || (finds[i] ne finds[i-1])) then begin
      cmd=*((*self.podb)[finds[i]]->getcmd())
      names=(*self.podb)[finds[i]]->filenames()
    endif
    cind=cinds[i]
    if arg_present(cobj) then cobj[i]=*((*self.podb)[finds[i]])->getcube(cind)
    ret[i].cubefile=names[cind]
;Convert the elements from the cmd fields into the fields of ret
;Only works this because both were defined with matching fields
    for j=0,n_tags(cmd)-3 do ret[i].(j)=cmd.(j)[cind]
    for j=0,n_tags(cmd.back_max)-1 do begin
      ret[i].back_max.(j)=cmd.back_max.(j)[cind]
      ret[i].back_min.(j)=cmd.back_min.(j)[cind]
    endfor
  endfor
endif else message,'Invalid selection specification'
return,ret
end

function pp_titanbrowse::getpixeldata,pixin
compile_opt idl2,logical_predicate

np=n_elements(pixin)
if ~np then return,!null

ret=replicate({pp_titanbrowse_pixdata},np)
l=pp_locate(pixin.cubefile)
cube_names=(l.keys()).toarray()
ci=self.getcubeinfo(cube_name=cube_names)

foreach cube,cube_names,icube do begin
  self.selectcubes,/all
  self.selectcubes,"cmd.file eq '"+cube+"'",count=count
  self.selectpixels,/all,count=count
  inds=pixin.x+pixin.y*ci[icube].samples
  print,count
endforeach

return,ret

end

pro pp_titanbrowse::cleanup
compile_opt idl2,hidden
ptr_free,self.mdbfiles,self.std,self.nselcubes,self.nselpixels
ptr_free,*self.selcubes,*self.selpixels,self.selcubes,self.selpixels_c,self.selpixels,self.selpixels_xz
ptr_free,*self.dbvecs.core,*self.dbvecs.back,self.dbvecs.core,self.dbvecs.back
if (self.nfiles gt 0) then begin
  obj_destroy,(*self.podb)
  ptr_free,self.podb
endif
end

pro pp_titanbrowse__define
;Object implementing the new titanbrowse, both the procedural API and GUI.
;Initialized from pp_titanbrowse_metadb savefiles.
compile_opt idl2
void={pp_titanbrowse_cubemd,dbindex:0L,pstart:0L,npixels:0L,lines:0L,samples:0L}
void={pp_titanbrowse,version:'',nfiles:0,mdbfiles:ptr_new(),podb:ptr_new(),std:ptr_new(),$
 dbvecs:{pp_titanbrowse_dbvecs,core:ptr_new(),back:ptr_new()},$
 nselcubes:ptr_new(),nselpixels:ptr_new(),selcubes:ptr_new(),selpixels:ptr_new(),$
 selpixels_c:ptr_new(),selpixels_xz:ptr_new(),update:0B,$
 backindex:obj_new(),inherits IDL_Object,evalres:ptr_new(),cubeevalres:ptr_new(),$
 cubehash:obj_new()}
end
