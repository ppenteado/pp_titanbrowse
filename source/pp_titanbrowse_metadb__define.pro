; docformat = 'rst'
;+
; :Uses: pp_editablecube__define, pp_getcubeheadervalue, pp_extractfields, pp_setcubeheadervalue, pp_readcube__define
; 
; :Author: Paulo Penteado (pp.penteado@gmail.com), Nov/2009
;-

function pp_titanbrowse_metadb::init,modelindex=modind,build_db=build_db,$
 savefile,build_collection=build_col,cubefiles=cubefiles,vis=vis,ir=ir,compress=compress
compile_opt idl2

compress=n_elements(compress) eq 1 ? compress : 1B

;Initialize collection object
ret=self->pp_cubecollection::init(savefile,build=build_col,cubefiles=cubefiles,vis=vis,ir=ir,compress=compress)
if (ret eq 0) then return,ret
;Defaults
modind=n_elements(modind) eq 1 ? modind : 0
modind<=(self.ncubes-1)
build_db=n_elements(build_db) eq 1 ? build_db : 0

;Id to test if savefile was created by this object
self.idstring=['pp_cubecollection_container','pp_titanbrowse_metadb_container']
if (~obj_valid(self.osav)) then self.osav=obj_new('idl_savefile',self.savefile)
if (~build_db) then begin ;If metadb is to be read from savefile
;Determine if the savefile already contains a metadb
  self.osav->restore,'idstring'
  if (n_elements(idstring) lt 2) || (idstring[1] ne self.idstring[1]) then begin
    print,'Not a pp_titanbrowse_metadb savefile'
    return,0
  endif
  self.osav->restore,['modind','cmd','std']
  obj_destroy,self.osav
  self.modind=modind
  self.cmd=ptr_new(cmd,/no_copy)
  self.std=std
  ret=1
endif else begin ;If meta db has to be constructed from a collection file
  ret=self->parsecubes(compress=compress)
endelse
obj_destroy,self.osav
return,ret
end

function pp_titanbrowse_metadb::parsecubes,compress=compress
;Parse the cubes in the collection to determine their metadata and
;eliminate those that do not have the minimum requirements
compile_opt idl2
ret=0
;In this routine, retrieved cube objects must not be destroyed, since they must
;remain in the collection while the pp_titanbrowse_metadb object exists.

;Determine what the database should contain from the model cube, to account for
;cubes with incomplete data
omodel=self->getcube(self.modind)
omodel->getproperty,backnames=bnames,backunits=bunits,wavelengths=wavs,$
 units=wunits,nback=nback,bands=nwavs
tnames=idl_validname(bnames,/convert_all)
self.std.nback=nback & self.std.bands=nwavs & self.std.wavs=ptr_new(wavs)
self.std.bnames=ptr_new(bnames) & self.std.unit=wunits & self.std.bunits=ptr_new(bunits)
self.std.tnames=ptr_new(tnames)
tmp=omodel->getbandbywavelength(1.0)
self.std.type=size(tmp,/type)
case self.std.type of
  4 : self.std.fill=ptr_new(!values.f_nan)
  5 : self.std.fill=ptr_new(!values.d_nan)
  else: self.std.fill=ptr_new((omodel->getspecial()).null) 
endcase
;Make a structure for the backplane ranges
;void=execute('btmp={'+strjoin(tnames,':!values.d_nan,')+':!values.d_nan}')
btmp=create_struct(tnames[0],!values.d_nan)
for i=1,nback-1 do btmp=create_struct(btmp,tnames[i],!values.d_nan)
;Temporary places to keep cube metadata
void={usecube:0B,ocube:obj_new(),rev:'',seq:'',seq_title:'',prod_id:'',start:'',stop:'',nat_start:0d0,$
 lines:0L,samples:0L,pixels:0L,surf_pixels:0L,exposure:0d0,ir_mode:'',vis_mode:'',file:'',$
 back_max:btmp,back_min:btmp}
cinfo=replicate(void,self.ncubes)
print,'Building metadb file, this may take a long time and use a lot of memory if there are many cubes'
for i=0L,self.ncubes-1 do begin
  error_status=0;catch,error_status
  if (error_status ne 0) then begin
    catch,/cancel
    cinfo[i].usecube=0
  endif else begin
    print,'Parsing cube ',(*self.cubefiles)[i],' (',strcompress(string(i+1,'/',self.ncubes),/remove),')'
    cinfo[i].ocube=self->getcube(i)
    count=1
    cinfo[i].seq=cinfo[i].ocube->getfromheader('SEQUENCE_ID',/unquote,count=tmp) & count*=tmp
    cinfo[i].seq_title=cinfo[i].ocube->getfromheader('SEQUENCE_TITLE',/unquote,count=tmp) & count*=tmp
    if (tmp eq 0) then continue ;If some of the required parameters is missing, skip this cube
;Get cube-constant metadata
    cinfo[i].rev=(strsplit(cinfo[i].seq_title,'_',/extract))[1]
    cinfo[i].prod_id=cinfo[i].ocube->getfromheader('PRODUCT_ID',/unquote)
    cinfo[i].start=cinfo[i].ocube->getfromheader('START_TIME',/unquote)
    cinfo[i].stop=cinfo[i].ocube->getfromheader('STOP_TIME',/unquote)
    cinfo[i].nat_start=double(cinfo[i].ocube->getfromheader('NATIVE_START_TIME',/unquote))
    cinfo[i].lines=cinfo[i].ocube->getproperty(/lines)
    cinfo[i].samples=cinfo[i].ocube->getproperty(/samples)
    cinfo[i].pixels=cinfo[i].samples*cinfo[i].lines
    lats=cinfo[i].ocube->getsuffixbyname('LATITUDE')
    cinfo[i].surf_pixels=total(finite(lats))
    expdur=double(cinfo[i].ocube->getfromheader('EXPOSURE_DURATION',count=tmp,/unquote))
    for j=0,tmp-1 do begin
      if (n_elements(expdur) eq 1) then break
      expdur=double(cinfo[i].ocube->getfromheader('EXPOSURE_DURATION',/unquote,sel=i))
    endfor
    cinfo[i].exposure=expdur
    tmp=cinfo[i].ocube->getfromheader('SAMPLING_MODE_ID',/unquote)
    cinfo[i].ir_mode=tmp[0]
    cinfo[i].vis_mode=tmp[1]
    cinfo[i].ocube->getproperty,file=file
    cinfo[i].file=file
;Get ranges for pixel-variable metadata
    for j=0,nback-1 do begin
      tmp=cinfo[i].ocube->getsuffixbyname(bnames[j])
      cinfo[i].back_min.(j)=min(tmp,max=mtmp,/nan)
      cinfo[i].back_max.(j)=mtmp
    endfor
    cinfo[i].usecube=1
  endelse
endfor
error_status=0;catch,error_status
if (error_status ne 0) then begin
  catch,/cancel
  ret=0
endif else begin
;Keep only the selected cubes
  w=where(cinfo.usecube,ncubes,/l64)
  cinfo=cinfo[w]
  ocubes=cinfo.ocube
  self.ncubes=ncubes
  *self.cubefiles=(*self.cubefiles)[w]
;Build index of heap variables, needed to retrieve individual elements of ocube from savefile
  *self.heapinds=long(strsplit(strjoin(string(ocubes,/print)),'<ObjHeapVar',/regex,/extract))
;Reorient for faster access
  ;btmp=create_struct(name='pp_titanbrowse_metadb_cmd',tnames[0],dblarr(ncubes,/nozero))
  btmp=create_struct(tnames[0],dblarr(ncubes,/nozero))
  for i=1,nback-1 do btmp=create_struct(btmp,tnames[i],dblarr(ncubes,/nozero))
  bmin=btmp & bmax=btmp
  for i=0,nback-1 do bmin.(i)=cinfo[*].back_min.(i)
  for i=0,nback-1 do bmax.(i)=cinfo[*].back_max.(i)  
  cmd={rev:cinfo.rev,seq:cinfo.seq,seq_title:cinfo.seq_title,$
   prod_id:cinfo.prod_id,start:cinfo.start,stop:cinfo.stop,nat_start:cinfo.nat_start,$
   lines:cinfo.lines,samples:cinfo.samples,pixels:cinfo.pixels,surf_pixels:cinfo.surf_pixels,$
   exposure:cinfo.exposure,ir_mode:cinfo.ir_mode,vis_mode:cinfo.vis_mode,file:cinfo.file,$
   back_max:bmax,back_min:bmin}
;Save db to file
  print,'Writing savefile "',strtrim(self.savefile,2),'"'
  obj_destroy,self.osav
  savefile=self.savefile & idstring=self.idstring & cubefiles=*self.cubefiles
  heapinds=*self.heapinds & modind=self.modind & std=self.std
  save,file=savefile,idstring,ncubes,cubefiles,heapinds,ocubes,$
   modind,cmd,std,compress=compress
  obj_destroy,ocubes
  ;self.osav=obj_new('idl_savefile',self.savefile)
  self.cmd=ptr_new(cmd,/no_copy)
  ret=1
endelse

return,ret
end

function pp_titanbrowse_metadb::getcmd
;Retrieves the pointer to the cube metadata structure
compile_opt idl2, hidden
return,self.cmd
end

function pp_titanbrowse_metadb::getcubevars,level
  ;Retrieves the cube variables tree nodes at the given level
  compile_opt idl2, hidden
  ret=[]
  c=(*self.cmd)
  if (level eq 0) then begin
    h=orderedhash(c)
    ;h.remove,["BACK_MAX","BACK_MIN"]
    ret=(h.keys()).toarray()
  endif
  if (level eq 1) then begin
    h=orderedhash(c.back_max)
    ret=(h.keys()).toarray()
  endif
  return,ret
end

function pp_titanbrowse_metadb::getpixelvars,level
  ;Retrieves the cube variables tree nodes at the given level
  compile_opt idl2, hidden
  ret=[]
  if (level eq "") then begin
    ret=["Core bands","Backplanes"]
  endif
  if (level eq "Backplanes") then begin
    c=(*self.cmd)
    h=orderedhash(c.back_max)
    ret=(h.keys()).toarray()
  endif
  if (level eq "Core bands") then begin
    bands=self.std.bands
    wavs=*(self.std.wavs)
    ret=strtrim(sindgen(bands),2)+" ("+string(wavs,format='(F6.4)')+" µm)"
  endif
  return,ret
end

pro pp_titanbrowse_metadb::cleanup
compile_opt idl2,hidden
self->pp_cubecollection::cleanup
ptr_free,self.cmd,self.std.wavs,self.std.bnames,self.std.tnames,self.std.bunits,self.std.fill
end

pro pp_titanbrowse_metadb__define
;Object containing a database of metadata from a pp_cubecollection.
;Includes metadata of all cubes, plus the pp_editablecube objects of all cubes, so they can be regenerated.
compile_opt idl2
void={pp_titanbrowse_metadb,inherits pp_cubecollection,modind:0L,cmd:ptr_new(),$
 idstring:strarr(2),std:{pp_titanbrowse_metadb_std,bands:0L,nback:0L,wavs:ptr_new(),$
 bnames:ptr_new(),tnames:ptr_new(),unit:'',bunits:ptr_new(),type:0,fill:ptr_new()}}
end
