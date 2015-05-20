; docformat = 'rst'
;+
; :Uses: pp_editablecube__define, pp_getcubeheadervalue, pp_extractfields, pp_setcubeheadervalue,
; pp_readcube__define, pp_cubecollection, pp_titanbrowse_metadb
; 
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Nov/2009
;-

function pp_titanbrowse_db::init,metadbfile,build=build,compress=compress
compile_opt idl2
ret=0

;Defaults
build=n_elements(build) eq 1 ? build : 0
compress=n_elements(compress) eq 1 ? compress : 1B

;Initialize the meta db object from its savefile
ret=self->pp_titanbrowse_metadb::init(metadbfile)
if (ret eq 0) then return,ret
self.dbfile=file_dirname(metadbfile,/mark)+file_basename(metadbfile,'.sav')+'_db.sav'

if build then begin ;If the database has to be created
  error_status=0;catch,error_status
  if (error_status ne 0) then begin
    catch,/cancel
    return,0
  endif else begin
    self->build_db,compress=compress
    ret=1
  endelse
endif
odbsav=obj_new('idl_savefile',self.dbfile,/relaxed_structure_assignment)
if (~build) then begin; If restoring from a savefile
  odbsav->restore,'idstring'
  if (idstring[2] ne 'pp_titanbrowse_db_container') then begin
    print,'Not a pp_titanbrowse_db savefile'
    return,0
  endif
  odbsav->restore,['npixels','pstart','coreheapinds','backheapinds']
  self.npixels=npixels
  self.pstart=ptr_new(pstart,/no_copy)
  self.coreheapinds=ptr_new(coreheapinds,/no_copy)
  self.backheapinds=ptr_new(backheapinds,/no_copy)
  self.pbands=ptr_new(ptrarr(self.std.bands),/no_copy)
  self.pbacks=ptr_new(ptrarr(self.std.nback),/no_copy)
endif
self.odbsav=odbsav
obj_destroy,self.odbsav
return,ret
end

pro pp_titanbrowse_db::build_db,compress=compress
;Processes the cubes to build the database savefile.
compile_opt idl2,hidden
npix=(*self.cmd).pixels
npixels=total(npix) ;Number of spatial pixels in this database
pstart=long(total(npix,/cumul)-npix) ;Index of the first pixel of each cube
;Reorient data from cube major to band/backplane major order
pbands=ptrarr(self.std.bands) ;Pointer to the vector of each band
pbacks=ptrarr(self.std.nback) ;Pointer to the vector of each backplane
for i=0,self.std.bands-1 do pbands[i]=ptr_new(make_array(npixels,type=self.std.type,/nozero),/no_copy)
for i=0,self.std.nback-1 do pbacks[i]=ptr_new(make_array(npixels,type=self.std.type,/nozero),/no_copy)
print,'Building db file, this may take a long time and use a lot of memory if there are many cubes'
for i=0L,self.ncubes-1 do begin 
  print,'Processing cube ',strcompress(string(i+1,'/',self.ncubes),/rem)
  ocube=self->getcube(i)
;Get the data from the cube
  ocube->getproperty,backnames=bnames,backunits=bunits,wavelengths=wavs,units=wunits,lines=lines,samples=samples
  if (strlowcase(wunits) eq strlowcase(self.std.unit)) then begin ;Only put data into database if units match
;Match the wavelengths between cube and database
    if array_equal(wavs,*self.std.wavs) then core=ocube->getproperty(/core) else begin
      core=replicate(*self.std.fill,samples,lines,self.std.bands)
      catch,error_status
      if (error_status ne 0) then catch,/cancel else begin
        tmp=ocube->getbandbywavelength(self.std.wavs,wavelengths=stmp)
        w=where(self.wavs ne stmp,nw)
        if (nw gt 0) then core[*,*,w]=*self.std.fill ;Fill with null where the needed wavelength is not avaialable
        catch,/cancel
      endelse
    endelse
  endif else core=replicate(*self.std.fill,samples,lines,self.std.bands) ;Fill with null if units are different
;Get the backplanes by their names
  sbnames=strlowcase(*self.std.bnames)
  bnames=strlowcase(bnames)
  sbunits=strlowcase(*self.std.bunits)
  bunits=strlowcase(bunits)
  if (array_equal(sbnames,bnames) && array_equal(sbunits,bunits)) then $
   backplanes=ocube->getproperty(/backplanes) else begin
    catch,error_status
    if (error_status ne 0) then catch,/cancel else begin
      backplanes=ocube->getsuffixbyname(sbnames,found=found,index=index)
;Check that the units match
      for j=0,self.std.nback-1 do if (found[j] gt 0)&&(bunits[index[j]] ne sbunits[j]) then backplanes[*,*,j]=*self.std.fill
      catch,/cancel
    endelse 
  endelse
;Put the data into the database arrays
  for j=0,self.std.bands-1 do (*pbands[j])[pstart[i]]=reform(core[*,*,j],samples*lines)
  for j=0,self.std.nback-1 do (*pbacks[j])[pstart[i]]=reform(backplanes[*,*,j],samples*lines)
;Get rid of the cube object
  obj_destroy,ocube
endfor
;Get the heap indexes of the core bands and backplanes
coreheapinds=long(strsplit(strjoin(string(pbands,/print)),'<PtrHeapVar',/regex,/extract))
backheapinds=long(strsplit(strjoin(string(pbacks,/print)),'<PtrHeapVar',/regex,/extract))
;Save the db to the file
idstring=[self.idstring,'pp_titanbrowse_db_container']
print,'Writing savefile "',strtrim(self.dbfile,2),'"'
save,file=self.dbfile,compress=compress,idstring,npixels,pstart,coreheapinds,backheapinds,pbands,pbacks
self.pstart=ptr_new(pstart,/no_copy)
self.npixels=npixels
self.coreheapinds=ptr_new(coreheapinds,/no_copy)
self.backheapinds=ptr_new(backheapinds,/no_copy)
self.pbands=ptr_new(ptrarr(self.std.bands),/no_copy)
self.pbacks=ptr_new(ptrarr(self.std.nback),/no_copy)
ptr_free,pbands,pbacks
end

pro pp_titanbrowse_db::opendbsav,close=close
compile_opt idl2
if keyword_set(close) then obj_destroy,self.odbsav else if (~obj_valid(self.odbsav)) $
 then self.odbsav=obj_new('idl_savefile',self.dbfile,/relaxed_structure_assignment)
end

function pp_titanbrowse_db::getband,index,pointer=pointer
;Returns the band vector of the selected index.
;If pointer is set, the vector is loaded into the internal pointer array, and its
;pointer is returned instead.
compile_opt idl2
ni=n_elements(index)
if keyword_set(pointer) then begin
  self->loadband,index
  ret=(*self.pbands)[index]
endif else begin
  sav=obj_valid(self.odbsav)
  if (~sav) then self.odbsav=obj_new('idl_savefile',self.dbfile,/relaxed_structure_assignment)
  for i=0,ni-1 do begin
    self.odbsav->restore,(*self.coreheapinds)[index],/pointer_heapvar,new_heapvar=tmp
    ret=i eq 0 ? tmp : [[ret],[tmp]]
  endfor
  if (~sav) then obj_destroy,self.odbsav
endelse
return,ret
end

function pp_titanbrowse_db::getbackplane,index,pointer=pointer
;Returns the backplane vector of the selected index.
;If pointer is set, the vector is loaded into the internal pointer array, and its
;pointer is returned instead.
compile_opt idl2
ni=n_elements(index)
if keyword_set(pointer) then begin
  self->loadbackplane,index
  ret=(*self.pbacks)[index]
endif else begin
  sav=obj_valid(self.odbsav)
  self.odbsav=obj_new('idl_savefile',self.dbfile,/relaxed_structure_assignment)
  for i=0,ni-1 do begin
    self.odbsav->restore,(*self.backheapinds)[index],/pointer_heapvar,new_heapvar=tmp
    ret=i eq 0 ? tmp : [[ret],[tmp]]
  endfor
  if (~sav) then obj_destroy,self.odbsav
endelse
return,ret
end

pro pp_titanbrowse_db::loadband,index
;Loads the selected band into the internal pointer array.
compile_opt idl2
case self.std.type of
  4: typesize=4
  5: typesize=8
  1: typesize=1
  2: typesize=2
  3: typesize=4
  6: typesize=8
  9: typesize=16
  12: typesize=2
  13: typesize=4
  14: typesize=8
  else:typesize=1
endcase
ni=n_elements(index)
sav=obj_valid(self.odbsav)
if (~sav) then self.odbsav=obj_new('idl_savefile',self.dbfile,/relaxed_structure_assignment)
for i=0,ni-1 do begin
  if (~ptr_valid((*self.pbands)[index[i]])) then begin
    self.odbsav->restore,(*self.coreheapinds)[index[i]],/pointer_heapvar,new_heapvar=ret
    (*self.pbands)[index[i]]=ret
    self.usedmem+=n_elements(*ret)*typesize
  endif
endfor
if (~sav) then obj_destroy,self.odbsav
end

pro pp_titanbrowse_db::loadbackplane,index
;Loads the selected backplane into the internal pointer array.
compile_opt idl2
case self.std.type of
  4: typesize=4
  5: typesize=8
  1: typesize=1
  2: typesize=2
  3: typesize=4
  6: typesize=8
  9: typesize=16
  12: typesize=2
  13: typesize=4
  14: typesize=8
  else:typesize=1
endcase
ni=n_elements(index)
sav=obj_valid(self.odbsav)
if (~sav) then self.odbsav=obj_new('idl_savefile',self.dbfile,/relaxed_structure_assignment)
for i=0,ni-1 do begin
  if (~ptr_valid((*self.pbacks)[index[i]])) then begin
  self.odbsav->restore,(*self.backheapinds)[index[i]],/pointer_heapvar,new_heapvar=ret
  (*self.pbacks)[index[i]]=ret
  self.usedmem+=n_elements(*ret)*typesize
  endif
endfor
if (~sav) then obj_destroy,self.odbsav
end

pro pp_titanbrowse_db::unload
;Unloads all currently loaded band and backplane vectors.
compile_opt idl2
ptr_free,*self.pbands,*self.pbacks
end

pro pp_titanbrowse_db::getproperty,std=std,pstart=pstart,used_memory=used_memory,revs=revs
compile_opt idl2
if arg_present(std) then std=self.std
if arg_present(pstart) then pstart=self.pstart
if arg_present(used_memory) then used_memory=self.usedmem
if arg_present(revs) then begin
  if ~obj_valid(self.revh) then begin
  cmd=self.getcmd()
  revs=(*cmd).rev
  self.revh=pp_locate(revs,unique_ind=revsu)
  self.revsu=ptr_new(revs[revsu])
  endif
  revs=*(self.revsu)
endif
end

pro pp_titanbrowse_db::cleanup
compile_opt idl2,hidden
self->pp_titanbrowse_metadb::cleanup
ptr_free,self.pstart,self.coreheapinds,self.backheapinds
obj_destroy,self.odbsav
ptr_free,*self.pbands,*self.pbacks,self.pbands,self.pbacks
end

pro pp_titanbrowse_db__define
;Object implementing the pixel database, from a pp_titanbrowse_metadb object.
;Contains band/backplane-major vectors of all core bands and backplanes for faster access.
compile_opt idl2
void={pp_titanbrowse_db,inherits pp_titanbrowse_metadb,dbfile:'',npixels:0L,$
 pstart:ptr_new(),odbsav:obj_new(),coreheapinds:ptr_new(),backheapinds:ptr_new(),$
 pbands:ptr_new(),pbacks:ptr_new(),usedmem:0ULL,revh:obj_new(),revsu:ptr_new()}
end
