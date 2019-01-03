; docformat = 'rst'
;+
; :Uses: pp_getcubeheadervalue, pp_extractfields, pp_buffered_vector, pp_setcubeheadervalue
; 
; :Author: Paulo Penteado (pp.penteado@gmail.com), Oct/2009
;-

;+
; :Description:
;    Initializes am editablecube object, either from a readcube object, or from a file.
;
; :Keywords:
;    orcube : in, optional
;      A readcube or editablecube object from which a copy will be created to be edited in this object. The
;      original object is unchanged.
;    file : in, optional
;      The filename from which to read the cube directly. If provided, orcube is ignored.
;    special : in, optional, default=0
;      Same as pp_readcube::init's special:
;      
;      Determines the type of special value replacement to use:
;      
;      0 uses the default special values
;      
;      1 uses the special values found in the header
;      
;      2 disables special value replacement
;      
;    preservespecial : in, optional, default=0
;      If set, the special values used in the original cube are kept (unless data type changes).
;      Otherwise, default values are used.
;
; :Examples:
;    See the example on pp_editablecube__define.
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Oct/2009
;-
function pp_editablecube::init,orcube=orcube,file=file,special=special,preservespecial=preservespecial
compile_opt idl2,hidden

ret=0
;If file is provided, read the cube with the methods inherited from readcube
if (n_elements(file) eq 1) then ret=self->pp_readcube::init(file,special=special) else begin
;If a readcube/editablecube object is provided, make a copy of its data
  orcube->getproperty,all=all
  self.file=all.file
  self.special=all.special
  self.labels=ptr_new(all.labels)
  self.tlabels=ptr_new(strtrim(all.labels,2))
  self.history=ptr_new(all.history)
  self.thistory=ptr_new(strtrim(all.history,2))
  self.core=ptr_new(all.core)
  if (all.nback gt 0) then self.backplanes=ptr_new(all.backplanes)
  if (all.nside gt 0) then self.sideplanes=ptr_new(all.sideplanes)
  if (all.nbottom gt 0) then self.bottomplanes=ptr_new(all.bottomplanes)
  self.info=all.info
  self.raw=ptr_new(all.raw)
  self.wavs=ptr_new(all.wavelengths)
  self.bnames=ptr_new(all.backnames)
  self.snames=ptr_new(all.sidenames)
  self.bonames=ptr_new(all.bottomnames)
  self.units=all.units
  self.bunits=ptr_new(all.backunits)
  self.bunits=ptr_new(all.sideunits)
  self.bunits=ptr_new(all.bottomunits)
  ret=1
endelse
if (ret eq 0) then return,ret
;Keep an unaltered copy of the original header parts
self.oldlabels=ptr_new(*self.labels)
self.oldhistory=ptr_new(*self.history)
;Count the length of the old header parts (in bytes)
self.llength=strlen(strjoin(*self.labels))+2L*n_elements(*self.labels)
self.oldllength=self.llength
self.hlength=strlen(strjoin(*self.history))+2L*n_elements(*self.history)
self.oldhlength=self.hlength
;Count the length of the raw binary part
self.binlength=n_elements(*self.raw)*self.info.bytes
;Set the treatment to do to special value labels if type changes
if (n_elements(preservespecial) eq 1) then self.preservespecial=preservespecial

return,ret
end

;+
; :Description:
;    Changes one or more properties of the cube, according to the data given in the
;    keywords. All keywords are optional.
;
; :Returns:
;    The keywords marked as both in and out, if undefined or invalid at input,
;    return the assigned default values to their content. Otherwise, all keywords
;    are input only and unchanged.
;
; :Keywords:
;    core : in, optional
;      An array to replace the core data with. Can be of any size, but must be 3D,
;      and can be of any numeric type. The object's data type gets replaced by the
;      type of core, if it is different. This is the only way to change the cube's
;      data type. To remove existing suffixes, change the 3 core dimensions without
;      providing corresponding suffix arrays.
;    backplanes : in, optional
;      An array to replace the backplana data with. Must have the first two dimensions
;      equal to the cube's core, can have any number of planes (must be 3D). Most useful
;      to add new backplanes.
;    sideplanes : in, optional
;      An array to replace the sideplane data with. Must have the last two dimensions
;      equal to the cube's core, can have any number of planes (must be 3D).
;    bottomplanes : in, optional
;      An array to replace the bottom data with. Must have the first and last dimensions
;      equal to the cube's core, can have any number of planes (must be 3D).
;    backnames : in, out, optional
;      A string array with each element containing the name of each backplane. If the number
;      of backplanes is changed and this is not provided, the backplane names get changed to
;      default values.
;    sidenames : in, out, optional
;      A string array with each element containing the name of each sideplane. If the number
;      of backplanes is changed and this is not provided, the sideplane names get changed to
;      default values.
;    bottomnames : in, out, optional
;      A string array with each element containing the name of each bottomplane. If the number
;      of backplanes is changed and this is not provided, the bottomplane names get changed to
;      default values.
;    wavelengths : in, out, optional
;      A string array where each element is the wavelength of each core band. If the number of
;      core bands is changed and this is not provided, the wavelengths get changed to their
;      default value 'UNKNONW'.
;    backunits : in, out, optional
;      A string array with each element containing the name of each backplane's unit. If the number
;      of backplanes is changed and this is not provided, the backplane units get changed to
;      default values 'UNKNOWN'.
;    sideunits : in, out, optional
;      A string array with each element containing the name of each sideplane's unit. If the number
;      of backplanes is changed and this is not provided, the sideplane units get changed to
;      default values 'UNKNOWN'.
;    bottomunits : in, out, optional
;      A string array with each element containing the name of each bottomplane's unit. If the number
;      of backplanes is changed and this is not provided, the bottomplane units get changed to
;      default values 'UNKNOWN'.
;    wavelengthunits : in, optional
;      A string array with the name of the core bands unit.
;
; :Examples:
;    See the example on pp_editablecube__define.
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Oct/2009
;-
pro pp_editablecube::setproperty,core=core,$
 backplanes=backplanes,sideplanes=sideplanes,bottomplanes=bottomplanes,$
 backnames=bnames,sidenames=snames,bottomnames=bonames,wavelengths=wavs,$
 backunits=bunits,sideunits=sunits,bottomunits=bounits,wavelengthunits=wunits
compile_opt idl2

typechanged=0
dimschanged=[0,0,0]
corechange=(size(core,/n_dimensions) eq 3)
backchange=(size(backplanes,/n_dimensions) eq 3)
sidechange=(size(sideplanes,/n_dimensions) eq 3)
bottomchange=(size(sideplanes,/n_dimensions) eq 3)

;Update the core if provided
if corechange then self->updatecore,core,dimschanged,typechanged
anydimschanged=(total(dimschanged) ne 0)
oldsuffdims=self.info.suffdims
;Update the backplanes if provided
if backchange||dimschanged[0]||dimschanged[1]||typechanged then self->updatesuffix,suffix=backplanes,'BAND',2,dimschanged,typechanged
;Update the sideplanes if provided
if sidechange||dimschanged[1]||dimschanged[2]||typechanged then self->updatesuffix,suffix=sideplanes,'SAMPLE',0,dimschanged,typechanged
;Update the bottomplanes if provided
if bottomchange||dimschanged[0]||dimschanged[2]||typechanged then self->updatesuffix,suffix=bottomplanes,'LINE',1,dimschanged,typechanged

;Fix suffix dimension information, if it changed
if (~array_equal(self.info.suffdims,oldsuffdims)) then begin
  self.info.dims=self.info.coredims+self.info.suffdims
  pp_setcubeheadervalue,*self.labels,'SUFFIX_ITEMS',self.info.suffdims
endif

;Replace the raw data, if necessary
if (corechange||backchange||sidechange||bottomchange) then begin
    ptr_free,self.raw
    raw=make_array(self.info.dims,type=self.info.datatype,/nozero)
    special=self->getspecialvalues(default=~self.preservespecial)
    raw[*]=special.null
    cd=self.info.coredims
    sd=self.info.suffdims
    raw[0:cd[0]-1,0:cd[1]-1,0:cd[2]-1]=*self.core
    if (sd[2] gt 0) then raw[0:cd[0]-1,0:cd[1]-1,cd[2]:cd[2]+sd[2]-1]=*self.backplanes
    if (sd[0] gt 0) then raw[cd[0]:cd[0]+sd[0]-1,0:cd[1]-1,0:cd[2]-1]=*self.sideplanes
    if (sd[1] gt 0) then raw[0:cd[0]-1,cd[1]:cd[1]+sd[1]-1,0:cd[2]-1]=*self.bottomplanes
    byteorder,raw,swap_if_big_endian=self.info.littleendian,$
     swap_if_little_endian=~self.info.littleendian
    self.raw=ptr_new(raw,/no_copy)
endif

;Update the wavelength units if provided
if (n_elements(wunits) eq 1) then begin
  self.wunits=wunits
  pp_setcubeheadervalue,*self.labels,'CORE_UNIT',self.wunits
endif
;Update the suffix units if provided
if (self.info.suffdims[2] gt 0) then begin
  *self.bunits=(n_elements(bunits) eq self.info.suffdims[2]) ? bunits : replicate('UNKNOWN',self.info.suffdims[2])
  pp_setcubeheadervalue,*self.labels,'BAND_SUFFIX_UNIT',*self.bunits
endif
if (self.info.suffdims[0] gt 0) then begin
  *self.sunits=(n_elements(sunits) eq self.info.suffdims[0]) ? sunits : replicate('UNKNOWN',self.info.suffdims[0])
  pp_setcubeheadervalue,*self.labels,'SAMPLE_SUFFIX_UNIT',*self.sunits
endif
if (self.info.suffdims[1] gt 0) then begin
  *self.bounits=(n_elements(bounits) eq self.info.suffdims[1]) ? bounits : replicate('UNKNOWN',self.info.suffdims[1])
  pp_setcubeheadervalue,*self.labels,'LINE_SUFFIX_UNIT',*self.info.bounits
endif

;Update the wavelengths if provided
if dimschanged[2]||(n_elements(wavs) gt 0) then self->updatedatainfo,names=wavs,/core
;Update names of backplanes if provided
if (oldsuffdims[2] ne self.info.suffdims[2])||(n_elements(bnames) gt 0) then $
 self->updatedatainfo,names=bnames,type='BAND'
;Update names of sideplanes if provided
if (oldsuffdims[0] ne self.info.suffdims[0])||(n_elements(snames) gt 0) then $
 self->updatedatainfo,names=snames,type='SAMPLE'
;Update names of bottomplanes if provided
if (oldsuffdims[1] ne self.info.suffdims[1])||(n_elements(bonames) gt 0) then $
 self->updatedatainfo,names=bonames,type='LINE'
;Fix file parts location information, if necessary
self->updatelocations
;Make the new trimmed version of the header
*self.tlabels=strtrim(*self.labels,2)

end

pro pp_editablecube::updatedatainfo,names=names,type=type,core=core
compile_opt idl2,hidden
;Updates the given names of suffix planes or the band wavelengths (if core is set).
;Type specifies the type of suffix: 'BAND' for backplane, 'SAMPLE' for sideplane, 'LINE' for bottomplane.
;Updates are done in fields of self and in the labels

;Defaults
core=n_elements(core) eq 1 ? core : 0

if (core) then begin
  *self.wavs=(n_elements(wavs) eq self.info.coredims[2]) ? wavs : replicate ('UNKNOWN',self.info.coredims[2])
   pp_setcubeheadervalue,*self.labels,'BAND_BIN_CENTER',*self.wavs
endif else begin
  case type of
   'BAND' : begin & nsuf=self.info.suffdims[2] & ptr=self.bnames & end
   'SAMPLE' : begin & nsuf=self.info.suffdims[0] & ptr=self.snames & end
   'LINE' : begin & nsuf=self.info.suffdims[1] & ptr=self.bonames & end
  endcase 
  if ptr_valid(ptr) then begin
    *ptr=(n_elements(names) eq nsuf) ? names : 'UNKNOWN_'+strtrim(sindgen(nsuf),2)
    pp_setcubeheadervalue,*self.labels,type+'_SUFFIX_NAME',*ptr
  endif
endelse
end

pro pp_editablecube::updatesuffix,suffix=suffix,name,ind,dimschanged,typechanged
compile_opt idl2,hidden
;Updates the given data (suffix) in the suffix type selected by
;name: 'BAND' for backplanes, 'SAMPLE' for sideplanes, 'LINE' for bottomplanes.
;ind selects the corresponding dimension in the suffix (2 for backplanes,
;0 for sideplanes, 1 for bottomplanes.
;dimschanged and typechanged signal that either the dimensions or the type
;of this backplane were changed, so the corresponding fields and labels must
;be changed. 

;Constants
suffixlabels=['_SUFFIX_NAME','_SUFFIX_UNIT','_SUFFIX_ITEM_TYPE',$
 '_SUFFIX_ITEM_BYTES','_SUFFIX_BASE','_SUFFIX_MULTIPLIER',$
 '_SUFFIX_VALID_MINIMUM','_SUFFIX_NULL','_SUFFIX_LOW_REPR_SAT',$
 '_SUFFIX_LOW_INSTR_SAT','_SUFFIX_HIGH_INSTR_SAT','_SUFFIX_HIGH_REPR_SAT']
case name of
  'BAND' : begin & ptrs=[self.bnames,self.backplanes,self.bunits] & dim=2 & end
  'SAMPLE': begin & ptrs=[self.snames,self.sideplanes,self.sunits] & dim=0 & end
  'LINE' : begin & ptrs=[self.bonames,self.bottomplanes,self.bounits] & dim=1 & end
endcase

szs=size(suffix,/l64)
if (szs[0] eq 3) then begin
;Replace the suffix data
  tmp=make_array(szs[1:3],type=self.info.datatype)
  tmp[*]=suffix
  if ptr_valid(ptrs[1]) then *(ptrs[1])=temporary(tmp) else begin ;If suffix plane is being created, its pointers are null
    ptrs[1]=ptr_new(tmp,/no_copy)
    tmp=replicate('UNKNOWN',szs[ind+1])
    ptrs[0]=ptr_new(tmp)
    ptrs[2]=ptr_new(tmp,/no_copy)
    case name of
      'BAND' : begin & self.bnames=ptrs[0] & self.backplanes=ptrs[1] & self.bunits=ptrs[2] & end 
      'SAMPLE': begin & self.snames=ptrs[0] & self.sideplanes=ptrs[1] & self.sunits=ptrs[2] & end
      'LINE' : begin & self.bonames=ptrs[0] & self.bottomplanes=ptrs[1] & self.bounits=ptrs[2] & end
    endcase  
  endelse
;Replace type information, if necessary 
  if (typechanged || (szs[ind+1] ne self.info.suffdims[ind])) then begin
    pp_setcubeheadervalue,*self.labels,name+'_SUFFIX_ITEM_BYTES',replicate(self.info.bytes,szs[ind+1])
    pp_setcubeheadervalue,*self.labels,name+'_SUFFIX_ITEM_TYPE',replicate(self.info.type,szs[ind+1])
    pp_setcubeheadervalue,*self.labels,name+'_SUFFIX_BASE',replicate(0.0,szs[ind+1])
    pp_setcubeheadervalue,*self.labels,name+'_SUFFIX_MULTIPLIER',replicate(1.0,szs[ind+1])
;Fix the special values information, if necessary
    special=self->getspecialvalues(default=typechanged)
    pp_setcubeheadervalue,*self.labels,name+'_SUFFIX_VALID_MINIMUM',$
     replicate((self.info.bytes lt 4 ? special.valid_min : string(special.valid_min,format='("16#",Z08,"#")')),szs[ind+1])
    pp_setcubeheadervalue,*self.labels,name+'_SUFFIX_NULL',$
     replicate((self.info.bytes lt 4 ? special.null : string(special.null,format='("16#",Z08,"#")')),szs[ind+1])
    pp_setcubeheadervalue,*self.labels,name+'_SUFFIX_LOW_REPR_SAT',$
     replicate((self.info.bytes lt 4 ? special.low_repr_sat : string(special.low_repr_sat,format='("16#",Z08,"#")')),szs[ind+1])
    pp_setcubeheadervalue,*self.labels,name+'_SUFFIX_LOW_INSTR_SAT',$
     replicate((self.info.bytes lt 4 ? special.low_instr_sat : string(special.low_instr_sat,format='("16#",Z08,"#")')),szs[ind+1])
    pp_setcubeheadervalue,*self.labels,name+'_SUFFIX_HIGH_INSTR_SAT',$
     replicate((self.info.bytes lt 4 ? special.high_instr_sat : string(special.high_instr_sat,format='("16#",Z08,"#")')),szs[ind+1])
    pp_setcubeheadervalue,*self.labels,name+'_SUFFIX_HIGH_REPR_SAT',$
     replicate((self.info.bytes lt 4 ? special.high_repr_sat : string(special.high_repr_sat,format='("16#",Z08,"#")')),szs[ind+1])
  endif
  self.info.suffdims[ind]=szs[ind+1]
endif else begin ;Get rid of information for that suffix
  for i=0,n_elements(suffixlabels)-1 do pp_setcubeheadervalue,*self.labels,$
   name+suffixlabels[i]
  ptr_free,ptrs[0:1]
  self.info.suffdims[ind]=0L
endelse
end

pro pp_editablecube::updatecore,core,dimschanged,typechanged
compile_opt idl2,hidden
;Updates the given core array into the self fields and the labels.
;dimschanged and typechanged signal that either the dimensions or the type
;of this backplane were changed, so the corresponding fields and labels must
;be changed.

;Replace the core array
*self.core=core
szc=size(core,/l64)
newdims=szc[1:3]
if ~array_equal(newdims,self.info.coredims) then begin ;Must change a bunch of things if core dimensions changed
  dimschanged=newdims ne self.info.coredims ;Mark which dimensions have changed
;Replace the dimension information
  self.info.coredims=newdims
  self.info.dims=self.info.coredims+self.info.suffdims
  pp_setcubeheadervalue,*self.labels,'CORE_ITEMS','('+string(newdims,format='(3(I0,:,","))')+')'    
endif
;Replace the type information, if necessary
typechanged=szc[4] ne self.info.datatype
if typechanged then begin
  self.info.type=self.info.littleendian ? 'PC_' : 'SUN_'
  oldbytes=self.info.bytes
  case szc[4] of
    1 : begin & self.info.type+='INTEGER' & self.info.bytes=1 & end
    2 : begin & self.info.type+='INTEGER' & self.info.bytes=2 & end
    3 : begin & self.info.type+='INTEGER' & self.info.bytes=3 & end
    4 : begin & self.into.type+='REAL' & self.info.bytes=4 & end
    5 : begin & self.info.type+='REAL' & self.info.bytes=8 & end
    else : begin & self.into.type+='INTEGER' & self.info.bytes=8 & end
  endcase
  self.info.datatype=szc[4]
;Fix the label type information
  pp_setcubeheadervalue,*self.labels,'CORE_ITEM_TYPE',self.info.type
  pp_setcubeheadervalue,*self.labels,'CORE_ITEM_BYTES',self.info.bytes
  pp_setcubeheadervalue,*self.labels,'SUFFIX_BYTES',self.info.bytes
;Fix the special values information
  special=self->getspecialvalues(/default)
;Fix the special values information, if necessary
  special=self->getspecialvalues(default=typechanged)
  pp_setcubeheadervalue,*self.labels,'CORE_VALID_MINIMUM',$
   replicate(self.info.bytes lt 4 ? special.valid_min : string(special.valid_min,format='("16#",Z08,"#")'),szc[3])
  pp_setcubeheadervalue,*self.labels,'CORE_NULL',$
   replicate(self.info.bytes lt 4 ? special.null : string(special.null,format='("16#",Z08,"#")'),szc[3])
  pp_setcubeheadervalue,*self.labels,'CORE_LOW_REPR_SAT',$
   replicate(self.info.bytes lt 4 ? special.low_repr_sat : string(special.low_repr_sat,format='("16#",Z08,"#")'),szc[3])
  pp_setcubeheadervalue,*self.labels,'CORE_LOW_INSTR_SAT',$
   replicate(self.info.bytes lt 4 ? special.low_instr_sat : string(special.low_instr_sat,format='("16#",Z08,"#")'),szc[3])
  pp_setcubeheadervalue,*self.labels,'CORE_HIGH_INSTR_SAT',$
   replicate(self.info.bytes lt 4 ? special.high_instr_sat : string(special.high_instr_sat,format='("16#",Z08,"#")'),szc[3])
  pp_setcubeheadervalue,*self.labels,'CORE_HIGH_REPR_SAT',$
   replicate(self.info.bytes lt 4 ? special.high_repr_sat : string(special.high_repr_sat,format='("16#",Z08,"#")'),szc[3])
endif
end

pro pp_editablecube::updatelocations
compile_opt idl2,hidden
;Verifies if the length and locations of the label, history and binary
;parts have changed, and if so, updates the necessary information in the self fields
;and in the labels.

;Determine if label part has changed in length
self.llength=strlen(strjoin(*self.labels))+2L*n_elements(*self.labels)
changelab=self.llength ne self.oldllength

;Determine if history part has changed in length
self.hlength=strlen(strjoin(*self.history))+2L*n_elements(*self.history)
changehist=self.hlength ne self.oldhlength

;Determine if binary part has changed in length
self.binlength=n_elements(*self.raw)*self.info.bytes
changebin=self.oldbinlength ne self.binlength

;Update location fields
self.info.labelrecords>=(1+ceil(double(self.llength)/self.info.recordbytes,/l64))
self.info.historyrecords>=ceil(double(self.hlength)/self.info.recordbytes,/l64)
self.info.binaryrecords=ceil(double(self.binlength)/self.info.recordbytes,/l64)
self.info.historystart=self.info.labelrecords
self.info.binarystart=self.info.historystart+self.info.labelrecords
self.info.filerecords=self.info.labelrecords+self.info.historyrecords+self.info.binaryrecords

;Replace the labels
pp_setcubeheadervalue,*self.labels,'FILE_RECORDS',self.info.filerecords
pp_setcubeheadervalue,*self.labels,'LABEL_RECORDS',self.info.labelrecords
pp_setcubeheadervalue,*self.labels,'\^HISTORY',self.info.historystart+1
pp_setcubeheadervalue,*self.labels,'\^QUBE',self.info.binarystart+1

;Update label length after the location labels were written
self.llength=strlen(strjoin(*self.labels))+2L*n_elements(*self.labels)
;Replace the current lengths
self.oldllength=self.llength
self.oldhlength=self.hlength
self.oldbinlength=self.binlength

end

;+
; :Description:
;    Changes values contained in the label or history part of the cube header.
;    Can set values of exisiting keys, add new keys, remove exisitng keys, or
;    just append lines to the header.
;
; :Keywords:
;    key : in, optional
;      Key name to be edited.
;    value : in, optional
;      Value to be assigned to the named key. If absent, the key is removed.
;    append : in, optional
;      Passed to pp_setcubeheadervalue as is.
;      A string scalar or array of lines to be inserted into the header. If provided, key and value are
;      ignored. No processing is done on the given lines, it is the user's responsability to ensure they
;      are valid.

;    history : in, optional, default=0
;      If set, editing is done to the history part of the header, instead of the label part.
;
; :Examples:
;    See the example on pp_editablecube__define.
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Oct/2009
;-
pro pp_editablecube::headeredit,key=key,value=value,append=append,history=hist
compile_opt idl2

;Defaults
hist=n_elements(hist) eq 1 ? hist : 0

tmp=hist ? *self.history : *self.labels
if (n_elements(append) gt 0) then pp_setcubeheadervalue,tmp,append=append else begin
  if (n_elements(value) gt 0) then pp_setcubeheadervalue,tmp,key,value else pp_setcubeheadervalue,tmp,key
endelse
if (hist) then *self.history=tmp else *self.labels=tmp

end

;+
; :Description:
;    Writes the cube in the object to a cube file.
;
; :Params:
;    filename, in, optional
;      The name of the file to which the cube will be written. Optional only
;      if the object already contains a name in the newfile field from a previous
;      call of write.
; :Keywords:
;    format : in, optional, default='fits'
;      File format to make. Valid options are 'fits' or 'csv'.
;
; :Examples:
;    See the example on pp_editablecube__define.
; 
; :Author: Paulo Penteado (pp.penteado@gmail.com), Oct/2009
;-
pro pp_editablecube::write,filename
compile_opt idl2

filename=n_elements(filename) eq 1 ? filename : self.newfile
self.newfile=filename
if (self.newfile eq '') then message,'Filename required to write the cube'
print,'Writing cube to file ',filename
openw,unit,self.newfile,/get_lun
;Write the labels
writeu,unit,strjoin((*self.labels)+string(13B)+string(10B))
;Pad to the number of label records
;if (self.llength lt self.info.labelrecords*self.info.recordsize) then $
; writeu,unit,replicate(string(10B),self.info.labelrecords*self.info.recordsize-self.llength)
point_lun,unit,self.info.recordbytes*self.info.historystart
;Write the history
writeu,unit,strjoin((*self.history)+string(13B)+string(10B))
;Pad to the number of history records
point_lun,unit,self.info.recordbytes*self.info.binarystart
;Write the data
writeu,unit,*self.raw
;Pad to the number of binary records
point_lun,unit,self.info.recordbytes*self.info.filerecords
free_lun,unit 

end

;+
; :Description:
;    Exports the cube in the object to a fits or csv file.
;
; :Params:
;    file, in, required
;      The name of the file to which the cube will be written.
;
; :Examples:
;    See the example on pp_editablecube__define.
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Oct/2009
;-
pro pp_editablecube::export,file,format=format
compile_opt idl2,logical_predicate

format=n_elements(format) ? format : 'fits'

c=self

templ={}
if c.bands then begin
  bandnames=strtrim(sindgen(c.bands),2)
  ml=max(strlen(bandnames))
  bandnames='BAND_'+string(sindgen(c.bands),format='(I0'+strtrim(ml,2)+')')
  foreach bn,bandnames do templ=create_struct(templ,bn,0d0)
endif else templ={}

foreach bn,c.backnames do templ=create_struct(templ,idl_validname(bn,/convert_all),0d0)

cubestruct=replicate(templ,c.lines*c.samples)
if c.bands then begin
  core=reform(c.core,c.samples*c.lines,c.bands)
  for i=0,c.bands-1 do cubestruct.(i)=core[*,i]
endif
if c.nback then begin
  backplanes=reform(c.backplanes,c.lines*c.samples,c.nback)
  for i=0,c.nback-1 do cubestruct.(i+c.bands)=backplanes[*,i]
endif

if strlowcase(format) eq 'fits' then mwrfits,cubestruct,file,/create
if strlowcase(format) eq 'csv' then write_csv_pp,file,cubestruct,/titles

end

pro pp_editablecube::cleanup
compile_opt idl2,hidden
ptr_free,self.oldlabels,self.oldhistory
self->pp_readcube::cleanup
end


;+
; :Description:
;    Object to read, edit and write an ISIS cube. Read functionality is the same
;    as that of the pp_readcube class (it is inherited from it, see its documentation
;    for details and examples on reading). Once the object is instantiated, either
;    from a file or from another editable cube, its data and metadata can be
;    changed, and it can be written to a file.
;    
;    Initialization parses the cube into the object, other methods retrieve parts of it,
;    edit it, or write it to a file.
;    
;    On reading, makes the same assumptions (inherits them) from the pp_readcube class:
;    All suffix items are the same data type as core items, cube has 3 axes in BSQ order,
;    and records have fixed length. On writing, these assumptions are maintained.
;    
;    The only methods intended to be public are: getproperty, getspecialvalues,
;    getfromheader, getsuffixbyname, getbandbywavelength, setproperty, headeredit, and write.
;    
;    See the documentation of the method setproperty for the most useful edit and
;    write example, or getproperty for the most useful read example.
;
; :Examples:
;    For read use, see the example on pp_readcube__define.
;    
;    To initialize from the cube CM_1553510065_1_ir.cub::
; 
;      a=obj_new('pp_editablecube',file='CM_1553510065_1_ir.cub')
;      
;    To add a dummy backplane::
;    
;      a->getproperty,backplanes=back,backnames=bnames,lines=lines,samples=samples
;      backplanes=[[[backplanes]],[[findgen(lines,samples)]]]
;      backnames=[backnames,'DUMMY']
;      a->setproperty,backplanes=back,backnames=bnames      
;    
;    To remove the first core band::
;    
;      a->getproperty,core=core,wavelengths=wavs
;      core=core[*,*,1:*] & wavs=wavs[1:*]
;      a->setproperty,core=core,wavelengths=wavs
;      
;    To add lines to the history part of the header::
;    
;      app=['GROUP = testedit','date = '+strcompress(systime(),/remove),'END_GROUP = testdate']  
;      a->headerset,append=app
;      
;    To write the edited cube to 'testedit.cub'::
;    
;      a->write,'testedit.cub'
;
;    To write the edited cube to 'testedit.fits'::
;
;      a->export,'testedit.cub'
;      
;    Destroy the object when done with it::
;    
;      obj_destroy,a
;
; :Uses: pp_getcubeheadervalue, pp_extractfields, pp_setcubeheadervalue 
; 
; :Uses: pp_readcube
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Oct/2009
;-
pro pp_editablecube__define
 void={pp_editablecube,inherits pp_readcube,newfile:'',$
  oldlabels:ptr_new(),oldhistory:ptr_new(),$
  llength:0L,oldllength:0L,hlength:0L,oldhlength:0L,$
  binlength:0LL,oldbinlength:0LL,preservespecial:0}
end
