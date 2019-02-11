; docformat = 'rst'
;+
;
; :Uses: pp_getcubeheadervalue, pp_extractfields, pp_buffered_vector
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Oct/2009
;-

;+
; :Description:
;    Provided with the name of the file, initializes the object reading the cube in it. 
;
; :Params:
;    file : in, required
;      The name of the file that contains the cube.
;
; :Keywords:
;    special : in, optional, default=0
;      Determines the type of special value replacement to use:
;      
;      0 uses the default special values
;      
;      1 uses the special values found in the header
;      
;      2 disables special value replacement
;
; :Examples:
;    See the example on pp_nhcube__define.
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Oct/2009
;-
function pp_airscube::init,file=file,special=special,_ref_extra=ex,sequencetitle=sequencetitle,sequenceid=sequenceid
compile_opt idl2,hidden
ret=0
if (n_elements(file) ne 1) then begin
  print,'pp_airscube: File name not provided'
  return,ret
endif else if (file eq '') then return,ret ;Silently get out to allow the trick in pp_cubecollection::init

root=stregex(file,'(AIRS.((([[:digit:]]{4})\.([[:digit:]]{2})\.([[:digit:]]{2}))\.([[:digit:]]{3})))\..*hdf',/extract,/subexpr)
sequencetitle=n_elements(sequencetitle) ? sequencetitle : root[3]
sequenceid=n_elements(sequenceid) ? sequenceid : root[4]+'.'+string(julday(root[5],root[6],root[4])-julday(1,0,root[4]),format='(I03)')



catch,error_status
if (error_status ne 0) then begin
  catch,/cancel
  print,'pp_airscube: File named "',file,'" could not be read or parsed'
endif else begin
  self.file=file
  
  


  tmp=pp_airshdftocube(file,_strict_extra=ex)
  self.labels=ptr_new(tmp.header)
  tmp.header['SEQUENCE_ID']=sequenceid
  tmp.header['SEQUENCE_TITLE']=sequencetitle
  tmp.header['PRODUCT_ID']=root[2]
  tlabels=string(tmp.header,/implied_print)
  self.tlabels=ptr_new(tlabels)
  self.history=ptr_new(tmp.header)
  self.core=ptr_new(tmp.core)
  self.backplanes=ptr_new(tmp.backplanes)
  self.raw=ptr_new(tmp)
  ret=self.parselabels()
  catch,/cancel

  
endelse

return,ret
end

function pp_airscube::parselabels
  ;Gets the minimum required information from the header to be able to
  ;read the data in the cube.
  compile_opt idl2,hidden


  ;;Get the core and suffix dimensions
  ;theader=*self.tlabels
  count=1
  tmp=*self.raw
  sz=size(tmp.core,/dimensions)
  self.info.coredims=long(sz)
  bsz=size(tmp.backplanes,/dimensions)
  self.info.suffdims=long(bsz)
  self.info.dims=self.info.coredims+self.info.suffdims
  ;;Determine the data type
  ;self.info.bytes=fix(pp_getcubeheadervalue(theader,'CORE_ITEM_BYTES',count=tmp)) & count*=tmp
  ;self.info.type=pp_getcubeheadervalue(theader,'CORE_ITEM_TYPE',count=tmp) & count*=tmp
  ;if (strpos(self.info.type,'REAL') ne -1) then begin
  ;  case self.info.bytes of
  ;    8 : self.info.datatype=5 ;double
  ;    else : self.info.datatype=4 ;float
  ;  endcase
  ;endif else begin
  ;  case self.info.bytes of
  ;    1 : self.info.datatype=1 ;byte
  ;    2 : self.info.datatype=2 ;int
  ;    8 : self.info.datatype=14 ;long64
  ;    else : self.info.datatype=3 ;long
  ;  endcase
  ;endelse
  ;;Determine endianness
  ;self.info.littleendian=(strpos(self.info.type,'PC') ne -1)
  ;;Determine the length and location of the labels, history and data parts
  ;self.info.recordbytes=long(pp_getcubeheadervalue(theader,'RECORD_BYTES',count=tmp)) & count*=tmp
  ;self.info.historystart=long(pp_getcubeheadervalue(theader,'\^HISTORY',count=tmp))-1L & count*=tmp
  ;self.info.binarystart=long(pp_getcubeheadervalue(theader,'\^QUBE',count=tmp))-1L & count*=tmp
  ;self.info.filerecords=long(pp_getcubeheadervalue(theader,'FILE_RECORDS',count=tmp))-1L & count*=tmp
  ;self.info.labelrecords=long(pp_getcubeheadervalue(theader,'LABEL_RECORDS',count=tmp))-1L & count*=tmp
  ;self.info.historyrecords=self.info.filerecords-self.info.labelrecords
  ;self.info.binaryrecords=self.info.filerecords-self.info.binarystart-1
  ;
  ;;Get the core wavelengths
  ;wavs=double(pp_getcubeheadervalue(theader,'BAND_BIN_CENTER',count=tmp))
  ;wavs=tmp gt 0 ? wavs : replicate('UNKNOWN',self.info.coredims[2])
  ;self.wavs=ptr_new(wavs,/no_copy)
  self.wavs=ptr_new(tmp.wavs)
  ;;Get the core units
  ;units=pp_getcubeheadervalue(theader,'BAND_BIN_UNIT',count=tmp)
  ;self.units=tmp gt 0 ? units : ''
  self.units='micron';radiances are 'mWatt/m2/cm-1/sr'
  ;;Get the backplane names
  self.bnames=ptr_new(tmp.backnames)
  self.bunits=ptr_new(tmp.bunits)
  ;if (self.info.suffdims[2] gt 0) then begin
  ;  bnames=pp_getcubeheadervalue(theader,'BAND_SUFFIX_NAME',count=tmp)
  ;  bnames=tmp gt 0 ? bnames : 'UNKNOWN_'+strtrim(sindgen(self.info.suffdims[2]),2)
  ;  self.bnames=ptr_new(bnames,/no_copy)
  ;;Get the backplane unit names
  ;  bunits=pp_getcubeheadervalue(theader,'BAND_SUFFIX_UNIT',count=tmp)
  ;  bunits=tmp gt 0 ? bunits : replicate('UNKNOWN',self.info.suffdims[2])
  ;  self.bunits=ptr_new(bunits,/no_copy)
  ;endif
  ;;Get the sideplane names
  ;if (self.info.suffdims[0] gt 0) then begin
  ;  snames=pp_getcubeheadervalue(theader,'SAMPLE_SUFFIX_NAME',count=tmp)
  ;  snames=tmp gt 0 ? snames : 'UNKNOWN_'+strtrim(sindgen(self.info.suffdims[0]),2)
  ;  self.snames=ptr_new(snames,/no_copy)
  ;;Get the sideplane unit names
  ;  sunits=pp_getcubeheadervalue(theader,'SAMPLE_SUFFIX_UNIT',count=tmp)
  ;  sunits=tmp gt 0 ? sunits : replicate('UNKNOWN',self.info.suffdims[0])
  ;  self.sunits=ptr_new(sunits,/no_copy)
  ;endif
  ;;Get the bottomplane names
  ;if (self.info.suffdims[1] gt 0) then begin
  ;  bonames=pp_getcubeheadervalue(theader,'LINE_SUFFIX_NAME',count=tmp)
  ;  bonames=tmp gt 0 ? bonames : 'UNKNOWN_'+strtrim(sindgen(self.info.suffdims[1]),2)
  ;  self.bonames=ptr_new(bonames,/no_copy)
  ;;Get the bottomplane unit names
  ;  bounits=pp_getcubeheadervalue(theader,'LINE_SUFFIX_UNIT',count=tmp)
  ;  bounits=tmp gt 0 ? bounits : replicate('UNKNOWN',self.info.suffdims[1])
  ;  self.bounits=ptr_new(bounits,/no_copy)
  ;endif


  tmp.header['SAMPLING_MODE_ID']='NORMAL'
  tmp.header['SAMPLING_MODE_ID']='NORMAL'
  ;tmp.header['PRODUCT_ID']=file_basename(self.file)
  time0='1993-01-01T00:00:00'
  cspice_furnsh,tmp.header['tmfile']
  cspice_str2et,time0,et0
  nadirtai=self.getfromheader('nadirTAI')
  tmp.header['EXPOSURE']=(nadirtai[-1]-nadirtai[0])/(product(sz))
  tmp.header['NATIVE_START_TIME']=nadirtai[0]
  tmp.header['START_TIME']=nadirtai[0]+et0
  tmp.header['STOP_TIME']=nadirtai[-1]+et0

  tlabels=string(tmp.header,/implied_print)
  self.tlabels=ptr_new(tlabels)
  cspice_kclear
  return,(count ne 0) ;A return value of 0 indicates failure to read all the required information
end

;+
; :Description:
;    Retrieves values contained in the label or history part of the cube header.
;    Just a wrapper for pp_getcubeheadervalue. See its documentation for details.
;
; :Params:
;    key : in
;      Key name to be retrieved.
;
; :Keywords:
;    history : in, optional, default=0
;      If set, reading is done on the history part of the header, instead of the label part.
;    count : out, optional
;      Passed to pp_getcubeheadervalue.
;      The number of occurences of the key found in the header. If more than 1
;      is found, the last occurence is used. Check this value to determine if
;      the key was not found (count will be 0 in that case).
;    fold_case : in, optional
;      Passed to pp_getcubeheadervalue.
;      Passed to stregex when searching for the key. If set, capitalization of
;      the key is ignored.
;    lines : out, optional
;      Passed to pp_getcubeheadervalue.
;      The line index (starting at zero) of the line in the header that provided
;      the retrieved value. If valued spanned more than one line, this is a vector
;      with the indexes of all such lines. If key not found, -1 is returned.
;    unquote : in, optional
;      Passed to pp_getcubeheadervalue.
;      If set, enclosing quotes are removed from the return values
;    sel : in, optional
;      Passed to pp_getcubeheadervalue.
;      In case more than one ocurrence of a keyword is found, sel gives the
;      index of the ocurrence to use (starts at 0). If not set, the last ocurrence
;      is the one used.

;
; :Examples:
;    See the examples on pp_cube__define and pp_getcubeheadervalue.
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Oct/2009
;-
function pp_airscube::getfromheader,key,history=hist,$
  count=count,fold_case=fold_case,lines=lines,unquote=unquote,sel=sel
  compile_opt idl2,logical_predicate

  ;Defaults
  hist=n_elements(hist) eq 1 ? hist : 0
  tmp=*self.labels
  if tmp.haskey(key) then begin
    ret=tmp[key]
    count=1
  endif else begin
    ret='0'
    count=0
  endelse
  if keyword_set(unquote) then begin
    s1=strpos(ret,"'")
    s2=strpos(ret,"'",/reverse_search)
    w=where((s1 eq 0) and (s2 eq strlen(ret)-1),c)
    if c then ret[w]=strmid(ret[w],1,strlen(ret[w])-2)
  endif
  return,ret
end

pro pp_airscube::getproperty,_ref_extra=ex,$
  year=year,doy=doy,month=month,day=day,id0=id0,id1=id1,instrument=instrument,jday=jday
compile_opt idl2,logical_predicate
if arg_present(year) then year=(*self.labels)['year']
if arg_present(month) then month=(*self.labels)['month']
if arg_present(day) then day=(*self.labels)['day']
if arg_present(doy) then doy=(*self.labels)['doy']
if arg_present(hour) then hour=(*self.labels)['hour']
if arg_present(minu) then minu=(*self.labels)['minu']
if arg_present(sec) then sec=(*self.labels)['sec']
if arg_present(id0) then id0=(*self.labels)['id0']
if arg_present(id1) then id1=(*self.labels)['id1']
if arg_present(instrument) then instrument=(*self.labels)['instrument']
if arg_present(jday) then jday=(*self.labels)['jday']
if n_elements(ex) then self.pp_editablecube::getproperty,_strict_extra=ex
end


;+
; 
; :Description:
;    Object to read an ISIS cube.
;    
;    Initialization parses the cube into the object, other methods retrieve parts of it.
;    
;    Assumes that all suffix items are the same data type as core items.
;    
;    Assumes that cube has 3 axes in BSQ order.
;    
;    Assumes constant length records.
;    
;    The only methods intended to be public are getproperty,getspecialvalues, getfromheader,
;    getsuffixbyname, and getbandbywavelength.
;    
; :Examples:
;    
;    To read the cube CM_1553510065_1_ir.cub::
; 
;      a=obj_new('pp_nhcube','CM_1553510065_1_ir.cub')
;    
;    To get the core and its wavelengths::
;    
;      a->getproperty,core=core,wavelengths=wavs
;      print,min(wavs,max=mw),mw
;      ;0.88421000       5.1225000
;      
;    To get the backplanes and their names::
;    
;      a->getproperty,backplanes=back,backnames=bnames
;      print,bnames
;      ;LATITUDE LONGITUDE SAMPLE_RESOLUTION LINE_RESOLUTION PHASE_ANGLE INCIDENCE_ANGLE EMISSION_ANGLE NORTH_AZIMUTH
;      
;    To get the file name::
;    
;      print,a->getproperty(/file)
;      ;CM_1553510065_1_ir.cub
;      
;    To get all the properties at once::
;    
;     a->getproperty,all=a_all
;     ;** Structure pp_nhcube_ALL, 24 tags, length=98912, data length=98895:
;     ;   FILE            STRING    'CM_1553510065_1_ir.cub'
;     ;   SPECIAL         BYTE         0
;     ;   LABELS          STRING    Array[268]
;     ;   HISTORY         STRING    Array[479]
;     ;   CORE            FLOAT     Array[1, 40, 256]
;     ;   BACKPLANES      FLOAT     Array[1, 40, 8]
;     ;   SIDEPLANES      POINTER   <NullPointer>
;     ;   BOTTOMPLANES    POINTER   <NullPointer>
;     ;   INFO            STRUCT    -> pp_nhcube_INFO Array[1]
;     ;   LINES           LONG                40
;     ;   SAMPLES         LONG                 1
;     ;   BANDS           LONG               256
;     ;   NBACK           LONG                 8
;     ;   NSIDE           LONG                 0
;     ;   NBOTTOM         LONG                 0
;     ;   RAW             FLOAT     Array[1, 40, 264]
;     ;   WAVELENGTHS     DOUBLE    Array[256]
;     ;   BACKNAMES       STRING    Array[8]
;     ;   SIDENAMES       POINTER   <NullPointer>
;     ;   BOTTOMNAMES     POINTER   <NullPointer>
;     ;   UNITS           STRING    'MICROMETER'
;     ;   BACKUNITS       STRING    Array[8]
;     ;   SIDEUNITS       POINTER   <NullPointer>
;     ;   BOTTOMUNITS     POINTER   <NullPointer>
;     
;    To get the latitudes::
;    
;      lats=a->getsuffixbyname('LATITUDE')
;      
;    Or, equivalenty::
;    
;      lats=a['LATITUDE']
;      
;    To get the band with wavelength nearest to 2.1 (in the units used in the cube)::
;    
;      selband=a->getbandbywavelength(2.1,wavelengths=selwavs)      
;      print,selwavs
;      ;2.1003400
;      
;    Or, equivalently::
;    
;      selband=a[2.1d0]
;      selwavs=a[2.1]
;      print,selwavs
;      ;2.1003400
;      
;    To get the start time of the cube::
;    
;      print,a->getfromheader('START_TIME')
;      ;"2007-084T10:00:57.286Z"
;
;    Destroy the object when done with it::
;    
;      obj_destroy,a
;    
; :Uses: pp_getcubeheadervalue, pp_extractfields
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Oct/2009
;-
pro pp_airscube__define
compile_opt idl2
void={pp_airscube, inherits pp_editablecube}
return
end
