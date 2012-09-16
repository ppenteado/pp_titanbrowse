; docformat = 'rst'
;+
; :Description:
;    Sets the value of the given key from the given header in ISIS cube format. If value is
;    not provided, the key is erased from the header. If append is given, its contents are just
;    inserted into the header, without affecting the rest of its contents (and value and key are ignored).
;
; :Params:
;    header : in, required
;      A string array where each element is one line of an ISIS cube.
;    key : in, optional
;      A string scalar with the key to be set. Regular expression metacharacters must be escaped.
;    value : in, optional
;      A scalar of vector of any type (it gets converted to string) with the value(s) to set
;      the key to. If absent and a key is given, that key is erased (if found) from the header.
;
; :Keywords:
;    append : in, optional
;      A string scalar or array of lines to be inserted into the header. If provided, key and value are
;      ignored. No processing is done on the given lines, it is the user's responsability to ensure they
;      are valid.
;
; :Examples:
; 
;    Make a simple example header::
;    
;      head=strarr(5)      
;      head[0]='CCSD3ZF0000100000001NJPL3IF0PDS200000001 = SFDU_LABEL'
;      head[1]='BAND_SUFFIX_NAME = (LATITUDE,LONGITUDE,SAMPLE_RESOLUTION,LINE_RESOLUTION,'
;      head[2]='PHASE_ANGLE,INCIDENCE_ANGLE,EMISSION_ANGLE,NORTH_AZIMUTH)'
;      head[3]='START_TIME = "2007-084T10:00:57.286Z"'
;      head[4]='END'
;      
;    Change the START_TIME::
;    
;      pp_setcubeheadervalue,head,'START_TIME','UNKNOWN'
;      print,head,format='(A0)'
;      ;CCSD3ZF0000100000001NJPL3IF0PDS200000001 = SFDU_LABEL
;      ;BAND_SUFFIX_NAME = (LATITUDE,LONGITUDE,SAMPLE_RESOLUTION,LINE_RESOLUTION,
;      ;PHASE_ANGLE,INCIDENCE_ANGLE,EMISSION_ANGLE,NORTH_AZIMUTH)
;      ;START_TIME = UNKNOWN
;      ;END
;
; :Uses: pp_getcubeheadervalue, pp_extractfields
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Oct/2009
;-
pro pp_setcubeheadervalue,header,key,value,append=append
compile_opt idl2

nvals=n_elements(value)
nhead=n_elements(header)

;Constants
metalist=['.','[',']','\','(',')','*','+','?','?','{','}','|','^','$']
if (n_elements(append) gt 0) then begin ;If just adding some lines to the header
  header=[header[0:nhead-2],append,header[nhead-1]]
endif else begin
;Replace regex metacharacters in key for writing
  keyn=key
  for i=0,n_elements(metalist)-1 do begin
    tmp=strsplit(keyn,'\\\'+metalist[i],/regex,/extract,count=count,/preserve_null)
    if (count gt 1) then keyn=strjoin(tmp,metalist[i])
  endfor
;Find if the key already exists
  val=pp_getcubeheadervalue(header,key,/not_trimmed,lines=lines,count=count)
  nlines=n_elements(lines)
  if (nvals eq 0) then begin ;If erasing a key
    if (count gt 0) then begin ;Nothing needs to be done if key is not in the header
      header=[header[0:lines[0]-1],header[lines[nlines-1]+1:*]] ;lines never contains 0 or nhead-1, so no problem here
    endif
  endif else begin ;If setting a key
;Convert to string if necessary
    tvalue=size(value,/type)
    case tvalue of
      7 : svalue=value
      1 : svalue=strtrim(string(fix(value)),2)
      else : svalue=strtrim(string(value),2)
    endcase
    if (nvals eq 1) then newlines=keyn+' = '+svalue else begin ;If scalar
      mxl=max(strlen(svalue));Longest string
      jvalue=strjoin(svalue,',')
      totall=strlen(jvalue)
;Write the lines with the values, splitting vectors if lines get too long
      if (totall le 60) then newlines=keyn+' = ('+jvalue+')' else begin
;Multiple line write      
        newlines=keyn+' = ('
        j=0
        for i=0,nvals-1 do begin
          newlines[j]+=svalue[i]
          if (i lt nvals-1) then begin
            newlines[j]+=','
            if strlen(newlines[j]) gt 80 then begin
              j=j+1
              newlines=[newlines,'']
            endif
          endif else newlines[j]+=')'
        endfor
      endelse
    endelse
    header=count gt 0 ? [header[0:lines[0]-1],newlines,header[lines[nlines-1]+1:*]] : [header[0:nhead-2],newlines,header[nhead-1]]
  endelse
endelse

end
