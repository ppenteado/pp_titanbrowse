; docformat = 'rst'


;+
; :Description:
;  Refit of cube_extract.pro, to read the cubes using pp_readcube objects instead of ISIS.
;  
;  Works similar to exttract3.f:
;  
;  Reads the given cube and makes outfile with geometrical information  
;  and a 2 column text file for each spectrum in the selection.
;  
;  If no selection specified, the whole cube is used.
;  
;  Selections are made with corners or list, described below.
;  
;  Pixels out of the disk (as determined by 'LATITUDE') are not included in the output, unless nosel is set.
;  
;  All pixel numbers given are vims numbers: [1,1] is the top left pixel.
;
;
; :Params:
;    cubefile : in, required
;      Passed to pp_readcube. The name of the file that contains the cube.
;    outfile : in, required
;      The name of the file where the geometrical information will be written to.
;      
;
; :Keywords:
;    corners : in, optional
;      A 4 element vector [xi,yi,xf,yf] specifying the region to select.
;    list : in, optional
;      A 2xn array with x,y pixel numbers to select.
;    nan : in, optional
;      Specifies what to use for pixels with special value. Defaults to !values.f_nan if oldfmt is not set,
;      'NUL' otherwise.
;    oldfmt : in, optional default=0
;      If set, forces outfile to be formatted as
;      
;               Filename      x      y      LATITUDE     LONGITUDE   PHASE_ANGLENCIDENCE_ANGLEEMISSION_ANGLE     DELTA_PHI
;               
;      And forces nan='NUL', for backwards compatibility with extract3.f.      
;    nosep : in, optional, default=0
;      If set, does not make the individual ASCII files for each spectrum.
;      
;    nosel : in,out optional, default=0
;      If set, returns all pixels in the selected region, not just those that fall on the surface.
;      If set, but the cube does not contain a 'LATITUDE' backplane, this is reset to 0.
;
;
; :Uses:pp_readcube__define, pp_getcubeheadervalue, pp_extractfields, pp_buffered_vector
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), May/2010 (refit to cube_extract from May/2008)
;-
pro pp_cube_extract,cubefile,outfile,corners=co,list=list,nan=nan,oldfmt=stdfmt,nosep=nosep,nosel=nosel
compile_opt idl2
;some defaults
if (n_elements(stdfmt) ne 1) then stdfmt=0
if (n_elements(nan) ne 1) then begin
  if (stdfmt) then nan='NUL' else nan=!values.f_nan
endif
if (n_elements(nosep) ne 1) then nosep=0
nosel=n_elements(nosel) eq 1 ? nosel : 0

;get the cube
;Refit to read with pp_readcube, instead of readall8/ISIS
;readall8,cubefile,data2=d2,wavs=wavs,bn=bn,/dphi,nval=nval,ncore=nc,nback=nb,nlines=nl,nsamples=ns,nosel=nosel
cube=obj_new('pp_readcube',cubefile)
cube->getproperty,core=core,wavelengths=wavs,backnames=backnames,bands=nc,lines=nl,samples=ns
if (~nosel) then begin ;select only the pixels that fall on the surface
  lats=cube->getsuffixbyname('LATITUDE')
  w=where((lats ge -9d1) and (lats le 9d1),nval)
endif else nval=nl*ns
if (nval eq 0) then begin
  print,'No pixels selected'
  return
endif
bn=['LATITUDE','LONGITUDE','SAMPLE_RESOLUTION','LINE_RESOLUTION','PHASE_ANGLE',$
 'INCIDENCE_ANGLE','EMISSION_ANGLE','NORTH_AZIMUTH','AZ_DIF_0'] ;Only include the old usual backplanes
nb=n_elements(bn)
;Retrieve the selected backplanes
backdata=cube->getsuffixbyname(bn)
obj_destroy,cube
backdata=reform(backdata,ns*nl,nb)
d2=fltarr(nval,nc+nb+2)
core=reform(core,nl*ns,nc,/overwrite)
d2[0,0]=nosel ? core : core[w,*]
d2[0,nc]=nosel ? backdata : backdata[w,*]
x=rebin(1+lindgen(ns),ns,nl) & x=reform(x,ns*nl,/over)
y=rebin(reform(1+lindgen(nl),1,nl),ns,nl) & y=reform(y,ns*nl,/over)
d2[0,nc+nb]=nosel ? x : x[w]
d2[0,nc+nb+1]=nosel ? y : y[w]
;End of the refit, the rest of the code should not notice the difference

;making the index list
if ((n_elements(co) ne 4)&&(n_elements(list) lt 2)) then li=lindgen(nval) else begin;by default, select everything
  if (n_elements(co) eq 4) then begin ;make list from corners
    list=lonarr(2,(co[2]-co[0]+1)*(co[3]-co[1]+1))
    for i=0,n_elements(list)/2-1 do list[*,i]=[i/(co[3]-co[1]+1),i mod (co[3]-co[1]+1)]+co[0:1]
  endif
;make index list from pixel list
  for i=0,n_elements(list)/2-1 do begin
    w=where((d2[*,nc+nb] eq list[0,i])*(d2[*,nc+nb+1] eq list[1,i]),nw);finds if that pixel is valid
    if (nw) then begin
      if (n_elements(li) eq 0) then li=w else li=[li,w]
    endif
  endfor  
endelse
if (n_elements(li) eq 0) then begin;exit if no pixels left
  print,'no pixels in selection'
  return
endif

;make outfile
;header and formats
bn=strcompress(bn,/rem)
if (stdfmt) then begin;if using old format, for backward compatibility 
  header=['               Filename','      x','      y','      LATITUDE','     LONGITUDE','   PHASE_ANGLE','NCIDENCE_ANGLE','EMISSION_ANGLE','     DELTA_PHI']
  nh=n_elements(header)
  vcols=intarr(nh-1)
  vcols[0:1]=[nb,nb+1]
  w=where(strupcase(bn) eq 'DELTA_AZIMUTH')
  if (w[0] ne -1) then bn[w]='DELTA_PHI'
  tn=strupcase(strmid(bn,13,14,/rev))
  for i=2,nh-2 do vcols[i]=where(tn eq strcompress(header[i+1],/rem))
  fmts=strarr(4)
  fmts[0]='(A23)'
  fmts[1]='(5X,I2.2)'
  fmts[2]='(F14.6)'
  fmts[3]='(A14)'
endif else begin;new format (this can be changed)
  nh=nb+3
  vcols=intarr(nh-1)
  vcols[0:1]=[nb,nb+1]
  vcols[2:nh-2]=indgen(nb)
  header=strarr(nh)
  header[0]=string('Filename',format='(A23)')
  header[1]=string('x',format='(A3)')
  header[2]=string('y',format='(A3)')
  header[3:3+nb-1]=string(bn,format='(A20)')
  fmts=strarr(4)
  fmts[0]='(A23)'
  fmts[1]='(2(1X,I2.2))'
  fmts[2]='(F20.6)'
  fmts[3]='(A20)';this should be the same width as fmts[2]
endelse
openw,unit,outfile,/get_lun
printf,unit,strjoin(header)
core=strmid(cubefile,0,13)
df=strarr(n_elements(li))
for k=0,n_elements(li)-1 do begin
  i=li[k]
  df[k]=core+'_'+string(d2[i,nc+nb],format='(I2.2)')+'_'+string(d2[i,nc+nb+1],format='(I2.2)')+'.dat'
  tmp=string(df[k],format=fmts[0])+strjoin(string(d2[i,nc+nb:nc+nb+1],format=fmts[1]))
  for j=2,n_elements(vcols)-1 do begin;put nan if invalid
    if ((vcols[j] ne -1)&&(finite(d2[i,nc+vcols[j]]))) then rtmp=d2[i,nc+vcols[j]] else rtmp=nan
    if (size(rtmp,/type) ne 7) then tmp+=string(rtmp,format=fmts[2]) else tmp+=string(rtmp,format=fmts[3]) 
  endfor
  printf,unit,tmp
endfor
free_lun,unit
;make the separate files
if (not nosep) then for k=0,n_elements(li)-1 do begin
  i=li[k]
  openw,unit,df[k],/get_lun
  for j=0,n_elements(wavs)-1 do begin
    if (finite(d2[i,j])) then rtmp=d2[i,j] else rtmp=nan;put nan if invalid
    if (size(rtmp,/type) eq 7) then stmp=string(rtmp,format='(A17)') else stmp=string(rtmp,format='(5X,F12.9)') 
    printf,unit,wavs[j],stmp,format='(5X,F12.9,A)'
  endfor
  free_lun,unit
endfor

end
