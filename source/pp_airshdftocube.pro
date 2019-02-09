function pp_airshdftocube_expandarray,x
compile_opt idl2,logical_predicate
sz=size(x,/dimensions)
x0=dblarr(sz[0]+2,sz[1]+2)
z0=dblarr(sz[0]+2,sz[1]+2)
  x0[1:-2,1:-2]=x 
  x0[0,1:-2]=x[0,*]-(x[1,*]-x[0,*])
  x0[-1,1:-2]=x[-1,*]+(x[-1,*]-x[-2,*])
  x0[*,0]=x0[*,1]-(x0[*,2]-x0[*,1])
  x0[*,-1]=x0[*,-2]+(x0[*,-2]-x0[*,-3])
return,x0
end

function pp_airshdftocube_getfield,h,f,airsres=airsres
compile_opt idl2,logical_predicate
airsres=keyword_set(airsres)
if h[f].haskey('_DATA') then begin
  tmpb=h[f,'_DATA']
  if h[f].haskey('_FillValue') then tmpf=(h[f,'_FillValue','_DATA'])[0] else begin
    tmpf=!values.d_nan
  endelse
endif else begin
  tmpb=h[f,f,'_DATA']
  if h[f,f].haskey('_FillValue') then tmpf=(h[f,'_FillValue','_DATA'])[0] else begin
    if isa(tmpb,/integer) then begin
      print,minmax(tmpb)
    endif else tmpf=!values.d_nan
  endelse
endelse
tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
sz=size(tmpb,/dimensions)
case airsres of
  1: begin
    if n_elements(sz) gt 1 && sz[-2] eq 30 then begin
      szn=sz
      szn[-2:-1]*=3
      tmpb=n_elements(szn) eq 2 ? congrid(tmpb,szn[0],szn[1]) : congrid(tmpb,szn[0],szn[1],szn[2])
    endif
    if sz[-1] eq 135 then begin
      szn=n_elements(sz) eq 1 ? [90,135] : [sz[0:-2],90,135]
      tmpb=n_elements(szn) eq 2 ? congrid(tmpb,szn[0],szn[1]) : congrid(tmpb,szn[0],szn[1],szn[2])
    endif
  end
  0: begin
    if n_elements(sz) gt 1 && sz[-2] eq 90 then begin
      szn=sz
      szn[-2:-1]/=3
      tmpb=n_elements(szn) eq 2 ? congrid(tmpb,szn[0],szn[1]) : congrid(tmpb,szn[0],szn[1],szn[2])
    endif 
    if sz[0] eq 135 then begin
      szn=n_elements(sz) eq 1 ? [30,45] : [sz[0:-2],30,45]
      tmpb=n_elements(szn) eq 2 ? congrid(reform(tmpb,[1,sz]),szn[0],szn[1]) : congrid(reform(tmpb,[1,sz]),szn[0],szn[1],szn[2])
    endif
  end
endcase
return,tmpb
end


function pp_airshdftocube,f,tmfile=tmfile,noreload=noreload,nounload=nounload,loadonly=loadonly,$
  airsres=airsres
compile_opt idl2,logical_predicate

airsres=keyword_set(airsres)
tmfile=n_elements(tmfile) ? tmfile : cgsourcedir()+'/data/pp_aqua.tm'
if ~file_test(tmfile) then tmfile=(file_which('pp_aqua.tm'))[0]

if ~keyword_set(noreload) then cspice_furnsh,tmfile
if keyword_set(loadonly) then return,!null

;f='lsb_0034933739_0x53c_sci_1.fit'
if stregex(f,'AIRS.[[:digit:]]{4}\.[[:digit:]]{2}\.[[:digit:]]{2}\.[[:digit:]]{3}\.L1[B|C]\.AIRS_Rad\..*\.hdf',/boolean) then begin
  f1=f
  root=stregex(f,'AIRS.[[:digit:]]{4}\.[[:digit:]]{2}\.[[:digit:]]{2}\.[[:digit:]]{3}',/extract)
  f2=(file_search(file_dirname(f)+path_sep()+root+'.L2.RetStd_IR.*hdf'))[-1]
endif else begin
  f2=f
  root=stregex(f,'AIRS.[[:digit:]]{4}\.[[:digit:]]{2}\.[[:digit:]]{2}\.[[:digit:]]{3}',/extract)
  f1=(file_search(file_dirname(f)+path_sep()+root+'.L1B.AIRS_Rad.*hdf'))[-1]
  l1t='L1B_AIRS_Science'
  if ~f1 then begin
    f1=(file_search(file_dirname(f)+path_sep()+root+'.L1C.AIRS_Rad.*hdf'))[-1]
    l1t='L1C_AIRS_Science'
  endif
endelse

print,'reading ',f1
h1=hdf_parse(f1,/read_data)
print,'reading ',f2
h2=hdf_parse(f2,/read_data)


l1rad=h1[l1t,'Data Fields','radiances','_DATA']
l1rad_fill=(h1[l1t,'Data Fields','radiances','_FillValue','_DATA'])[0]
l1rad[where(l1rad eq l1rad_fill,/null)]=!values.d_nan
l1fre=h1[l1t,'Data Fields','nominal_freq','nominal_freq','_DATA']
if ~airsres then begin
  sz=size(l1rad,/dimensions)
  l1rad=congrid(l1rad,sz[0],sz[1]/3,sz[2]/3)  
endif


a=pp_airs(kernels=tmfile,hdf=file_expand_path(f2))
circ=4
bo=a.earthfovs(circ=0,exd=exd,/airs,/border)
;e3=a.earthfovs(circ=circ,exd=exd,/airs)
if airsres then e3r=a.earthfovs(circ=circ,exd=exd,/airs,rectangles=1.5d0) else e3r=a.earthfovs(circ=circ,exd=exd,/amsu,rectangles=1.5d0) 
;e3a=a.earthfovs(circ=circ,exd=exd,/amsu)

sz=size(l1rad,/dimensions)

nwav=sz[0]
nlines=sz[2]
nsamples=sz[1]


core=transpose(l1rad,[1,2,0])
backnames=['LATITUDE','LONGITUDE'];,'LAT_0','LON_0','ALT_0']
bunits=['DEGREE','DEGREE'];,'DEGREE','DEGREE','KM']
vars=['LAT','LON','ALT']
foreach var,vars do begin
  for ip=0,4 do begin
    backnames=[backnames,var+'_'+strtrim(ip,2)]
    bunits=[bunits,var eq 'ALT' ? 'KM' : 'DEGREE']
  endfor
endforeach
l1names=["dust_flag","dust_score","spectral_clear_indicator","BT_diff_SO2",$
  'sat_lat','sat_lon','satzen','satazi','solzen','solazi','glintlat','glintlon','sun_glint_distance','topog','landFrac']
backnames=[backnames,l1names]
bunits=[bunits,'dust_flag','dust_score','sci','K']
bunits=[bunits,replicate('degree',8),'km','m','fraction']

l2names=['TSurfAir','TSurfStd','PSurfStd','H2OMMRSatSurf','H2OMMRSurf','RelHumSurf',$
  'CH4_total_column','nSurfStd','PBest','PGood','nBestStd','nGoodStd','TSurfStdErr','TSurfAirErr',$
  'Temp_dof','PTropopause','T_Tropopause','totH2OStd','H2O_dof','GP_Tropopause','GP_Surface','CldFrcTot',$
  'totO3Std','MWSurfClass','totH2OMWOnlyStd','totCldH2OStd',$
  'CldFrcStd0','CldFrcStd1','PCldTop0','PCldTop1','TCldTop0','TCldTop1','nCld0']
bunits=[bunits,'K','K','hPa','g/kg','g/kg','percent',$
  'cm-2','index','index','index','index','index','K','K','dof',$
  'hPa','K','kg/m2','dof','m','m','fraction','DU','class','kg/m2','kg/m2',$
  'fraction','fraction','hPa','hPa','K','K','n']


h2['L2_Standard_atmospheric&surface_product','Data Fields','CldFrcStd0']=h2['L2_Standard_atmospheric&surface_product','Data Fields','CldFrcStd']
h2['L2_Standard_atmospheric&surface_product','Data Fields','CldFrcStd0','_NDIMENSIONS']=2L
h2['L2_Standard_atmospheric&surface_product','Data Fields','CldFrcStd0','_DIMENSIONS']=[90,135]
tmpd=h2['L2_Standard_atmospheric&surface_product','Data Fields','CldFrcStd','_DATA']
h2['L2_Standard_atmospheric&surface_product','Data Fields','CldFrcStd0','_DATA']=reform(tmpd[0,*,*,*,*],90,135)
h2['L2_Standard_atmospheric&surface_product','Data Fields','CldFrcStd1']=h2['L2_Standard_atmospheric&surface_product','Data Fields','CldFrcStd0']
h2['L2_Standard_atmospheric&surface_product','Data Fields','CldFrcStd1','_DATA']=reform(tmpd[1,*,*,*,*],90,135)

h2['L2_Standard_atmospheric&surface_product','Data Fields','PCldTop0']=h2['L2_Standard_atmospheric&surface_product','Data Fields','CldFrcStd0']
tmpd=h2['L2_Standard_atmospheric&surface_product','Data Fields','PCldTop','_DATA']
h2['L2_Standard_atmospheric&surface_product','Data Fields','PCldTop0','_DATA']=reform(tmpd[0,*,*,*,*],90,135)
h2['L2_Standard_atmospheric&surface_product','Data Fields','PCldTop1']=h2['L2_Standard_atmospheric&surface_product','Data Fields','PCldTop0']
h2['L2_Standard_atmospheric&surface_product','Data Fields','PCldTop1','_DATA']=reform(tmpd[1,*,*,*,*],90,135)

h2['L2_Standard_atmospheric&surface_product','Data Fields','TCldTop0']=h2['L2_Standard_atmospheric&surface_product','Data Fields','CldFrcStd0']
tmpd=h2['L2_Standard_atmospheric&surface_product','Data Fields','TCldTop','_DATA']
h2['L2_Standard_atmospheric&surface_product','Data Fields','TCldTop0','_DATA']=reform(tmpd[0,*,*,*,*],90,135)
h2['L2_Standard_atmospheric&surface_product','Data Fields','TCldTop1']=h2['L2_Standard_atmospheric&surface_product','Data Fields','TCldTop0']
h2['L2_Standard_atmospheric&surface_product','Data Fields','TCldTop1','_DATA']=reform(tmpd[1,*,*,*,*],90,135)

h2['L2_Standard_atmospheric&surface_product','Data Fields','nCld0']=h2['L2_Standard_atmospheric&surface_product','Data Fields','nCld']
tmpd=h2['L2_Standard_atmospheric&surface_product','Data Fields','nCld','_DATA']
h2['L2_Standard_atmospheric&surface_product','Data Fields','nCld0','_DATA']=reform(tmpd[*,*,*,*],90,135)

backnames=[backnames,l2names]
bunits=[bunits,'K','K','hPa','mmr','mmr','percent','ch4']

l2levvars=['TAirStd','GP_Height','O3VMRLevStd','COVMRLevStd','CH4VMRLevStd','TAirMWOnlyStd']
levs=h2['StdPressureLev:L2_Standard_atmospheric&surface_product','_DATA']
slevs=strtrim(string(levs,format='(F0.1)'),2)
l2namesl=[]
foreach ll,l2levvars do l2namesl=[l2namesl,ll+'_'+slevs]
nlevs=n_elements(levs)
backnames=[backnames,l2namesl]
bunits=[bunits,replicate('K',nlevs),replicate('m',nlevs),replicate('vmr',nlevs),$
  replicate('vmr',nlevs),replicate('vmr',nlevs),replicate('K',nlevs)]

hlevvars=['H2OMMRSatLevStd','H2OMMRLevStd','RelHum']
hlevs=h2['H2OPressureLev:L2_Standard_atmospheric&surface_product','_DATA']
shlevs=strtrim(string(hlevs,format='(I0)'),2)

l2nameshlevs=[]
foreach ll,hlevvars do l2nameshlevs=[l2nameshlevs,ll+'_'+shlevs]
backnames=[backnames,l2nameshlevs]
nhlevs=n_elements(hlevs)
bunits=[bunits,replicate('g/kg',nhlevs),replicate('g/kg',nhlevs),replicate('percent',nhlevs)]

hlayvars=['H2OMMRSat','H2OMMRStd']
hlays=h2['H2OPressureLay:L2_Standard_atmospheric&surface_product','_DATA']
shlays=strtrim(string(hlays,format='(I0)'),2)

l2nameshlays=[]
foreach ll, hlayvars do l2nameshlays=[l2nameshlays,ll+'_'+shlays]
backnames=[backnames,l2nameshlays]
nhlays=n_elements(hlays)
bunits=[bunits,replicate('g/kg',nhlays),replicate('g/kg',nhlays)]

nback=n_elements(backnames)
backplanes=dblarr(nsamples,nlines,nback)+!values.d_nan




if airsres then begin
  lat=h1[l1t,'Geolocation Fields','Latitude','_DATA']
  latfv=(h1[l1t,'Geolocation Fields','Latitude','_FillValue','_DATA'])[0]
  lon=h1[l1t,'Geolocation Fields','Longitude','_DATA']
  lonfv=(h1[l1t,'Geolocation Fields','Longitude','_FillValue','_DATA'])[0]
endif else begin
  lat=h2['L2_Standard_atmospheric&surface_product','Geolocation Fields','Latitude','_DATA']
  latfv=(h2['L2_Standard_atmospheric&surface_product','Geolocation Fields','Latitude','_FillValue','_DATA'])[0]
  lon=h2['L2_Standard_atmospheric&surface_product','Geolocation Fields','Longitude','_DATA']
  lonfv=(h2['L2_Standard_atmospheric&surface_product','Geolocation Fields','Longitude','_FillValue','_DATA'])[0]  
endelse
lat[where(lat eq latfv,/null)]=!values.d_nan
lon[where(lon eq lonfv,/null)]=!values.d_nan


backplanes[*,*,0]=lat
backplanes[*,*,1]=lon
for ip=1,4 do begin
  backplanes[*,*,2+ip]=e3r.lat[ip-1,*] ;LAT
  backplanes[*,*,7+ip]=e3r.lon[ip-1,*] ;LON
  backplanes[*,*,12+ip]=0d0 ;ALT
endfor
backplanes[*,*,2]=lat
backplanes[*,*,7]=lon
backplanes[*,*,12]=0d0 ;ALT

ln=17
foreach l1n,l1names,il1 do begin
  backplanes[*,*,ln++]=pp_airshdftocube_getfield(h1[l1t,'Data Fields'],l1n,airsres=airsres)
endforeach

szb=size(backplanes,/dimensions)

foreach l2n,l2names,il2 do begin
  ;print,l2n
  backplanes[*,*,ln++]=pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],l2n,airsres=airsres)
endforeach

foreach levvar,l2levvars do begin
  foreach lev,levs,il do begin
    backplanes[*,*,ln++]=reform((pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],levvar,airsres=airsres))[il,*,*])
  endforeach
endforeach

foreach hlevvar,hlevvars do begin
  foreach hlev,hlevs,il do begin
    backplanes[*,*,ln++]=reform((pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],hlevvar,airsres=airsres))[il,*,*])
  endforeach
endforeach

foreach hlayvar,hlayvars do begin
  foreach hlay,hlays,il do begin
    backplanes[*,*,ln++]=reform((pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],hlayvar,airsres=airsres))[il,*,*])
  endforeach
endforeach


wavs=1d4/l1fre
eh1=pp_eosparse(f1)
eh2=pp_eosparse(f2)
h0=eh1+eh2
h0['nadirTAI']=h1[l1t,'Data Fields','nadirTAI','nadirTAI','_DATA']
h0['tmfile']=tmfile
attr=eh2["L2_Standard_atmospheric&surface_product",'attributes']
h0['instrument']=attr['instrument']
h0['year']=(attr['start_year'])[0]
h0['month']=(attr['start_month'])[0]
h0['day']=(attr['start_day'])[0]
h0['hour']=(attr['start_hour'])[0]
h0['minu']=(attr['start_minute'])[0]
h0['sec']=(attr['start_sec'])[0]
h0['id0']=(attr['granule_number'])[0]
h0['doy']=julday(h0['month'],h0['day'],h0['year'])-julday(1,0,h0['year'])
h0['id1']=h0['year']*1000000+h0['doy']*1000+h0['id0']
h0['jday']=julday(h0['month'],h0['day'],h0['year'],h0['hour'],h0['minu'],h0['sec'])
ret={core:core,wavs:wavs,backplanes:backplanes,backnames:backnames,header:h0,bunits:bunits}

if ~keyword_set(nounload) then cspice_kclear
return,ret
end
