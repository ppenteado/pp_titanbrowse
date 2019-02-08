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
tmpb=h[f,'_DATA']
tmpf=(h[f,'_FillValue','_DATA'])[0]
tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
sz=size(tmpb,/dimensions)
case airsres of
  1: begin
    if sz[-2] eq 30 then begin
      szn=sz
      szn[-2:-1]*=3
      tmpb=congrid(tmpb,szn)
    endif
  end
  0: begin
    if sz[-2] eq 90 then begin
      szn=sz
      szn[-2:-1]/=3
      tmpb=congrid(tmpb,szn[0],szn[1])
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

;d=mrdfits(f,0,h0,/dscale)
;w=mrdfits(f,1,/dscale)
;e=mrdfits(f,7,/dscale)
;a=mrdfits(f,2,/dscale)
;sz=size(d,/dimensions)
sz=size(l1rad,/dimensions)

nwav=sz[0]
nlines=sz[2]
nsamples=sz[1]





;x=dblarr(nlines,nsamples)
;y=dblarr(nlines,nsamples)
;z=dblarr(nlines,nsamples)
;mina=min(a[*,*,0],minloc)
;minl=array_indices(a[*,*,0],minloc)
;found=bytarr(nlines,nsamples)
;point=dblarr(3,nlines,nsamples)
;pointg=dblarr(3,nlines,nsamples)
;pf=poly_fit(dindgen(nsamples),e[0,*],1,/double,yfit=ets)
;ets2=[pf[0]-1d0*pf[1],ets,pf[0]+nsamples*pf[1]]
;qs=[[[0d0,0d0,0d0,0d0]],[e[1:4,*]],[[0d0,0d0,0d0,0d0]]]
;qs[*,0]=qs[*,1]-(qs[*,1]-qs[*,0])
;qs[*,-1]=qs[*,-1]+(qs[*,-1]-qs[*,-2])
;geop=list()
;ts=0.5d0*[0,-1,-1,1,1]
;for ip=0,4 do begin
;  for isample=0,nsamples-1 do begin
;    et=ets[isample]
;    et=interpolate(ets2,isample+ts[ip])
;    cspice_sxform, 'NH_SPACECRAFT', 'IAU_JUPITER', et, xform
;    cspice_sxform, 'J2000', 'IAU_JUPITER', et, xform2000
;    q=e[1:4,isample]
;    q=interpolate(qs,dindgen(4),isample+ts[ip],/grid,/double)
;    cspice_q2m, q, m
;    
;  ;  if isample eq 0 then oldxform=xform else begin
;  ;    print,isample,array_equal(oldxform,xform)
;  ;    oldxform=xform
;  ;  endelse
;    cspice_spkgps, target, et, frame, observer, pos, ltime
;    if ip eq 0 then begin
;      z[*,isample]=a[*,minl[1],2]
;      x[*,isample]=a[*,minl[1],0]
;      if isample eq (nsamples-1) then begin
;        x0=pp_nhfitstocube_expandarray(x)
;        z0=pp_nhfitstocube_expandarray(z)      
;      endif
;    endif else if (isample eq 0) then begin
;      indsi=1+dindgen(nlines)
;      indsj=1+dindgen(nsamples)
;      case ip of
;        1: begin
;            indsi-=0.5d0 & indsj-=0.5d0;top left
;          end
;        3: begin
;          indsi+=0.5d0 & indsj+=0.5d0;bottom right
;        end
;        2: begin
;          indsi+=0.5d0 & indsj-=0.5d0;top right
;        end
;        4: begin
;          indsi-=0.5d0 & indsj+=0.5d0;bottom left
;        end      
;      endcase
;      x=interpolate(x0,indsi,indsj,/grid,/double)
;      z=interpolate(z0,indsi,indsj,/grid,/double)
;    endif
;    for iline=0,nlines-1 do begin
;      u=[x[iline,isample],  y[iline,isample], z[iline,isample]]
;      
;      ;jstate = transpose(xform) # [u,0d0,0d0,0d0]
;      
;      uj = transpose(m) # u
;      jstate = transpose(xform2000) # [uj,0d0,0d0,0d0]
;      
;      
;      if ip eq 0 then begin
;        cspice_surfpt, pos, jstate[0:2], radii[0], radii[1], radii[2], pointp, foundp
;        found[iline,isample]=foundp  
;        cspice_recpgr, body, pointp, re, flat, lon, lat, alt
;        lon=lon*180d0/!dpi
;        cww=where(lon gt 180d0,ncww)
;        if ncww then lon[cww]-=360d0
;        point[*,iline,isample]=foundp ? [lon,lat*180d0/!dpi,alt] : !values.d_nan
;      endif
;      
;      cspice_npedln, radii[0], radii[1], radii[2], pos, jstate[0:2], pnear, dist
;      cspice_recpgr, body, pnear, re, flat, lon, lat, alt
;      lon=lon*180d0/!dpi
;      cww=where(lon gt 180d0,ncww)
;      if ncww then lon[cww]-=360d0
;      pointg[*,iline,isample]=[lon,lat*180d0/!dpi,dist]
;    endfor
;  endfor
;  geop.add,pointg
;endfor

;core=dblarr(nlines,nsamples,nwav+1)
;core[0]=transpose(d,[0,2,1])


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
backnames=[backnames,"dust_flag","dust_score","spectral_clear_indicator","BT_diff_SO2"]
backnames=[backnames,'TSurfAir','TSurfStd','PSurfStd','H2OMMRSatSurf','H2OMMRSurf','RelHumSurf',$
  'TAirStd_500','COVMRLevStd_500','CH4_total_column']

nback=n_elements(backnames)
backplanes=dblarr(nsamples,nlines,nback)+!values.d_nan
bunits=[bunits,'dust_flag','dust_score','sci','K']
bunits=[bunits,'K','K','hPa','mmr','mmr','percent','K','mmr','ch4']

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

;tmpb=h1[l1t,'Data Fields','dust_flag','_DATA']
;tmpf=(h1[l1t,'Data Fields','dust_flag','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,17]=pp_airshdftocube_getfield(h1[l1t,'Data Fields'],'dust_flag',airsres=airsres);tmpb

;tmpb=h1[l1t,'Data Fields','dust_score','_DATA']
;tmpf=(h1[l1t,'Data Fields','dust_score','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,18]=pp_airshdftocube_getfield(h1[l1t,'Data Fields'],'dust_score',airsres=airsres);tmpb

;tmpb=h1[l1t,'Data Fields','spectral_clear_indicator','_DATA']
;tmpf=(h1[l1t,'Data Fields','spectral_clear_indicator','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,19]=pp_airshdftocube_getfield(h1[l1t,'Data Fields'],'spectral_clear_indicator',airsres=airsres);tmpb

;tmpb=h1[l1t,'Data Fields','BT_diff_SO2','_DATA']
;tmpf=(h1[l1t,'Data Fields','BT_diff_SO2','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,20]=pp_airshdftocube_getfield(h1[l1t,'Data Fields'],'BT_diff_SO2',airsres=airsres);tmpb

szb=size(backplanes,/dimensions)

;tmpb=h2['L2_Standard_atmospheric&surface_product','Data Fields','TSurfAir','_DATA']
;tmpf=(h2['L2_Standard_atmospheric&surface_product','Data Fields','TSurfAir','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,21]=pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],'TSurfAir',airsres=airsres);congrid(tmpb,szb[0],szb[1])

;tmpb=h2['L2_Standard_atmospheric&surface_product','Data Fields','TSurfStd','_DATA']
;tmpf=(h2['L2_Standard_atmospheric&surface_product','Data Fields','TSurfStd','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,22]=pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],'TSurfStd',airsres=airsres);congrid(tmpb,szb[0],szb[1])

;tmpb=h2['L2_Standard_atmospheric&surface_product','Data Fields','PSurfStd','_DATA']
;tmpf=(h2['L2_Standard_atmospheric&surface_product','Data Fields','PSurfStd','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,23]=pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],'PSurfStd',airsres=airsres);congrid(tmpb,szb[0],szb[1])

;tmpb=h2['L2_Standard_atmospheric&surface_product','Data Fields','H2OMMRSatSurf','_DATA']
;tmpf=(h2['L2_Standard_atmospheric&surface_product','Data Fields','H2OMMRSatSurf','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,24]=pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],'H2OMMRSatSurf',airsres=airsres);congrid(tmpb,szb[0],szb[1])

;tmpb=h2['L2_Standard_atmospheric&surface_product','Data Fields','H2OMMRSurf','_DATA']
;tmpf=(h2['L2_Standard_atmospheric&surface_product','Data Fields','H2OMMRSurf','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,25]=pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],'H2OMMRSurf',airsres=airsres);congrid(tmpb,szb[0],szb[1])

;tmpb=h2['L2_Standard_atmospheric&surface_product','Data Fields','RelHumSurf','_DATA']
;tmpf=(h2['L2_Standard_atmospheric&surface_product','Data Fields','RelHumSurf','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,26]=pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],'RelHumSurf',airsres=airsres);congrid(tmpb,szb[0],szb[1])

;tmpb=h2['L2_Standard_atmospheric&surface_product','Data Fields','TAirStd','_DATA']
;tmpf=(h2['L2_Standard_atmospheric&surface_product','Data Fields','TAirStd','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,27]=reform((pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],'TAirStd',airsres=airsres))[6,*,*]);congrid(reform(tmpb[6,*,*]),szb[0],szb[1])

;tmpb=h2['L2_Standard_atmospheric&surface_product','Data Fields','COVMRLevStd','_DATA']
;tmpf=(h2['L2_Standard_atmospheric&surface_product','Data Fields','COVMRLevStd','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,28]=reform((pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],'COVMRLevStd',airsres=airsres))[6,*,*]);congrid(reform(tmpb[6,*,*]),szb[0],szb[1])

;tmpb=h2['L2_Standard_atmospheric&surface_product','Data Fields','CH4_total_column','_DATA']
;tmpf=(h2['L2_Standard_atmospheric&surface_product','Data Fields','CH4_total_column','_FillValue','_DATA'])[0]
;tmpb[where(tmpb eq tmpf,/null)]=!values.d_nan
backplanes[*,*,29]=pp_airshdftocube_getfield(h2['L2_Standard_atmospheric&surface_product','Data Fields'],'CH4_total_column',airsres=airsres);congrid(tmpb,szb[0],szb[1])

wavs=1d4/l1fre
eh1=pp_eosparse(f1)
eh2=pp_eosparse(f2)
h0=eh1+eh2
h0['nadirTAI']=h1[l1t,'Data Fields','nadirTAI','nadirTAI','_DATA']
h0['tmfile']=tmfile
ret={core:core,wavs:wavs,backplanes:backplanes,backnames:backnames,header:h0,bunits:bunits}

if ~keyword_set(nounload) then cspice_kclear
return,ret
end
