;docformat = 'rst rst'
;+
; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`)
;-
function pp_geosat::init,satname=satname,satid=satid,kernels=kernels,noclear=noclear
compile_opt idl2,logical_predicate

if (n_elements(satname)*n_elements(satid)*n_elements(kernels)) then begin
  self.satname=satname
  self.satid=satid
  self.kernels=kernels
  self.noclear=keyword_set(noclear)
  if ~self.noclear then cspice_kclear
  print,'Loading kernels from ',self.kernels
  cspice_furnsh,kernels
  return,1
endif else begin
  print,'Not enough data provided; need at least satellite name and ID'
  return,0
endelse
end

function pp_geosat::intercepts,strepoch=strepoch,xyz,usesatellitetime=usesatellitetime,$
  et=et,time=time,start_time=start_time
compile_opt idl2,logical_predicate

time=n_elements(time) ? time : 0d0

if ~keyword_set(usesatellitetime) then begin
  
  target   = self.satid
  strepoch    = n_elements(strepoch) ? strepoch : 'July 4, 2003 11:00 AM PST'
  frame    = 'ITRF93'
  observer = 'Earth'
  abcorr = 'LT+S'

  if (~n_elements(et)) then begin
    if n_elements(start_time) then begin
      cspice_str2et,start_time,et0
      et=time+et0
    endif else cspice_str2et, strepoch, et
  endif
  cspice_spkezr, target, et, frame,abcorr, observer, $
    state, ltime

  cspice_bodvrd,'EARTH','RADII',3,radii
  re=radii[0]
  rp=radii[2]
  flat=(re-rp)/re
  a=radii[0]
  b=radii[1]
  c=radii[2]
  
  sat_xyz=state[0:2]
  sat_v=state[3:5]
  
  defsysv,'!satind',exists=ex
  if ex then begin
    cspice_pgrrec,'EARTH',!satlon[!satind]*!dpi/180d0,!satlat[!satind]*!dpi/180d0,!satalt[!satind],re,flat,sat_xyz
    !satind++
  endif

  

  sat_z_axis=-(sat_xyz/norm(sat_xyz))
  cspice_ucrss,sat_z_axis,sat_v,sat_y_axis
  cspice_ucrss,sat_y_axis,sat_z_axis,sat_x_axis

  satMat=[[sat_x_axis],[sat_y_axis],[sat_z_axis]]
  
  if isa(xyz,'struct') then begin
    nvecs=n_elements(xyz.x)
    sz=size(xyz,/dimensions)
    detec_coord_from_earth=dblarr([3,sz])
    spoint=dblarr([3,sz])
    srfvec=dblarr([3,sz])
    trgepc=dblarr(sz)
    sfound=bytarr(sz)
    tsatMat=transpose(satMat)
    foreach xyzt,xyz,ix do begin
      cspice_mxv,tsatMat,[xyzt.x,xyzt.y,xyzt.z],detec_coord_from_earth1
      detec_coord_from_earth[ix*3]=detec_coord_from_earth1
      cspice_sincpt,'Ellipsoid',observer,et,frame,abcorr,self.satid,frame,$
        detec_coord_from_earth1,spoint1,trgepc1,srfvec1,sfound1
      spoint[ix*3]=spoint1
      trgepc[ix]=trgepc1
      srfvec[ix*3]=srfvec1
      sfound[ix]=sfound1
    endforeach
  endif else begin
    cspice_mxv,transpose(satMat),xyz,detec_coord_from_earth
    cspice_sincpt,'Ellipsoid',observer,et,frame,abcorr,self.satid,frame,$
      detec_coord_from_earth,spoint,trgepc,srfvec,sfound
  endelse


endif



  if isa(xyz,'struct') then begin
    point=dblarr([3,sz])
    found=bytarr(sz)
    foreach xyzt,xyz,ix do begin
      cspice_surfpt,sat_xyz,detec_coord_from_earth[ix*3:ix*3+2],a,b,c,point1,found1
      point[ix*3]=point1
      found[ix]=found1
    endforeach
    cspice_recpgr,'EARTH',reform(spoint,[3,nvecs]),re,flat,slon,slat,salt
    cspice_recpgr,'EARTH',reform(point,[3,nvecs]),re,flat,lon,lat,alt
    slon=reform(slon,sz,/overwrite)
    slat=reform(slat,sz,/overwrite)
    salt=reform(salt,sz,/overwrite)
    lon=reform(lon,sz,/overwrite)
    lat=reform(lat,sz,/overwrite)
    alt=reform(alt,sz,/overwrite)
  endif else begin
    cspice_surfpt,sat_xyz,detec_coord_from_earth,a,b,c,point,found
    cspice_recpgr,'EARTH',spoint,re,flat,slon,slat,salt
    cspice_recpgr,'EARTH',point,re,flat,lon,lat,alt
    cspice_reclat,spoint,salt,slon,slat
    cspice_reclat,ppoint,alt,lon,lat
  endelse
  
  cspice_reclat,sat_xyz,satalt,satlon,satlat
  cspice_recpgr,'EARTH',sat_xyz,re,flat,satlon,satlat,satalt
  
  ret={surfpt:point,surfptlat:lat*180d0/!dpi,surfptlon:lon*180d0/!dpi,surfptfound:found,$
    sincpt:spoint,sincptlat:slat*180d0/!dpi,sincptlon:slon*180d0/!dpi,sincptfound:sfound,$
    et:et,epoch:strepoch,time:time,start_time:start_time,sat_xyz:sat_xyz,sat_v:sat_v,$
    xyz:xyz,xyz_earth:detec_coord_from_earth,satlon:satlon*180d0/!dpi,satlat:satlat*180d0/!dpi,satalt:satalt}

  return,ret
end

pro pp_geosat::cleanup
compile_opt idl2,logical_predicate
;if ~self.noclear then cspice_kclear
end

pro pp_geosat::getproperty,satname=satname,satid=satid
compile_opt idl2,logical_predicate
if arg_present(satname) then satname=self.satname
if arg_present(satid) then satid=self.satid
end

pro pp_geosat__define
compile_opt idl2,logical_predicate
!null={pp_geosat,satname:'',satid:'',kernels:'',noclear:0B,inherits IDL_Object}
end
