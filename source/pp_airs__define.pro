;docformat = 'rst rst'
;+
; :Author: Paulo Penteado (Paulo.Penteado@jpl.nasa.gov)
;-
function pp_airs::init,kernels=kernels,hdf=hdf
compile_opt idl2,logical_predicate
ret=self.pp_geosat::init(satname='AQUA',satid='-127424',kernels=kernels)
if n_elements(hdf) then begin
  self.hdf=hdf
  self.hdfo=hdf_parse(hdf,/read_data)
endif
return,ret
end

function pp_airs::scanfovs,circle_points=icp,amsu=amsu,hsb=hsb,airs=airs,exd=exd,$
  border=border,rectangles=rectangles
compile_opt idl2,logical_predicate
border=keyword_set(border)
cp=border ? 32 : icp
amsu=keyword_set(amsu)
hsb=amsu ? 0B : keyword_set(hsb)
airs=(hsb || amsu) ? 0B : 1
if keyword_set(rectangles) then begin
  airs=0
  hsb=0
  amsu=0
endif else rectangles=0

;scan angle: 3.3° diameter, all adjacent
;http://disc.sci.gsfc.nasa.gov/AIRS/images/Scan_Geo.jpg
;http://disc.sci.gsfc.nasa.gov/AIRS/documentation/airs_instrument_guide.shtml

scanangsize=1.1d0*!dpi/180d0*(amsu ? 3d0 : 1d0)
case 1 of
  amsu : begin
    image_angles_theta_scan=(dindgen(30)-14.5d0)*scanangsize
    port_center=0d0
    image_angles_phi_track=replicate(0d0,30)
    xind=indgen(30)
    yind=replicate(1,30)
  end
  hsb : begin
    image_angles_theta_scan=(dindgen(90)-44d0)*scanangsize
    image_angles_theta_scan=[image_angles_theta_scan,image_angles_theta_scan,image_angles_theta_scan]
    port_center=0d0
    image_angles_phi_track=replicate(0d0,270)
    image_angles_phi_track[0:89]=-scanangsize*1.25d0
    image_angles_phi_track[180:269]=scanangsize*1.25d0
    xind=indgen(90)
    xind=[xind,xind,xind]
    yind=intarr(270)
    yind[90:179]=1
    yind[180:269]=2
    if ~keyword_set(exd) then begin
      newind=indgen(3,30)
      newind=[newind,newind+90,newind+180]
      newind=reform(newind,270)
      image_angles_theta_scan=image_angles_theta_scan[newind]
      image_angles_phi_track=image_angles_phi_track[newind]
      xind=xind[newind]
      yind=yind[newind]
    endif
  end
  else : begin
    maxang=asin(0.6d0/1.1d0)
    image_angles_theta_scan=(dindgen(90)-44d0)*scanangsize
    image_angles_theta_scan=[image_angles_theta_scan,image_angles_theta_scan,image_angles_theta_scan]
    port_center=0d0
    image_angles_phi_track=replicate(0d0,270)
    image_angles_phi_track[0:89]=-scanangsize*1.25d0
    image_angles_phi_track[180:269]=scanangsize*1.25d0
    xind=indgen(90)
    xind=[xind,xind,xind]
    yind=intarr(270)
    yind[90:179]=1
    yind[180:269]=2
    if ~keyword_set(exd) then begin
      newind=indgen(3,30)
      newind=[newind,newind+90,newind+180]
      newind=reform(newind,270)
      image_angles_theta_scan=image_angles_theta_scan[newind]
      image_angles_phi_track=image_angles_phi_track[newind]
      xind=xind[newind]
      yind=yind[newind]
    endif
  end
endcase


;fov center vectors, in satellite frame
;http://disc.sci.gsfc.nasa.gov/AIRS/documentation/v6_docs/v6releasedocs-1/AIRS_L1C_UserGuide.pdf
;+x axis is positively oriented in
;the direction of orbital flight completing an orthogonal triad
;with y and z.
;+y axis is oriented normal
;to the orbit plane with the positive sense opposite to that of
;the orbit's angular momentum vector H.
;+z axis is positively oriented
;Earthward parallel to the satellite radius vector R from the
;spacecraft center of mass to the center of the Earth.
center=0d0-0.5d0*scanangsize*!dpi/180d0
image_angles_theta_scan*=1d0
image_angles_phi_track*=1d0
xcenter=sin(image_angles_phi_track)
zcenter=cos(center-image_angles_theta_scan)*cos( image_angles_phi_track )
ycenter=-sin(center-image_angles_theta_scan)*cos( image_angles_phi_track  )

cp=n_elements(cp) ? cp : 0

if cp then begin
  xr=dblarr(cp,n_elements(xcenter))
  yr=xr
  zr=xr
  if rectangles then begin
    scanoffsets=scanangsize*[0.5d0,-0.5d0,-0.5d0,0.5d0]
    trackoffsets=[0.5d0,0.5d0,-0.5d0,-0.5d0]*scanangsize*rectangles
    for icorner=0,3 do begin
      xe=sin(image_angles_phi_track+trackoffsets[icorner])
      ecenter=scanoffsets[icorner]+center
      ze=cos(ecenter-image_angles_theta_scan)*cos( image_angles_phi_track+trackoffsets[icorner] )
      ye=-sin(ecenter-image_angles_theta_scan)*cos( image_angles_phi_track+trackoffsets[icorner] )
      angs=replicate(0d0,cp)
        xr[icorner*cp/4:(icorner+1)*cp/4-1,*]=replicate(1d0,cp/4)#xe
        yr[icorner*cp/4:(icorner+1)*cp/4-1,*]=replicate(1d0,cp/4)#ye
        zr[icorner*cp/4:(icorner+1)*cp/4-1,*]=replicate(1d0,cp/4)#ze
    endfor
  endif else begin
    if airs then begin
      angs=((dindgen(cp/2)/(cp/2-1d0))-0.5d0)*2d0*maxang+!dpi/2d0
      angs=[angs,-reverse(angs)]
    endif else angs=2d0*!dpi*dindgen(cp)/(cp-1d0)
    ;create vectors on the edge of each circle, by shifting by the radius in the scan direction
    xe=sin(image_angles_phi_track)
    ecenter=scanangsize*0.5d0+center
    ze=cos(ecenter-image_angles_theta_scan)*cos( image_angles_phi_track )
    ye=-sin(ecenter-image_angles_theta_scan)*cos( image_angles_phi_track  )
    ;rotate these vectors around the center vectors
    foreach ang,angs,ia do foreach x,xcenter,ix do begin
      cspice_vrotv,[xe[ix],ye[ix],ze[ix]],[xcenter[ix],ycenter[ix],zcenter[ix]],ang,r1
      xr[ia,ix]=r1[0]
      yr[ia,ix]=r1[1]
      zr[ia,ix]=r1[2]
    endforeach
  endelse
  ret=replicate({x:!values.d_nan,y:!values.d_nan,z:!values.d_nan,$
    xr:dblarr(cp),yr:dblarr(cp),zr:dblarr(cp)},n_elements(xcenter))
  ret.x=xcenter
  ret.y=ycenter
  ret.z=zcenter
  ret.xr=xr
  ret.yr=yr
  ret.zr=zr
endif else ret=pp_structtransp({x:xcenter,y:ycenter,z:zcenter})

if border then begin
;  xmin=min(xr,ixmin,max=xmax,subscript_max=ixmax)
;  wmin=where(xr eq xmin)
;  wmax=where(xr eq xmax)
;  y1=min(yr[wmin],iy1,max=y2,subscript_max=iy2)
;  x1=xr[wmin[iy1]] & x2=xr[wmin[iy2]]
;  z1=zr[wmin[iy1]] & z2=zr[wmin[iy2]]
;  y3=min(yr[wmax],iy3,max=y4,subscript_max=iy4)
;  x3=xr[wmax[iy3]] & x4=xr[wmax[iy4]]
;  z3=zr[wmax[iy3]] & z4=zr[wmax[iy4]] 
;  xbo=[x1,x2,x3,x4]
;  ybo=[y1,y2,y3,y4]
;  zbo=[z1,z2,z3,z4]
;  ret=pp_structtransp({x:xbo,y:ybo,z:zbo})
  icp=0
  indsi=lindgen(n_elements(xr))
  cp4=cp/4
  indsbl=indsi[cp4*3:cp4*4-1] ;bottom left
  indstl=indsi[6*cp:6*cp+cp4-1] ;top left
  indstr=indsi[cp*269+cp4:cp*269+2*cp4-1] ;top roght
  indsbr=indsi[cp*263+2*cp4:cp*263+3*cp4-1] ;bottom right
  indsb=lonarr(90) ;bottom
  for ib=0,2 do indsb[ib:-1:3]=indsi[3*cp4+ib*cp:-1:9*cp]
  indsb=reverse(indsb)
  indst=lonarr(90) ;top
  for ib=0,2 do indst[ib:-1:3]=indsi[6*cp+cp4+ib*cp:-1:9*cp]
  indsl=[0L,4L*cp-1,3L*cp,7L*cp-1] ;left
  indsr=[269L*cp+2*cp4,266L*cp+2*cp4-1,266L*cp+2*cp4,263L*cp+2*cp4-1] ;right
  ret=({bl:[[xr[indsbl]],[yr[indsbl]],[zr[indsbl]]],$
    tl:[[xr[indstl]],[yr[indstl]],[zr[indstl]]],$
    tr:[[xr[indstr]],[yr[indstr]],[zr[indstr]]],$
    br:[[xr[indsbr]],[yr[indsbr]],[zr[indsbr]]],$
    b:[[xr[indsb]],[yr[indsb]],[zr[indsb]]],$
    t:[[xr[indst]],[yr[indst]],[zr[indst]]],$
    l:[[xr[indsl]],[yr[indsl]],[zr[indsl]]],$
    r:[[xr[indsr]],[yr[indsr]],[zr[indsr]]]})
    
endif

return,ret

end


function pp_airs::earthfovs,_ref_extra=ex,circle_points=cp,$
  verbose=verbose,border=border
compile_opt idl2,logical_predicate

border=keyword_set(border)

h=self.hdfo
hk=(h.keys()).toarray()
w=where(strmatch(hk,'L2_*_atmospheric&surface_product'),/null,ccw)
if (ccw eq 0) then w=where(strmatch(hk,'L1B_AIRS_Science'),/null)
;nadirtai=h["L2_Standard_atmospheric&surface_product","Data Fields","nadirTAI","nadirTAI","_DATA"]
nadirtai=h[hk[w[0]],"Data Fields","nadirTAI","nadirTAI","_DATA"]
;nadirtai=mg_hdf_getdata(self.hdf,"nadirTAI")
nt=n_elements(nadirtai)

fovs=self.scanfovs(_strict_extra=ex,circle_points=cp,border=border)

if border then begin
  left=list()
  right=list()
endif else begin
  if cp then begin
    lats=dblarr(cp,n_elements(fovs)*n_elements(nadirtai))
    lons=lats
  endif else begin
    lats=dblarr(n_elements(fovs),n_elements(nadirtai))
    lons=lats
  endelse
endelse
ii=[]
ic=0L
foreach tai,nadirtai,it do begin
  if keyword_set(verbose) then print,it
  if border then begin
    if it eq 0 then begin
      fovsb=pp_structtransp({x:fovs.b[*,0],y:fovs.b[*,1],z:fovs.b[*,2]})
      i=self.intercepts(start_time='1993-01-01T00:00:00',time=tai,fovsb)
      fovsb=pp_structtransp({x:fovs.bl[*,0],y:fovs.bl[*,1],z:fovs.bl[*,2]})
      il=self.intercepts(start_time='1993-01-01T00:00:00',time=tai,fovsb)
      fovsb=pp_structtransp({x:fovs.br[*,0],y:fovs.br[*,1],z:fovs.br[*,2]})
      ir=self.intercepts(start_time='1993-01-01T00:00:00',time=tai,fovsb)
      bottom={lat:[ir.surfptlat,i.surfptlat,il.surfptlat],lon:[ir.surfptlon,i.surfptlon,il.surfptlon]}
    endif
    if it eq n_elements(nadirtai)-1 then begin
      fovsb=pp_structtransp({x:fovs.t[*,0],y:fovs.t[*,1],z:fovs.t[*,2]})
      i=self.intercepts(start_time='1993-01-01T00:00:00',time=tai,fovsb)
      top={lat:i.surfptlat,lon:i.surfptlon}
    endif
    fovsb=pp_structtransp({x:fovs.l[*,0],y:fovs.l[*,1],z:fovs.l[*,2]})
    i=self.intercepts(start_time='1993-01-01T00:00:00',time=tai,fovsb)
    l={lat:i.surfptlat,lon:i.surfptlon}
    fovsb=pp_structtransp({x:fovs.r[*,0],y:fovs.r[*,1],z:fovs.r[*,2]})
    i=self.intercepts(start_time='1993-01-01T00:00:00',time=tai,fovsb)
    r={lat:i.surfptlat,lon:i.surfptlon}
    
    left.add,l
    right.add,reverse(r)
  endif else begin
    if cp then begin
      foreach fov,fovs,ifov do begin
        fovsc=pp_structtransp({x:fov.xr,y:fov.yr,z:fov.zr})
        i=self.intercepts(start_time='1993-01-01T00:00:00',time=tai,fovsc)
        lats[*,ic]=i.surfptlat
        lons[*,ic]=i.surfptlon
        ic+=1
      endforeach
    endif else begin
      i=self.intercepts(start_time='1993-01-01T00:00:00',time=tai,fovs)
      lons[*,it]=i.surfptlon
      lats[*,it]=i.surfptlat
    endelse
  endelse
  ii=[ii,i]
endforeach

if border then begin
  left=left.toarray()
  right.reverse
  right=right.toarray()
  nel=n_elements(left.lat)
  ret={lats:[bottom.lat,reform(left.lat,nel),top.lat,reform(right.lat,nel)],lons:[bottom.lon,reform(left.lon,nel),top.lon,reform(right.lon,nel)]}
endif else ret={lat:lats,lon:lons,ii:ii}

return,ret
end


pro pp_airs__define
compile_opt idl2,logical_predicate

!null={pp_airs,inherits pp_geosat,hdf:'',hdfo:obj_new()}

end

