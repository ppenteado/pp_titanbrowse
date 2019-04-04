function pp_wmts::init,cache=cache,verbose=verbose,nextday=nextday,$
  conturl=conturl
compile_opt idl2,logical_predicate
self.cache=n_elements(cache) ? cache : 2
self.verbose=keyword_set(verbose)
self.nextday=keyword_set(nextday)

;getcapabilities url
self.conturl=n_elements(conturl) ? conturl : 'https://gibs.earthdata.nasa.gov/wmts/epsg4326/best/1.0.0/WMTSCapabilities.xml'
;'https://gibs.earthdata.nasa.gov/wmts/epsg4326/best/wmts.cgi?SERVICE=WMTS&request=GetCapabilities'
;'https://gibs.earthdata.nasa.gov/wmts/epsg{EPSG:Code}/best/{ProductName}/default/{Time}/{TileMatrixSet}/{ZoomLevel}/{TileRow}/{TileCol}.png'
;'https://gibs.earthdata.nasa.gov/wmts/epsg4326/best/wmts.cgi?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0&FORMAT=image%2Fpng'
;'https://gibs.earthdata.nasa.gov/wmts/epsg4326/best/wmts.cgi?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=MODIS_Terra_CorrectedReflectance_TrueColor&STYLE=&TILEMATRIXSET=250m&TILEMATRIX=6&TILEROW=13&TILECOL=36&FORMAT=image%2Fjpeg&TIME=2012-07-09'
if self.cache then begin
  rid='pp_wmts_tmp'
  tdir=filepath(getenv('USER')+'_'+rid+path_sep(),/tmp)
  tf=idl_validname(file_dirname(self.conturl),/convert_all)+'_'+idl_validname(file_basename(self.conturl),/convert_all)
  t=tdir+tf+'.sav'
  if ~file_test(t,/read) then begin
    cap=read_xml8(pp_wget_cached(self.conturl,rid=rid,verbose=self.verbose))
    save,file=t,cap,/verbose
  endif else begin
    if self.verbose then print,'Using cached capabilities data' 
    restore,t,/verbose
  endelse
  self.cap=cap
endif else begin 
  self.cap=read_xml8_pp(string=strjoin(wget(self.conturl,/string_array),byte(10)))
endelse

;parse capabilities
c=self.cap['Capabilities','Contents','TileMatrixSet']

tms=orderedhash(/fold_case)
if ~isa(c,'list') then c=list(c)
foreach cc,c do begin
  tms[cc["ows_Identifier","_text"]]=cc
  tms[cc["ows_Identifier","_text"],"TileMatrix"]=pp_xml8hashparsetext(cc["TileMatrix"],/fold_case)
endforeach
self.tms=tms
c=self.cap['Capabilities','Contents','Layer']
tms=orderedhash(/fold_case)
foreach cc,c do tms[cc["ows_Identifier","_text"]]=pp_xml8hashparsetext(cc,/fold_case)
self.layers=tms
self.verbose=keyword_set(verbose)
return,1
end



pro pp_wmts::getproperty,cap=cap,layers=layers,tms=tms
compile_opt idl2,logical_predicate
if arg_present(cap) then cap=self.cap
if arg_present(tms) then tms=self.tms
if arg_present(layers) then layers=self.layers

end

function pp_wmts::boundingboxtotiles,bb,layer,level,proj=proj,ms=ms,$
  range=range
compile_opt idl2,logical_predicate

;get layer and tilematrix parameters
l=self.layers[layer]
catch,err
if err then begin
  catch,/cancel
  ll=l['TileMatrixSetLink']
  if isa(ll,'list') then ll=ll[0]
  tm=self.tms[ll['TileMatrixSet','_text'],'TileMatrix',level]
  tmt=self.tms[ll['TileMatrixSet','_text']]
endif else begin
  tm=self.tms[l['TileMatrixSetLink','TileMatrixSet'],'TileMatrix',level]
  tmt=self.tms[l['TileMatrixSetLink','TileMatrixSet']]
endelse
;tm=self.tms[l['TileMatrixSetLink','TileMatrixSet'],level]
w=long(tm['TileWidth'])
h=long(tm['TileHeight'])
mw=long(tm['MatrixWidth'])
mh=long(tm['MatrixHeight'])
tlc=double(strsplit(tm['TopLeftCorner'],/extract))

if tmt.haskey('ows_SupportedCRS') then begin
  crs=tmt['ows_SupportedCRS','_text']
endif


if tmt.haskey('ows_BoundingBox') then begin
  lowercorner=tmt['ows_BoundingBox','ows_LowerCorner','_text']
  uppercorner=tmt['ows_BoundingBox','ows_UpperCorner','_text']
  lcorner=double(strsplit(lowercorner,/extract))
  ucorner=double(strsplit(uppercorner,/extract))
endif else begin
  lcorner=[0d0,0d0]
  ucorner=[0d0,0d0]
endelse

;if product(self.lcorner*self.ucorner) then begin
;  tlc*=[-180d0,90d0]/self.ucorner
;endif
case 1 of
  stregex(crs,'EPSG\:([[:digit:]\.\:])+(\.|\:)3857',/boolean): begin ;web mercator
    ;ms=map_proj_init('mercator',sphere_radius=6378137d0)
    proj='webmercator'
    ms=map_proj_init('mercator',/gctp,semimajor_axis=6378137d0,semiminor_axis=6378137d0)
    llimit=[-85.05d0,-180d0,85.05d0,180d0]
    ;print,self.crs
    if ~product(lcorner*ucorner) then begin
      lcorner=map_proj_forward(180d0,-85.0511d0,map_structure=ms)
      ucorner=map_proj_forward(-180d0,85.0511d0,map_structure=ms)
    endif
  end
  else: begin
    proj='geographic'
    ms=map_proj_init(proj)
    llimit=[-90d0,-180d0,90d0,180d0]
    ;print,self.crs
    if ~product(lcorner*ucorner) then begin
      lcorner=[180d0,-90d0]
      ucorner=[-180d0,90d0]
    endif
  end
endcase

obb=bb
bb[0]=bb[0]>llimit[1]
bb[1]=bb[1]>llimit[0]
bb[2]=bb[2]<llimit[3]
bb[3]=bb[3]<llimit[2]
tmp=map_proj_forward(bb[0],bb[1],map_structure=ms)
bb[0:1]=tmp
tmp=map_proj_forward(bb[2],bb[3],map_structure=ms)
bb[2:3]=tmp


;calculate tiles that cover the bounding box
lw=(lcorner[0]-ucorner[0])/mw
lh=(lcorner[1]-ucorner[1])/mh
left=floor((bb[0]-tlc[0])/lw)
right=ceil((bb[2]-tlc[0]-lw)/lw)
top=floor((bb[3]-tlc[1])/lh)
bottom=ceil((bb[1]-tlc[1]-lh)/lh)
tbb=[tlc[0]+lw*left,tlc[1]+lh*(bottom+1),tlc[0]+lw*(right+1),tlc[1]+lh*(top)];true output bounding box (the bounding box for the tiles) 
fwid=w*(right-left+1)
fhei=h*(bottom-top+1)


;calculate what area is used in the whole tile grid
scd=double(tm['ScaleDenominator'])
case 1 of
  
  proj eq 'geographic' : begin
    a=6378137d0 ;Earth equatorial radius
    b=6356752.3142d0 ;Earth polar radius
    rwid=(tbb[2]-tbb[0])*a*!dpi/180d0
    rhei=(tbb[3]-tbb[1])*a*!dpi/180d0
  end
  else: begin
    rwid=(tbb[2]-tbb[0])
    rhei=(tbb[3]-tbb[1])
    ;print,proj
  end
endcase

;scaledenominator uses the standard .028mm pixel size
lastcol=round(rwid/(scd*.28d-3))-1
firstrow=fhei-round(rhei/(scd*.28d-3))

;calculate at what indices the full image would have to be cropped to obtain the same bounding box as the input
scx=(tbb[2]-tbb[0])/(lastcol+1d0)
scy=(tbb[3]-tbb[1])/(fhei-firstrow+1d0)
cropinds=long([(bb[0]-tbb[0])/scx,(bb[1]-tbb[1])/scy,(bb[2]-tbb[0])/scx-1d0,(bb[3]-tbb[1])/scy-2])


templ=l['ResourceURL']
if isa(templ,'list') then templ=templ[0]
templ=templ['template']
range=bb
ret={layer:layer,bb:bb,tbb:tbb,col:[left,right],row:[top,bottom],template:templ,$
  lobj:l,tobj:tm,rwid:rwid,rhei:rhei,fwid:fwid,fhei:fhei,lastcol:lastcol,firstrow:firstrow,$
  cropinds:cropinds,range:range,obb:obb}
bb=obb
return,ret
end

function pp_wmts::autolevel,bb,layer,dims,crop=crop,outdims=outdims,bbts=bbts
compile_opt idl2,logical_predicate

;get layer and tilematrix parameters
l=self.layers[layer]
catch,err
if err then begin
  catch,/cancel
  ll=l['TileMatrixSetLink']
  if isa(ll,'list') then ll=ll[0]
  tms=self.tms[ll['TileMatrixSet','_text'],'TileMatrix']

endif else tms=self.tms[l['TileMatrixSetLink','TileMatrixSet'],'TileMatrix']
bbts=list()
for lev=0,n_elements(tms)-1 do begin
  bbts.add,self.boundingboxtotiles(bb,layer,lev)
endfor
bbts=bbts.toarray()

if keyword_set(crop) then begin
  ci=bbts.cropinds
  fwids=reform(ci[2,*]-ci[0,*]+1)
  fheis=reform(ci[3,*]-ci[1,*]+1)
endif else begin
  fwids=bbts.fwid
  fheis=bbts.fhei
endelse
w=where((fwids ge dims[0]) and (fheis ge dims[1]))
ret=w[0]
return,ret

end

function pp_wmts::getimage,bb,layer,level,time=time,found=found,proj=proj,ms=ms,$
  bbt=bbt
compile_opt idl2,logical_predicate

time=n_elements(time) ? time : '2017-07-01'

;get bounding box data
bbt=self.boundingboxtotiles(bb,layer,level,proj=proj,ms=ms)

;prepare arrays
tw=long(bbt.tobj['TileWidth'])
th=long(bbt.tobj['TileHeight'])

wid=bbt.fwid
hei=bbt.fhei

;prepare urls
templ=bbt.template
if stregex(templ,'/{Style}/',/boolean) then begin
  style=bbt.lobj['Style']
  if ~isa(style,'list') then style=list(style)
  foreach st,style do begin
    if strmatch(st['isDefault'],'true',/fold_case) then begin
      stfound=1
      stylestr=st['ows_Identifier']
    endif
  endforeach
  if ~stfound then stylestr=st['ows_Identifier']
  templ=templ.replace('{Style}',stylestr) 
endif
burl=templ.replace('{TileMatrixSet}',bbt.lobj["TileMatrixSetLink","TileMatrixSet"])
burl=burl.replace('{TileMatrix}',strtrim(level,2))
;burl=burl.replace('{Time}',time)

;get colortables, if any
havecmap=0B
if bbt.lobj.haskey('ows_Metadata') then begin
  md=bbt.lobj['ows_Metadata']
  foreach m,md do begin
    if m.haskey('xlink_role') && strmatch(m['xlink_role'],'*colormap*',/fold_case) then begin
      cmap=self.cache ? (read_xml8(pp_wget_cached(m['xlink_href'],rid='pp_wmts_tmp',verbose=self.verbose)))['ColorMaps','ColorMap'] : $
       (read_xml8_pp(string=strjoin(wget(m['xlink_href'],/string_array),byte(10))))['ColorMaps','ColorMap']
      break
    endif
  endforeach
endif
if isa(cmap,'hash')||isa(cmap,'list') then begin
  cmaph=orderedhash()
  cmaphr=list()
  cmapr=list()
  if isa(cmap,'hash') then cmap=list(cmap)
  foreach cm,cmap do begin
    if ~isa(cm['Entries','ColorMapEntry'],'list') then cml=list(cm['Entries','ColorMapEntry']) else cml=cm['Entries','ColorMapEntry']
    foreach e,cml do begin
      val=(fix(e['ref']))
      rgb=byte(fix(strsplit(e['rgb'],', ',/extract)))
      alpha=strmatch(e['transparent'],'*true*',/fold_case) ? 0B : 255B
      print,e,/impl
      if e.haskey('value') then begin
        value=strsplit(e['value'],/extract,'[]()')
        foreach val,value do begin
          sval=strtrim(strsplit(val,',',/extract),2)
          w=where(sval,count)
          if count then begin
            if count eq 1 then cmaph[fix(sval[0])]=[rgb,alpha] else begin
              ;for vv=fix(sval[0]),fix(sval[1]) do cmaph[vv]=[rgb,alpha]
              cmaphr.add,double(sval[0])
              cmaphr.add,double(sval[1])
              cmapr.add,[rgb,alpha]
              cmaph[fix(e['ref'])]=[rgb,alpha]
            endelse
          endif
        endforeach
      endif
    endforeach
  endforeach
  havecmap=1B
  cmaphr=cmaphr.toarray()
endif

;get tiles
twid=long(bbt.tobj['MatrixWidth'])
for r=bbt.row[0],bbt.row[1] do begin
  for c=bbt.col[0],bbt.col[1] do begin
    ;cwrapped=c lt twid ? c : ((c+1) mod twid)-1
    if c lt twid then begin
      cwrapped=c
      ctime=time
    endif else begin
      cwrapped=((c+1) mod twid)-1
      if self.nextday then begin
        cjd=julday(strmid(time,5,2),strmid(time,8,2),strmid(time,0,4))
        ctime=string(cjd-1,format='(C(CYI4.4,"-",CMOI2.2,"-",CDI2.2))')
      endif else ctime=time
    endelse
    url=burl.replace('{TileRow}',strtrim(r,2))
    url=url.replace('{TileCol}',strtrim(cwrapped,2))
    url=url.replace('{Time}',ctime)
    if self.verbose then print,url
    t=pp_wget_cached(url,rid='pp_wmts_tmp',clear=~self.cache,verbose=self.verbose)
    !null=query_image(t,tiinfo)
    ti=read_image(t,rgbr,rgbg,rgbb)
    if tiinfo.has_palette then begin
      nti=bytarr([4,size(ti,/dimensions)])+255B
      l=pp_locate(ti)
      iv=0
      rgbrgb=transpose([[rgbr],[rgbg],[rgbb]])
      foreach ll,l,v do begin
        for ic=0,2 do nti[ll*4+ic]=rgbrgb[ic,v]
      endforeach
      ti=nti
    endif
    if ~n_elements(im) then case 1 of
      (size(ti,/n_dim) eq 2): begin
        nc=0
        im=make_array(type=size(ti,/type),[wid,hei])
      end
      else: begin
        nc=(size(ti,/dimensions))[0]
        im=make_array(type=size(ti,/type),[nc,wid,hei])
      end
    endcase
    if nc ge 1 then im[0,(c-bbt.col[0])*tw,(bbt.row[1]-r)*th]=ti else im[(c-bbt.col[0])*tw,(bbt.row[1]-r)*th]=ti
  endfor
endfor
if ~n_elements(im) then begin
  im=bytarr(256,256)
  nc=1
  imc=im
  found=0
endif else begin  ;crop possible leftover pixels
  imc=nc ge 1 ? im[*,0:bbt.lastcol,bbt.firstrow:hei-1] : im[0:bbt.lastcol,bbt.firstrow:hei-1]
  found=1
endelse

;apply colortable if there is one
if havecmap && (size(ti,/n_dim) eq 2) then begin
  iim=bytarr([4,size(imc,/dimensions)])
  l=pp_locate(imc)
  iv=0
  foreach ll,l,v do begin
    print,v,iv++
    if cmaph.haskey(v) then for ic=0,3 do iim[ll*4+ic]=cmaph[v,ic]
  endforeach
  
  imc=iim
endif

ret={im:imc,burl:burl,bbt:bbt}
return,ret
end

function pp_wmts::getcimage,bbi,layer=layer,time=time,dims=dims,imp=imp,mst=mst,$
  limit=limit,center_longitude=center_longitude,center_latitude=center_latitude,$
  convert_to_geographic=convgeo
compile_opt idl2,logical_predicate

bbi=n_elements(bbi) eq 4 ? bbi : [-180d0,-90d0,180d0,90d0]
dims=n_elements(dims) eq 2 ? dims : [800,800]

zl=self.autolevel(bbi,layer,dims,/crop)
imp=self.getimage(bbi,layer,zl,time=time,found=found,proj=proj,ms=ms,bbt=bbt)
ci=imp.bbt.cropinds
nc=size(imp.im,/n_dim)



mst={grid_units:2,image_location:bbi[[0,1]],image_dimensions:bbi[[2,3]]-bbi[[0,1]]}
limit=bbi[[1,0,3,2]]
center_longitude=mean(bbi[[0,2]])
center_latitude=mean(bbi[[1,3]])

if ~found then return,imp.im

ret=nc eq 2 ? imp.im[ci[0]:ci[1],ci[2]:ci[3]] : imp.im[*,ci[0]:ci[2],ci[1]:ci[3]]

if keyword_set(convgeo) && proj ne 'geographic' then begin
  ;print,proj
  msg=map_proj_init('equirectangular')
  range=bbt.range
  if nc eq 3 then begin
    ncols=(size(ret,/dimensions))[0]
    rti=map_proj_image(reform(ret[0,*,*]),range,image_structure=ms,map_structure=msg,/bilinear)
    rt=dblarr([ncols,size(rti,/dimensions)])
    rt[0,*,*]=rti
    for i=1,ncols-1 do rt[i,*,*]=map_proj_image(reform(ret[i,*,*]),range,image_structure=ms,map_structure=msg,/bilinear) 
  endif else rt=map_proj_image(ret,range,image_structure=ms,map_structure=msg)
  ret=byte(rt)
endif

return,ret 
end

pro pp_wmts__define
compile_opt idl2,logical_predicate
!null={pp_wmts,inherits IDL_Object,conturl:'',cap:obj_new(),tms:obj_new(),$
  layers:obj_new(),cache:0,verbose:0,nextday:0,lcorner:dblarr(2),ucorner:dblarr(2),crs:''}
end
