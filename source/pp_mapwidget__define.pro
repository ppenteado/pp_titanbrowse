
function pp_mapwidget::init,parent,_ref_extra=rex,maps=maps,dbobject=dbobject
compile_opt idl2,hidden

self.dbobject=dbobject

;Initialize map parameters
self.projs=ptr_new(['Equirectangular','Near Side Perspective','Orthographic','Stereographic','Mollweide',$
  'Sinusoidal','Robinson','Goode'])
self.proj_inds=ptr_new([117,115,114,110,125,116,121,124])
self.iso=ptr_new([0,1,1,1,0,0,0,0])
self.bmap=1
self.height=2.
self.minim=0d0 & self.maxim=100d0

;Create the widgets
ret=self->basewidget::init(parent,_strict_extra=rex,row=2,/base_align_top)
self->getproperty,name=basename,geo=geo
coord=obj_new('mapcoord',117)
coord->getproperty,map_structure=map
coord->setproperty,xrange=map.uv_box[[0,2]],yrange=map.uv_box[[1,3]]
coord->getproperty,map_structure=map
self.mapstructure=map
grid=obj_new('map_grid',map_object=coord,color='red',thick=2.)
draw=obj_new('drawwidget',self,xsize=geo.xsize-20,ysize=geo.ysize-200,coord_object=coord,/button_events,name=basename+'_drawwidget')
image=obj_new('catimage',bytarr(10,10),xsize=geo.xsize,ysize=geo.ysize-200,/keep_aspect,display=1);,sclmin=0B,sclmax=255B,bottom=0B,scaletype=0)
draw->add,image,position=0,/use_coords
draw->add,grid,position=1
row2=obj_new('basewidget',self,row=1,/base_align_top)
row2col1=obj_new('basewidget',row2,column=1,/base_align_left)
droplist=obj_new('droplistwidget',row2col1,value=['Cube location','Selected pixels location','Selected pixels function'],name=basename+'_mode',title='Display mode:')
void=obj_new('droplistwidget',row2col1,value=['Center','Corners'],name=basename+'_precision',title='Pixel location:')
void=obj_new('buttonwidget',row2col1,value='Export to bitmap file',name=basename+'_export_file')
row2col1_checkbox=obj_new('basewidget',row2col1,/nonexclusive)
void=obj_new('buttonwidget',row2col1_checkbox,value='Draw only pixels on surface',name=basename+'_surface_only')
;Map control widgets
mapcont=obj_new('basewidget',row2,/base_align_top,frame=1,title='Map controls',row=3)
proj=obj_new('droplistwidget',mapcont,value=*self.projs,title='Projection:',name=basename+'_projection',index=self.proj)

;Load background maps
nmaps=n_elements(maps)
smaps=replicate({title:'',range:[-180d0,-90d0,180d0,90d0],image:ptr_new()},nmaps)
smaps.title=maps
;for i=0,nmaps-1 do if (maps[i].file ne '') then mapimages[i]=ptr_new(read_image(maps[i].file))
;self.mapimages=ptr_new(mapimages,/no_copy)
for i=0,nmaps-1 do if (maps[i] ne 'none') then begin ;Get the map metadata from the tiff file
  void=query_tiff(maps[i],geotiff=geo,info)
  smaps[i].title=info.description
  latmax=max(geo.modeltiepointtag[4,*],min=latmin)
  lonmax=max(geo.modeltiepointtag[3,*],min=lonmin)
  smaps[i].range=[lonmin,latmin,lonmax,latmax]
  smaps[i].image=ptr_new(read_tiff(maps[i]))
endif

background=obj_new('droplistwidget',mapcont,value=smaps.title,title='Background:',name=basename+'_background',index=self.bmap)
row3=obj_new('basewidget',mapcont,row=1)
lat=obj_new('sliderwidget',row3,maximum=90.0,minimum=-90.0,title='Center latitude',name=basename+'_latitude',value=self.lat)
lon=obj_new('sliderwidget',row3,maximum=180.0,minimum=-180.0,title='Center longitude',name=basename+'_longitude',value=self.lon)
height=obj_new('fieldwidget',row3,value=self.height,title='Height:',name=basename+'_height',/cr_events)
row4=obj_new('basewidget',mapcont,row=1)
minim=obj_new('sliderwidget',row4,maximum=100.0,minimum=0.0,title='Imagem minimum',name=basename+'_minim',value=self.minim)
maxim=obj_new('sliderwidget',row4,maximum=100.0,minimum=0.0,title='Image maximum',name=basename+'_maxim',value=self.maxim)

self.maps=ptr_new(smaps,/no_copy)
;self.mapimages=ptr_new(mapimages,/no_copy)

self.latslider=lat
self.lonslider=lon
self.image=image
self.draw=draw
self.others=row2
self.coord=coord
self.grid=grid
self.mode=droplist
;self.maps=ptr_new(maps)
return,ret
end

pro pp_mapwidget::update
compile_opt idl2,hidden
h=self.height*self.mapstructure.a
self.mapstructure=self.coord->setmapprojection((*self.proj_inds)[self.proj],center_latitude=self.lat,$
 center_longitude=self.lon,height=h,semimajor=5d6,semiminor=5d6,/relaxed)
uvbox=self.mapstructure.uv_box
aspect=(uvbox[3]-uvbox[1])/(uvbox[2]-uvbox[0])
self.coord->setproperty,xrange=uvbox[[0,2]],yrange=uvbox[[1,3]]
self.draw.getproperty,geo=geo
;xsz=400 & ysz=400
xsz=geo.xsize & ysz=geo.ysize
dim=(*self.iso)[self.proj] ? [xsz<ysz,ysz<xsz] : [xsz<(2d0*ysz),ysz<(xsz/2d0)]
if (self.bmap ne 0) && (~self.pixel_function) then begin
  dim=(*self.iso)[self.proj] ? [xsz<ysz,ysz<xsz] : [xsz<(2d0*ysz),ysz<(xsz/2d0)]
  image=(*((*self.maps)[self.bmap]).image)
  sz=size(image,/n_dimensions)
  if (sz eq 2) then self.background=ptr_new(map_proj_image(image,$
   ((*self.maps)[self.bmap]).range,map_structure=self.mapstructure,dimensions=dim)) $
    else begin
      im=bytarr(3,dim[0],dim[1],/nozero)
      for i=0,2 do im[i,*,*]=map_proj_image(reform(image[i,*,*]),$
   ((*self.maps)[self.bmap]).range,map_structure=self.mapstructure,dimensions=dim)
      self.background=ptr_new(im,/no_copy)
    endelse
  self.image->setproperty,xsize=dim[0],ysize=dim[1],xstart=(xsz-dim[0])/2d0,ystart=(ysz-dim[1])/2d0
  self.image->setproperty,image=*self.background
  self.draw->draw,/erase
;  self.draw->resize,dim[0],dim[1]
endif else begin
  self.image->setproperty,image=bytarr(10,10)
  if (~self.pixel_function) then self.draw->draw,/erase
endelse
if (self.cube && ptr_valid(self.data)) then begin
  ;self.draw->draw,/erase
  plot,self.mapstructure.uv_box[[0,2]]*xsz/dim[0],self.mapstructure.uv_box[[1,3]]*ysz/dim[1],/nodata,xstyle=5,ystyle=5,/noerase,xmargin=[0,0],ymargin=[0,0]
  if (self.precision) then begin
    sz=size((*self.data).lons)
    nd=sz[0]
    nel=sz[nd+2]
    np=nd eq 3 ? sz[1]*sz[2] : sz[1]
    lats=reform(transpose((*self.data).lats),nel)
    lons=reform(transpose((*self.data).lons),nel)
    if (self.surfaceonly) then begin 
      alts=reform(transpose((*self.data).alts),nel)
      w=where(alts gt 0.,nw)
      if (nw gt 0L) then begin
        lats[w]=!values.f_nan
        lons[w]=!values.f_nan
      endif
    endif
    xy=map_proj_forward(-lons,lats,map_structure=self.mapstructure)
    for i=0L,np-1L do oplot,xy[0,i*5:(i+1)*5-1],xy[1,i*5:(i+1)*5-1],color=fsc_color('blue')
  endif else begin
    sz=size((*self.data).clons)
    nd=sz[0]
    nel=sz[nd+2]
    lats=(*self.data).clats
    lons=(*self.data).clons
    if (self.surfaceonly) then begin
      alts=(*self.data).calts
      w=where(alts gt 0.,nw)
      if (nw gt 0L) then begin
        lats[w]=!values.f_nan
        lons[w]=!values.f_nan
      endif
    endif
    xy=map_proj_forward(-lons,lats,map_structure=self.mapstructure)
    if (nd eq 1) then oplot,xy[0,*],xy[1,*],color=fsc_color('green') else begin
      lats=reform(reform(xy[1,*]),sz[1],sz[2])
      lons=reform(reform(xy[0,*]),sz[1],sz[2])
      for i=0L,sz[1]-1L do begin
        w=where(finite(reform(lats[i,*])),nw)
        if (nw gt 0) then oplot,lons[i,w],lats[i,w],psym=nw gt 0 ? 0 : 6,color=fsc_color('green')
      endfor
      for i=0L,sz[2]-1L do begin
        w=where(finite(lats[*,i]),nw)
        if (nw gt 0) then oplot,lons[w,i],lats[w,i],psym=nw gt 0 ? 0 : 6,color=fsc_color('green')
      endfor
    endelse
  endelse
endif
if (self.selected_pixels && ptr_valid(self.data)) then begin
  ;self.draw->draw,/erase
  plot,self.mapstructure.uv_box[[0,2]]*xsz/dim[0],self.mapstructure.uv_box[[1,3]]*ysz/dim[1],/nodata,xstyle=5,ystyle=5,/noerase,xmargin=[0,0],ymargin=[0,0]
  lats=(*self.data)[*].backplanes.lat_0
  lons=(*self.data)[*].backplanes.lon_0
  alts0=(*self.data)[*].backplanes.alt_0
  alts1=(*self.data)[*].backplanes.alt_1
  alts2=(*self.data)[*].backplanes.alt_2
  alts3=(*self.data)[*].backplanes.alt_3
  alts4=(*self.data)[*].backplanes.alt_4
  w=where(not (alts0 or alts1 or alts2 or alts3 or alts4))
  lats=lats[w] & lons=lons[w]
  xy=map_proj_forward(lons,lats,map_structure=self.mapstructure)
  oplot,xy[0,*],xy[1,*],psym=7,color=fsc_color('blue'),thick=2.
endif
if (self.pixel_function && ptr_valid(self.data)) then begin
 ; plot,self.mapstructure.uv_box[[0,2]]*xsz/dim[0],self.mapstructure.uv_box[[1,3]]*ysz/dim[1],/nodata,xstyle=5,ystyle=5,$
   ; /noerase,xmargin=[0,0],ymargin=[0,0]
  lats=(*self.data)[*].backplanes.lat_0
  lons=(*self.data)[*].backplanes.lon_0
  ;xy=map_proj_forward(lons,lats,map_structure=self.mapstructure)
  ;oplot,xy[0,*],xy[1,*],psym=7,color=fsc_color('blue'),thick=2.
  eval=*self.eval
  alt0=eval.pixdata.backplanes.alt_0
  alt1=eval.pixdata.backplanes.alt_1
  alt2=eval.pixdata.backplanes.alt_2
  alt3=eval.pixdata.backplanes.alt_3
  alt4=eval.pixdata.backplanes.alt_4
  w=where(not (alt0 or alt1 or alt2 or alt3 or alt4),count)
  lons=lons[w]
  lats=lats[w]
  vals=eval[w].val
  range=pp_quartile(vals,[self.minim/100d0,self.maxim/100d0])
  triangulate,lons,lats,tr,b,/degrees,sphere=sph,fvalue=vals
  nvals=trigrid(vals,sphere=sph,[0.5,0.5],[-180.,-90.,180.,90.],/degrees,missing=!values.d_nan,xgrid=lonout,ygrid=latout)
  pvals=map_proj_image(nvals,[-180.,-90.,180.,90.],map_structure=self.mapstructure,dimensions=dim)
  pvals=bytscl(range[0]>pvals<range[1])
  ;print,range,minmax(nvals)
  self.image->setproperty,xsize=dim[0],ysize=dim[1],xstart=(xsz-dim[0])/2d0,ystart=(ysz-dim[1])/2d0
  self.image->setproperty,image=pvals
  self.draw->draw,/erase
  self.pvals=ptr_new(pvals)

  ;self.background=ptr_new(pvals)
  ;self.image->setproperty,image=bytarr(10,10)
;  self.draw->draw,/erase
  mps=self.mapstructure
  ;save,file='test.sav'
endif
end

pro pp_mapwidget::draw,_ref_extra=ref
compile_opt idl2,hidden
self.others->draw
self->update
self.draw->draw,/erase
self->update
end


pro pp_mapwidget::eventhandler,event
compile_opt idl2,hidden
ename=strlowcase(event.name)
self->getproperty,name=basename
case ename of
  basename+'_drawwidget' : begin
    if event.press then begin
      self.dragstart=[event.x,event.y]
      return
    endif
    if event.release then begin 
      self.dragend=[event.x,event.y]
      drag=self.dragend-self.dragstart
      lendrag=sqrt(total(drag^2))
      self.draw.getproperty,xsize=dxsz,ysize=dysz
      radius=min([dxsz,dysz])
      drag/=radius
      if lendrag then begin
         ;print,drag
         ;print,self.lon,self.lat
         nlon=-180d0>(self.lon-(180d0-self.lon)*drag[0])<180d0
         nlat=-90d0>(self.lat-(90d0-self.lat)*drag[1])<90d0
         ;print,nlon,nlat
         self.lon=nlon
         self.lat=nlat
         self.latslider.setproperty,value=self.lat
         self.lonslider.setproperty,value=self.lon
      endif else return
    endif
  end
  basename+'_projection' : self.proj=event.index
  basename+'_background' : self.bmap=event.index
  basename+'_precision' : self.precision=event.index
  basename+'_mode' : begin
    self.cube=event.index eq 0
    self.selected_pixels=event.index eq 1
    self.pixel_function=event.index eq 2
    ptr_free,self.data
    if (self.selected_pixels || self.pixel_function) then begin ;Obtain the pixel data 
      self.dbobject->getproperty,nselpixels=npix
      widget_control,/hourglass
      if (npix gt 0L) then self.data=ptr_new(self.dbobject->getselectedpixels())
      if (npix gt 0L) && (self.pixel_function) then self.eval=ptr_new(self.dbobject.evalres)
    endif
    if self.cube && self.cubesdata then self.data=ptr_new(*self.cubesdata)
  end
  basename+'_longitude' : self.lon=event.value
  basename+'_latitude' : self.lat=event.value
  basename+'_minim' : self.minim=event.value
  basename+'_maxim' : self.maxim=event.value
  basename+'_height' : if (*event.value gt 1d-3) then self.height=(*event.value) else begin
    self.height=1d-3
    id->setproperty,value=1d-3
  endelse
  basename+'_export_file' : begin
    self->getproperty,id=id
    file=dialog_pickfile(default_extension='.png',filter=['*.png','*.PNG','*.jpg','*.jpeg','*.JPG','*.JPEG','*.ps','*.PS'],$
     display_name='Export map bitmap to file',$
     /overwrite_prompt,file='mapwidget_bitmap',dialog_parent=id,/write)
    tmp=strsplit(file,'.',/extract)
    type=strupcase(tmp[n_elements(tmp)-1])
    if (type eq 'PS') then type='POSTSCRIPT'
    if (file ne '') then self.draw->output,filename=file,/nodialog,type=type
  end
  basename+'_surface_only' : self.surfaceonly=1-self.surfaceonly
  else:
endcase
self->update
;self->draw
self->getproperty,parents=par
par->eventhandler,event
end

pro pp_mapwidget::getproperty,cube=cube,selected_pixels=selected_pixels,pixel_function=pixel_function,_ref_extra=rex
compile_opt idl2,hidden
if (arg_present(cube)) then cube=self.cube
if (arg_present(selected_pixels)) then cube=self.selected_pixels
if (arg_present(pixel_function)) then cube=self.pixel_function
if (n_elements(rex) gt 0) then self->basewidget::getproperty,_strict_extra=rex
end

pro pp_mapwidget::messagehandler,title,sender=sender,data=data
compile_opt idl2,hidden
if (title eq 'cubeselected') then begin
  self.cube=1B & self.selected_pixels=0B & self.pixel_function=0B
  self.mode->setproperty,index=0
  ptr_free,self.data
  self.data=ptr_new({lats:data->getsuffixbyname(['LAT_1','LAT_2','LAT_3','LAT_4','LAT_1']),$
   lons:(-(data->getsuffixbyname(['LON_1','LON_2','LON_3','LON_4','LON_1'])-360d0 mod 360)),$
   alts:data->getsuffixbyname(['ALT_1','ALT_2','ALT_3','ALT_4','ALT_1']),$
   clats:data->getsuffixbyname('LAT_0'),clons:(-(data->getsuffixbyname('LON_0')-360d0 mod 360)),$
   calts:data->getsuffixbyname('ALT_0')})
  self.cubesdata=ptr_new(*self.data)
  self->update
endif
end

pro pp_mapwidget__define
compile_opt idl2
void={pp_mapwidget,inherits basewidget,draw:obj_new(),data:ptr_new(),cube:0B,$
 selected_pixels:0B,pixel_function:0B,mapimages:ptr_new(),lat:0.,lon:0.,proj:0,bmap:0,$
 projs:ptr_new(),mapstructure:!map,background:ptr_new(),image:obj_new(),others:obj_new(),$
 clean:0B,iso:ptr_new(),coord:obj_new(),grid:obj_new(),proj_inds:ptr_new(),height:0.,$
 mode:obj_new(),maps:ptr_new(),precision:0B,cubedata:obj_new(),dbobject:obj_new(),surfaceonly:0B,$}
 eval:ptr_new(),pvals:ptr_new(),minim:0d0,maxim:100d0,dragstart:[0L,0L],dragend:[0L,0L],$
 latslider:obj_new(),lonslider:obj_new(),cubesdata:ptr_new()}
 
end
