
function pp_cubewidget::init,parent,_ref_extra=rex,std=std
compile_opt idl2

self.backplane_mode=2B

;Initialize the base widget
ret=self->basewidget::init(parent,_strict_extra=rex,column=2,/base_align_left)
self->getproperty,name=basename,geo=geo

;Create the widgets
coord=obj_new('catcoord',xrange=[0,9],yrange=[9,0])
draw=obj_new('drawwidget',self,xsize=geo.xsize-240,ysize=geo.ysize-20,/motion_events,coord_object=coord,name=basename+'_draw',/button_events)
image=obj_new('scaleimage',bytarr(10,10),xsize=geo.xsize-240,ysize=geo.ysize-20)
draw->add,image,position=0,/use_coords
row2=obj_new('basewidget',self,column=1,/base_align_left)
row2col1=obj_new('basewidget',row2,column=1,/base_align_left,title='Band selection',frame=1)
void=obj_new('labelwidget',row2col1,value='Band selection')
slide=obj_new('sliderwidget',row2col1,minimum=0,maximum=std.bands-1,title='Band',name=basename+'_band',value=self.band,/drag)
wbandw=(*std.wavs)[self.band]
band_label=obj_new('labelwidget',row2col1,value=string(wbandw,format='(F7.4)')+' '+string(181B)+'m '+string(1d4/wbandw,format='(F7.2)')+' cm-1')
band_mode=obj_new('droplistwidget',row2col1,value=['Image','Contour','None'],index=self.band_mode,name=basename+'_band_mode',title='Display mode:')
row2col2=obj_new('basewidget',row2,column=1,/base_align_top,title='Backplane selection',frame=1)
void=obj_new('labelwidget',row2col2,value='Backplane selection')
backplane=obj_new('treewidget',row2col2,scr_ysize=90)
for i=0,std.nback-1 do void=obj_new('treewidget',backplane,value=(*std.bnames)[i],uvalue=i,name=basename+'_backplane')
backplane_mode=obj_new('droplistwidget',row2col2,value=['Image','Contour','None'],index=self.backplane_mode,name=basename+'_backplane_mode',title='Display mode:')
row2col3=obj_new('basewidget',row2,column=1,/base_align_left)
button_base=obj_new('basewidget',row2col3,/nonexclusive)
grid=obj_new('buttonwidget',button_base,value='Draw grid',name=basename+'_drawbutton')
grid->setproperty,set_button=self.grid
flip=obj_new('buttonwidget',button_base,value='Flip',name=basename+'_flipbutton')
flip->setproperty,set_button=self.flip
plot=obj_new('buttonwidget',button_base,value='Plot spectrum',name=basename+'_plotbutton')
plot->setproperty,set_button=self.plot
file_label=obj_new('labelwidget',row2col3,value='No cube selected',/dynamic_resize)
location_label=obj_new('labelwidget',row2col3,value='No cube selected                      ',/dynamic_resize)
geo_label=obj_new('labelwidget',row2col3,value='No cube selected                      ',/dynamic_resize)
export_base=obj_new('basewidget',row2col3,frame=1,column=1,/base_align_left)
void=obj_new('labelwidget',export_base,value='Export cube')
export_cube_name=obj_new('fieldwidget',export_base,title='Export to variable:',name=basename+'_export_name',/cr_events,/column)
export_cube_file=obj_new('buttonwidget',export_base,value='Export to cube file',name=basename+'_export_file')


self.std=ptr_new(std)
self.band_label=band_label
self.image=image
self.file_label=file_label
self.coord=coord
self.location_label=location_label
self.geo_label=geo_label
self.draw=draw
return,ret
end

pro pp_cubewidget::cleanup
compile_opt idl2,hidden
if (obj_valid(self.spectrum)) then obj_destroy,self.spectrum
self->basewidget::cleanup
end

pro pp_cubewidget::eventhandler,event
compile_opt idl2,hidden
ename=strlowcase(event.name)
self->getproperty,name=basename
case ename of
  basename+'_band' : begin
    self.band=event.value
    wbandw=(*(*self.std).wavs)[self.band]
    self.band_label->setproperty,value=string(wbandw,format='(F7.4)')+' '+string(181B)+'m '+$
      string(1d4/wbandw,format='(F7.2)')+' cm-1'
    if obj_valid(self.cube) then begin
      im=self.flip ? self.cube->getbandbyindex(self.band) : reverse(self.cube->getbandbyindex(self.band),2)
      ptr_free,self.band_im
      self.band_im=ptr_new(im)
      self.sz=size(im,/dimensions)
      if (self.band_mode eq 0) then begin
        immin=min(im,max=immax,/nan)
        self.image->setproperty,image=im,sclmin=immin,sclmax=immax,bottom=immin
      endif
    endif
  end
  basename+'_band_mode' : self.band_mode=event.index
  basename+'_backplane_mode' : self.backplane_mode=event.index
  basename+'_backplane' : begin
    event.id->getproperty,value=val
    self.backplane=val
    if obj_valid(self.cube) then begin
      im=self.flip ? self.cube->getsuffixbyname(self.backplane) : reverse(self.cube->getsuffixbyname(self.backplane),2)
      ptr_free,self.backplane_im
      self.backplane_im=ptr_new(im)
      self.sz=size(im,/dimensions)
      if (self.backplane_mode eq 0B) then begin
        immin=min(im,max=immax,/nan)
        self.image->setproperty,image=im,sclmin=immin,sclmax=immax,bottom=immin
      endif
    endif
  end
  basename+'_drawbutton' : begin
    self.grid=~self.grid
    event.id->setproperty,set_button=self.grid
  end
  basename+'_flipbutton' : begin
    self.flip=~self.flip
    event.id->setproperty,set_button=self.flip
  end
  basename+'_draw' : if (obj_valid(self.cube)) then begin
    value=self.image->pixel_to_value(event.x, event.y,inside=inside,xpixel=xpix,ypixel=ypix,xdata=xdata,ydata=ydata)
    self.coord->getproperty,yrange=yr,xrange=xr
    x=xpix+1 
    y=self.flip ? ypix+1 : yr[0]-ypix+1
    val='X: '+string(x,format='(I4)')+' Z: '+string(y,format='(I4)')$
     +' Value: '+string(value,format='(E12.4)')
    self.location_label->setproperty,value=val
;    val='Lat: '+string((*self.lat)[x-1,y-1],format='(F7.3)')+' Lon:'+string((*self.lon)[x-1,y-1],format='(F8.3)')$
;     +string(10B)+' Alt: '+string((*self.alt)[x-1,y-1],format='(E11.4)')
     val='Lat: '+string((*self.lat)[x-1,-y],format='(F7.3)')+' Lon:'+string((*self.lon)[x-1,-y],format='(F8.3)')$
     +string(10B)+' Alt: '+string((*self.alt)[x-1,-y],format='(E11.4)')
    self.geo_label->setproperty,value=val
    if (self.plot && ((event.clicks ne 0) || (event.press ne 0))) then begin
      if ~ptr_valid(self.mask) then begin
        mask=bytarr(3,xr,yr)
      endif else mask=*self.mask
      
      self.cube->getproperty,core=core,struct_backplanes=backplanes
      newdata={cube:self.cube,pixel:{core:reform(core[x-1,yim,*]),backplanes:backplanes[x-1,yim]}}
      if (event.press eq 4) then begin
        self.spectrum->getproperty,spectrum_data=olddata,nspec=nspec
        newdata=[olddata,newdata]
      endif
      self.spectrum->setproperty,spectrum_data=newdata
      self.spectrum->plot
      self.draw->setwindow
    endif
  endif
  basename+'_plotbutton' : begin
    if (obj_valid(self.spectrum)) then obj_destroy,self.spectrum else begin
      top=catgettopobject(self)
      top->getproperty,geometry=geo
      self.spectrum=obj_new('pp_spectrumwidget',scr_xsize=800,scr_ysize=300,name='spectrum',xoffset=geo.xoffset,yoffset=4*geo.yoffset+geo.ysize)
    endelse
    self.plot=obj_valid(self.spectrum)
    event.id->setproperty,set_button=self.plot
  end
  basename+'_export_name' : if (obj_valid(self.cube)) then begin
    name=idl_validname(*event.value)
    if (name ne '') then begin
      cube=self.cube
      savetomain,cube,name
    endif
  endif
  basename+'_export_file' : if (obj_valid(self.cube)) then begin
    self.cube->getproperty,file=cubefile
    self->getproperty,id=id
    file=dialog_pickfile(default_extension='.cub',filter=['*.cub','*.CUB','*.QUB','*.qub'],display_name='Export cube to file',$
     /overwrite_prompt,file=cubefile,dialog_parent=id,/write)
    if (file ne '') then self.cube->write,file
  endif
  else:
endcase
self->draw
if (obj_valid(self.cube)) then begin
  if (self.grid) then begin
    lat=self.cube->getsuffixbyname('LAT_0',found=afound)
    lon=self.cube->getsuffixbyname('LON_0',found=ofound)
    if (afound*ofound eq 0) then begin
      lat=self.cube->getsuffixbyname('LATITUDE')
      lon=self.cube->getsuffixbyname('LONGITUDE')
    endif
    lat=self.flip ? reverse(lat,2) : lat
    lon=self.flip ? reverse(lon,2) : lon
    contour,lat,/noerase,color=cgcolor('red'),xstyle=1,ystyle=1,nlevels=21,xmargin=[0,0],ymargin=[0,0]
    contour,lon,/noerase,color=cgcolor('red'),xstyle=1,ystyle=1,nlevels=21,xmargin=[0,0],ymargin=[0,0]
  endif
  if (self.band_mode eq 1B) && ptr_valid(self.band_im) then begin
    szcont=size(*self.band_im,/dimensions)
    xcont=dindgen(szcont[0])-0.5d0
    ycont=dindgen(szcont[1])-0.5d0
    sbim=self.flip ? reverse(*self.band_im,2) : *self.band_im
    contour,sbim,xcont,ycont,color=cgcolor('blue'),nlevels=21,xstyle=1,ystyle=1,/noerase,$
      xmargin=[0,0],ymargin=[0,0],xrange=[0,szcont[0]-1],yrange=[0,szcont[1]-1]
  endif
  if (self.backplane_mode eq 1B) && ptr_valid(self.backplane_im) then begin
   szcont=size(*self.backplane_im,/dimensions)
   xcont=dindgen(szcont[0])-0.5d0
   ycont=dindgen(szcont[1])-0.5d0
   sbim=self.flip ? reverse(*self.backplane_im,2) : *self.backplane_im
   contour,sbim,xcont,ycont,color=cgcolor('green'),nlevels=21,xstyle=1,ystyle=1,/noerase,$
     xmargin=[0,0],ymargin=[0,0],xrange=[0,szcont[0]-1],yrange=[0,szcont[1]-1]
  endif
endif
if (ename eq basename+'_band') && (event.drag ne 1) then begin ;Do not forward drag events
  self->getproperty,parents=par
  par->eventhandler,event
endif
end

pro pp_cubewidget::messagehandler,title,sender=sender,data=data
compile_opt idl2,hidden
self.cube=data
data->getproperty,file=file,lines=lines,samples=samples
self.file_label->setproperty,value='Cube: '+file
ptr_free,self.band_im
ptr_free,self.backplane_im
self.coord->setproperty,yrange=[lines-1,0],xrange=[0,samples-1]
ptr_free,self.lat,self.lon,self.alt
self.lat=ptr_new(reverse(self.cube->getsuffixbyname('LAT_0'),2))
self.lon=ptr_new(reverse(self.cube->getsuffixbyname('LON_0'),2))
self.alt=ptr_new(reverse(self.cube->getsuffixbyname('ALT_0'),2))

end



pro pp_cubewidget__define
compile_opt idl2
void={pp_cubewidget,inherits basewidget,band:0L,backplane:'',std:ptr_new(),band_mode:0B,$
 backplane_mode:0B,grid:0B,band_label:obj_new(),image:obj_new(),cube:obj_new(),file_label:obj_new(),$
 band_im:ptr_new(),backplane_im:ptr_new(),coord:obj_new(),location_label:obj_new(),plot:0B,spectrum:obj_new(),draw:obj_new(),$
 lat:ptr_new(),lon:ptr_new(),alt:ptr_new(),geo_label:obj_new(),flip:0B,mask:ptr_new(),sz:lonarr(2),$
 masked:0L}
end
