function pp_spectrumwidget::init,_ref_extra=rex
compile_opt idl2,hidden

;Initialize the base widget
ret=self->toplevelbase::init(_strict_extra=rex,row=2,/base_align_top)
self->getproperty,name=basename,geo=geo
;Create the widgets
coord=obj_new('catcoord',xrange=[0,9],yrange=[0,9])
draw=obj_new('drawwidget',self,xsize=geo.xsize-20,ysize=geo.ysize-50,name=basename+'_draw')
row2=obj_new('basewidget',self,row=1)
value_label=obj_new('labelwidget',row2,value='                  ',/dynamic_resize)
export_pixel_name=obj_new('fieldwidget',row2,title='Export to variable:',name=basename+'_export_name',/cr_events)
self->draw
self.draw=draw
return,ret
end

pro pp_spectrumwidget::eventhandler,event
compile_opt idl2,hidden
ename=strlowcase(event.name)
self->getproperty,name=basename
case ename of
  basename+'_export_name' : if (ptr_valid(self.spectrum_data)) then begin
    name=idl_validname(*event.value)
    if (name ne '') then begin
      data=*self.spectrum_data
      savetomain,data,name
    endif
  endif
  else:
endcase
end

pro pp_spectrumwidget::setproperty,spectrum_data=spectrum_data,_ref_extra=rex
compile_opt idl2,hidden
if (n_elements(spectrum_data) gt 0) then begin
  ptr_free,self.spectrum_data
  self.spectrum_data=ptr_new(spectrum_data)
  self.nspec=n_elements(spectrum_data)
endif
if (n_elements(rex) gt 0) then self->toplevelbase::setproperty,_strict_extra=rex
end

pro pp_spectrumwidget::getproperty,spectrum_data=spectrum_data,_ref_extra=rex,nspec=nspec
compile_opt idl2,hidden
if (arg_present(spectrum_data)) then spectrum_data=ptr_valid(self.spectrum_data) ? *self.spectrum_data : -1
if (arg_present(nspec)) then nspec=self.nspec
if (n_elements(rex) gt 0) then self->toplevelbase::getproperty,_strict_extra=rex
end

pro pp_spectrumwidget::plot
compile_opt idl2,hidden
if (self.nspec gt 0) then begin
  (*self.spectrum_data)[0].cube->getproperty,wavelengths=wavs
  wmin=min(wavs,max=wmax,/nan)
  vmin=min((*self.spectrum_data)[*].pixel.core,max=vmax,/nan)
  self.draw->setwindow
  plot,[wmin,wmax],[0d0,vmax*1.5],/nodata,xstyle=1
  for i=0,self.nspec-1 do oplot,wavs,(*self.spectrum_data)[i].pixel.core
endif
end

pro pp_spectrumwidget__define
compile_opt idl2
void={pp_spectrumwidget,inherits toplevelbase,spectrum_data:ptr_new(),draw:obj_new(),nspec:0}
end
