; docformat = 'rst'
;+
; :Uses: pp_editablecube__define, pp_getcubeheadervalue, pp_extractfields, pp_setcubeheadervalue, pp_readcube__define,
;   pp_cubecollection__define, pp_titanbrowse_metadb__define, pp_titanbrowse_db__define, pp_titanbrowse__define
;   pp_mapwidget__define,pp_cubewidget__define,pp_spectrumwidget__define.
; 
; Requires Catalist library, Coyote library (http://www.dfanning.com/catalyst/howtoinstall.html),
;    issmap_2009.tiff, issmap_201506.tiff, vimsmap_2009.tiff
;    
; :Version: 20151020
;
; :Author: Paulo Penteado (pp.penteado@gmail.com)
;-

function pp_titanbrowse_gui::init,mdbfiles,vis=vis,_ref_extra=_ex
compile_opt idl2,logical_predicate
self.version='20151020'
;Initialize db
self.db=obj_new('pp_titanbrowse',mdbfiles,vis=vis,_strict_extra=_ex)
self.db->getproperty,version=dbversion
if (~obj_valid(self.db)) then return,0
;Set up GUI
;Default window dimension
DEFXSZ=800
DEFYSZ=700
self.DEFXSZ=DEFXSZ
self.DEFYSZ=DEFYSZ
ret=self->toplevelbase::init(/scroll,/size_events,$
 x_scroll_size=DEFXSZ,y_scroll_size=DEFYSZ,name='tlb',$
 title='pp_titanbrowse_gui gui'+self.version+' db'+dbversion);,mbar=mbar) ;Top-level widget
;edit=obj_new('buttonwidget',mbar,/menu,value='Edit')
;copy=obj_new('buttonwidget',edit,value='Copy',accelerator='Alt+C',name='copy')
;Define tabs
tlb=self
tabw=obj_new('pp_titanbrowse_gui_tabwidget',tlb,scr_xsize=DEFXSZ,scr_ysize=DEFYSZ)
tlb->registerformessage,tabw,'resize'
tab1=obj_new('basewidget',tabw,row=1,/base_align_top,name='main_tab',title='Main')
tab2=obj_new('basewidget',tabw,column=1,name='cube_sel_tab',title='Cube selection')
tab3=obj_new('basewidget',tabw,column=1,name='pixel_sel_tab',title='Pixel selection')
;Fourth tab
;maps=replicate({name:'',file:'',range:[-180d0,-90d0,180d0,90d0]},3)
rpath=file_dirname(routine_filepath('pp_titanbrowse_gui__define'),/mark_directory)
;maps[1].name='ISS 2007' & maps[1].file=rpath+'issmap_200710.tiff' & maps[1].range=[-180d0,-90d0,180d0,71d0]
;maps[2].name='VIMS (Barnes 2009)' & maps[2].file=rpath+'vimsmap_2009.tiff' & maps[2].range=[-181d0,-90d0,181d0,90d0] 
;maps[0].name='none'
maps=['none',rpath+'issmap_201506.tiff',rpath+'vimsmap_2009.tiff',rpath+'issmap_2009.tiff']
tab4=obj_new('pp_mapwidget',tabw,name='map',title='Map',scr_xsize=DEFXSZ,scr_ysize=DEFYSZ,maps=maps,dbobject=self.db)
tlb->registerformessage,tab4,'cubeselected'
;Fifth tab
self.db->getproperty,std=std
tab5=obj_new('pp_cubewidget',tabw,name='cube',title='Cube',scr_xsize=DEFXSZ,scr_ysize=DEFYSZ,std=*std)
tlb->registerformessage,tab5,'cubeselected'
;First tab
;File tree
tree=obj_new('pp_titanbrowse_gui_treewidget',tab1,/folder,xsize=DEFXSZ*0.4,ysize=DEFYSZ*0.95)
tlb->registerformessage,tree,'resize'
self.db->getproperty,mdbfiles=mdbfiles
mdbfiles=file_basename(mdbfiles,'.sav')
for i=0,n_elements(mdbfiles)-1 do void=obj_new('treewidget',tree,value=mdbfiles[i],$
 name='cube_tree',uvalue=i,/folder)
tab1col2=obj_new('basewidget',tab1,column=1,/base_align_left)
;Cube information text
text=obj_new('pp_titanbrowse_gui_textwidget',tab1col2,name='cube_info',$
 value='Cube information',scr_xsize=DEFXSZ*0.5,scr_ysize=DEFYSZ*0.6,frame=1,/scroll)
tlb->registerformessage,text,'resize'
tlb->registerformessage,text,'cube_select'
export_base=obj_new('basewidget',tab1col2,frame=1,column=1,/base_align_left)
void=obj_new('labelwidget',export_base,value='Export cube information')
export_cubeinfo_name=obj_new('fieldwidget',export_base,title='Export to variable:',name='export_name',/cr_events,/column)
export_cubeinfo_file=obj_new('buttonwidget',export_base,value='Export to text file',name='export_file')
;Second tab
cubesel=obj_new('pp_titanbrowse_gui_selectorwidget',tab2,/cube,name='cubeselector',xsize=DEFXSZ-25,ysize=DEFYSZ-40,frame=1,db=self.db)
;Third tab
pixelsel=obj_new('pp_titanbrowse_gui_selectorwidget',tab3,/pixel,name='pixelselector',xsize=DEFXSZ-25,ysize=DEFYSZ-40,frame=1,std=*std,db=self.db)
cubesel->registerformessage,pixelsel,'updatelist'
cubesel->registerformessage,cubesel,'updatelist'
pixelsel->registerformessage,cubesel,'updatelist'
pixelsel->registerformessage,pixelsel,'updatelist'
tlb->registerformessage,cubesel,'functionreplace'


self.selected_cube=[-1,-1]
self.cubeinfo_text=text
;Update sizes of widgets that need it
tlb->getproperty,geometry=geo
tlb->sendmessage,'resize',data=[geo,geo]
tlb->draw
tab4->update
tab4->draw
return,ret
end

pro pp_titanbrowse_gui::cleanup
compile_opt idl2,hidden,logical_predicate
obj_destroy,self.db
self->toplevelbase::cleanup
end

pro pp_titanbrowse_gui::eventhandler,event
;Single handler for every event 
compile_opt idl2,hidden,logical_predicate
;print,'event:'
;help,event,/structure
ename=strlowcase(event.name)
switch ename of
  'tlb': begin ;Resize event
    self->getproperty,geometry=oldgeo
    self->setproperty,xsize=event.x,ysize=event.y,scr_xsize=event.x,scr_ysize=event.y
    self->getproperty,geometry=newgeo
    self->sendmessage,'resize',data=[newgeo,oldgeo] ;Tell everybody who needs to be resized
    self->draw
    break
  end
  'cube_tree': begin ;Top level tree events (files)
    event.id->getproperty,uvalue=uv,expanded=exp
    if (exp) or ((event.event_name eq 'WIDGET_TREE_SEL')&&(event.clicks eq 2)) then begin
      if (size(uv,/type) ne 7) then begin
        self.db->getproperty,odb=odb
        cmd=*(odb[uv]->getcmd())
        revs=cmd.rev[uniq(cmd.rev)]
        irevs=n_elements(revs) gt 1 ? value_locate(revs,cmd.rev) : replicate(0,size(cmd.rev,/dim))
        h=histogram(irevs,binsize=1,reverse_indices=ri)
        for i=0,n_elements(h)-1 do void=obj_new('treewidget',event.id,value=revs[i],$
          uvalue={pcmd:ptr_new(cmd),ri:ri[ri[i]:ri[i+1]-1],file:uv,odb:odb[uv]},name='cube_tree_rev',/folder)
        event.id->setproperty,uvalue=revs
      endif
      if ~exp then event.id->setproperty,expanded=1
    endif
    break
  end
  'cube_tree_rev': begin ;Secoond level tree events (revs)
    event.id->getproperty,uvalue=uv,expanded=exp
    if (exp) or ((event.event_name eq 'WIDGET_TREE_SEL')&&(event.clicks eq 2)) then begin
      if (size(uv,/type) ne 7) then begin
        titles=(*uv.pcmd).seq_title[uv.ri]
        s=sort(titles)
        utitles=titles[uniq(titles,s)]
        ititles=n_elements(utitles) gt 1 ? value_locate(utitles,titles) : replicate(0,size(titles,/dim))
        h=histogram(ititles,binsize=1,reverse_indices=ri)
        for i=0,n_elements(h)-1 do void=obj_new('treewidget',event.id,value=utitles[i],$
         uvalue={ri:uv.ri[ri[ri[i]:ri[i+1]-1]],ind:i,odb:uv.odb,file:uv.file},name='cube_tree_seqt',/folder)
        event.id->setproperty,uvalue=utitles
      endif
      if ~exp then event.id->setproperty,expanded=1
    endif
    break
  end
  'cube_tree_seqt': begin ;Third level tree events (seq_titles)
    event.id->getproperty,uvalue=uv,expanded=exp
    if (exp) or ((event.event_name eq 'WIDGET_TREE_SEL')&&(event.clicks eq 2)) then begin
      if (size(uv,/type) ne 7) then begin
        cubes=(uv.odb->filenames())[uv.ri]
        for i=0L,n_elements(cubes)-1 do void=obj_new('treewidget',event.id,value=cubes[i],$
         uvalue={ind:uv.ri[i],file:uv.file},name='cube_tree_cube')
        event.id->setproperty,uvalue=cubes
      endif
      if ~exp then event.id->setproperty,expanded=1
    endif
    break
  end
  'cube_tree_cube': begin ;Fourth level tree events (cubes)
    event.id->getproperty,uvalue=uv,select=select
    if (select) then begin
      self.selected_cube=[uv.file,uv.ind]
      self->sendmessage,'cube_select',data=self
      if (event.clicks eq 2) then begin
        cubeinfo=self.db->getcubeinfo(file=uv.file,cube_index=uv.ind)
        self->sendmessage,'functionreplace',data="cmd.prod_id eq '"+cubeinfo.prod_id+"'"
        self.db->getproperty,odb=odb
        self->sendmessage,'cubeselected',data=odb[uv.file]->getcube(uv.ind)
        ptr_free,self.cubeinfo
        self.cubeinfo=ptr_new(cubeinfo)
      endif
    endif
    break
  end
  'pixel_button' : ;Fall through to be handled by the next clause
  'cube_button': begin
    event.id->getproperty,uvalue=uv
    widget_control,/hourglass
    case uv of 
    'clear' : if (ename eq 'cube_button') then self.db->selectcubes,/none else self.db->selectpixels,/none 
    'all' : if (ename eq 'cube_button') then self.db->selectcubes,/all else self.db->selectpixels,/all
    'propagate' : if (ename eq 'cube_button') then self.db->selectpixels,/all else self.db->selectcubes,/pixel
    'freezeswitch' : begin
      event.handler->getproperty,frozen=frozen
      event.handler->setproperty,frozen=~frozen
    end
    'histogram' : begin
      histodata=dist(256)
        if event.name eq 'pixel_button' then begin
          eval=(self.db).evalres
          if n_elements(eval) then begin
            histodata=eval.val
            alt0=eval.pixdata.backplanes.alt_0
            alt1=eval.pixdata.backplanes.alt_1
            alt2=eval.pixdata.backplanes.alt_2
            alt3=eval.pixdata.backplanes.alt_3
            alt4=eval.pixdata.backplanes.alt_4
            w=where(not (alt0 or alt1 or alt2 or alt3 or alt4),count)
            histodata=histodata[w]
          endif
        endif else begin
          histodata=(self.db).cubeevalres
        endelse
        ;title=event.name eq 'pixel_button' ? self.lastpixexpr : self.lastcubeexpr
        minmaxh=minmax(histodata,/nan)
        meanh=mean(histodata,/nan)
        stddevh=stddev(histodata,/nan)
        title='Min: '+strtrim(minmaxh[0],2)+' Max: '+strtrim(minmaxh[1],2)+' Mean: '+strtrim(meanh,2)+$
         ' Stddev: '+strtrim(stddevh,2)
        if ~obj_valid(self.histowindow) then begin
          pname=(strsplit(event.name,'_',/extract))[0]
          cgwindow,wtitle='pp_titanbrowse histogram ('+pname+' values)';,'cghistoplot',histodata,$
            ;title=title,histdata=histdata
          wid=cgquery(/current,objectref=obj)
          self.histowindow=obj
        endif; else begin
          cgset,self.histowindow,/object
          cghistoplot,histodata,histdata=histdata,/window,title=title,/nan,/fillpolygon
          cghistoplot,histodata,histdata=histdata,/nan,/fillpolygon
          cgshow,self.histowindow,/object
        ;endelse
        minmaxhd=minmax(histdata)
        cgwindow,'cgplot',[meanh,meanh],minmaxhd,/over,/addcmd,linestyle=1
        cgwindow,'cgplot',[minmaxh[0],minmaxh[0]],minmaxhd,/over,/addcmd
        cgwindow,'cgplot',[minmaxh[1],minmaxh[1]],minmaxhd,/over,/addcmd
        quarts=pp_quartile(histodata,[0.05,0.95])
;        cgwindow,'cgplot',[quarts[0],quarts[0]],minmaxhd,/over,/addcmd,linestyle=1
;        cgwindow,'cgplot',[quarts[1],quarts[1]],minmaxhd,/over,/addcmd,linestyle=1
;        print,meanh,quarts
     end
    endcase
;Update own widget and the other with the new selection
    event.handler->sendmessage,'updatelist',data=self.db
    break
  end
  'pixelselector_functext' : ;Fall through to be handled by the next clause
  'cubeselector_functext' : begin
    widget_control,/hourglass
    event.id->getproperty,value=val
    ev=(strmid(val,0,1) eq '=')
    if (ename eq 'cubeselector_functext') then begin
      if ev then begin
        pexpr=self.db->parseexpr(strmid(val,1),/cube)
        widget_control,/hourglass
        !null=self.db.evalexpr(pexpr,/store,/cube)
        self.lastcubeexpr=strmid(val,1)
      endif else begin
        pexpr=self.db->parseexpr(val,/cube)
        widget_control,/hourglass
        self.db->selectcubes,pexpr,count=count
      endelse
    endif else begin
      if ev then begin
        pexpr=self.db->parseexpr(strmid(val,1),/pixel)
        widget_control,/hourglass
        !null=self.db.evalexpr(pexpr,/store)
        self.lastpixexpr=strmid(val,1)
      endif else begin
        pexpr=self.db->parseexpr(val,/pixel)
        widget_control,/hourglass
        self.db->selectpixels,pexpr,count=count
      endelse
    endelse
    event.handler->sendmessage,'updatelist',data=self.db
    break
  end
  'export_name' : if (ptr_valid(self.cubeinfo)) then begin
    name=idl_validname(*event.value)
    if (name ne '') then begin
      cubeinfo=*self.cubeinfo
      savetomain,cubeinfo,name
    endif
    break
  endif
  'export_file' : if (obJ_valid(self.cubeinfo_text)) then begin
    deffile=file_basename((*self.cubeinfo).cubefile,'.cub')+'_cubeinfo.txt'
    self->getproperty,id=id
    file=dialog_pickfile(default_extension='.txt',filter=['*.txt','*.TXT'],display_name='Export cube to file',$
     /overwrite_prompt,dialog_parent=id,/write,file=deffile)
    if (file ne '') then begin
      self.cubeinfo_text->getproperty,value=val
      openw,unit,file,/get_lun
      printf,unit,val,format='(A0)'
      free_lun,unit
    endif
  endif
  else:
  endswitch
end

pro pp_titanbrowse_gui_tabwidget::messagehandler,title,sender=sender,data=data
;Called when resizing is needed
;Call to draw is left to the sender
compile_opt idl2,hidden,logical_predicate
self->getproperty,scr_xsize=sxs,scr_ysize=sys
xb=data[1].xsize-sxs & yb=data[1].ysize-sys
self->setproperty,scr_xsize=data[0].xsize-xb,scr_ysize=data[0].ysize-yb
end

pro pp_titanbrowse_gui_treewidget::messagehandler,title,sender=sender,data=data
;Called when resizing is needed
;Call to draw is left to the sender
compile_opt idl2,hidden,logical_predicate
self->getproperty,scr_xsize=sxs,scr_ysize=sys
xr=(1d0*sxs)/data[1].xsize & yb=data[1].ysize-sys
self->setproperty,scr_xsize=data[0].xsize*xr,scr_ysize=data[0].ysize-yb
end

pro pp_titanbrowse_gui_textwidget::messagehandler,title,sender=sender,data=data
;Called when resizing is needed
;Call to draw on resize is left to the sender
compile_opt idl2,hidden,logical_predicate
case title of
  'resize': begin ;Update size
    self->getproperty,scr_xsize=sxs,scr_ysize=sys
    if (self.minyborder eq 0) then self.minyborder=data[1].ysize-sys
    xr=(1d0*sxs)/data[1].xsize & yb=data[1].ysize-sys
    self->setproperty,scr_xsize=data[0].xsize*xr,scr_ysize=data[0].ysize-yb
  end
  'cube_select': begin ;Update cube information 
    cubeinfo=data.db->getcubeinfo(file=data.selected_cube[0],cube_index=data.selected_cube[1])
;Format cubeinfo for output
    text0='Cube information for '+cubeinfo.cubefile
    tn=tag_names(cubeinfo) & nt=n_tags(cubeinfo)
    text1=strarr(nt-2)
    j=0
    for i=0,nt-1 do if (size(cubeinfo.(i),/type) ne 8) then begin
      text1[j]=' '+string(tn[i],format='(A23)')+'   '+strcompress(string(cubeinfo.(i)),/remove) 
      j++
    endif 
    tn=tag_names(cubeinfo.back_min) & nt=n_tags(cubeinfo.back_min)
    text2=strarr(nt*2)
    text3=strarr(nt)
    for i=0,nt-1 do begin
      text2[i*2]=' '+string('MIN_'+tn[i],format='(A23)')+'   '+strcompress(string(cubeinfo.back_min.(i)),/remove) 
      text2[i*2+1]=' '+string('MAX_'+tn[i],format='(A23)')+'   '+strcompress(string(cubeinfo.back_max.(i)),/remove)
    endfor
    text=[text0,text1,text2]
    self->setproperty,value=text
  end
  endcase
end

function pp_titanbrowse_gui_selectorwidget::init,parent,cube=cube,pixel=pixel,std=std,db=db,_ref_extra=rex
;Compound widget for cube or pixel selection
;Extra keywords go to the base widget initializer
compile_opt idl2,hidden,logical_predicate
;Defaults
cube=keyword_set(cube) ? 1B : keyword_set(pixel) ? 0B : 1B
pixel=~cube
self.frozen=1B

;Initialize the base
ret=self->basewidget::init(parent,_strict_extra=rex,column=1)
self->getproperty,name=basename,geometry=geo
;Create the child widgets
row1=obj_new('basewidget',self,row=1,frame=1,/base_align_left)
;void=obj_new('labelwidget',row1,value='Function for selection')
;func=obj_new('textwidget',row1,name=basename+'_functext',/editable,/scroll,scr_xsize=geo.scr_xsize-20)
func=obj_new('fieldwidget',row1,name=basename+'_functext',title='Function for selection',/column,scr_xsize=geo.scr_xsize-20,/cr_events)
row1->getproperty,geometry=row1geo
row2=obj_new('basewidget',self,column=2)
col1=obj_new('basewidget',row2,frame=1,row=2,scr_ysize=geo.ysize-row1geo.ysize-60)
void=obj_new('labelwidget',col1,value='Available variables')
tree=obj_new('treewidget',col1,name=basename+'_vartree',/folder,scr_ysize=geo.ysize-row1geo.ysize-80)
col1->getproperty,geometry=col1geo
col2=obj_new('basewidget',row2,row=3,frame=1,xsize=geo.xsize-col1geo.xsize-30,/base_align_left)
list_title=obj_new('labelwidget',col2,value='Selected '+(cube ? 'cube' : 'pixel')+'s         ')
list=obj_new('textwidget',col2,/scroll,scr_xsize=geo.xsize-col1geo.xsize-30,scr_ysize=geo.ysize-row1geo.ysize-160,font='Courier')
row3=obj_new('basewidget',col2,row=1,/base_align_top)
tlabel=cube ? 'cube' : 'pixel'
button_base=obj_new('basewidget',row3,row=2,/base_align_top)
void=obj_new('buttonwidget',button_base,value='Clear list',/dynamic_resize,name=tlabel+'_button',uvalue='clear',$
 tooltip='Deselect all '+tlabel+'s')
frozenswitch=obj_new('buttonwidget',button_base,value='Unfreeze selection display',/dynamic_resize,name=tlabel+'_button',uvalue='freezeswitch',$
 tooltip='Switch on/off updating the selection text display')
void=obj_new('buttonwidget',button_base,value='Select all',/dynamic_resize,name=tlabel+'_button',uvalue='all',$
 tooltip=cube ? 'Select every cube in the database' : 'Select every pixel of the selected cubes')
void=obj_new('buttonwidget',button_base,value='Propagate selection',/dynamic_resize,name=tlabel+'_button',uvalue='propagate',$
 tooltip=cube ? 'Select every pixel of the selected cubes' : 'Keep selected only the cubes with pixels in the list (and clear the pixel list)')
void=obj_new('buttonwidget',button_base,value='Histogram',/dynamic_resize,name=tlabel+'_button',uvalue='histogram',$
   tooltip='Make a histogram of the selected function values')
export_base=obj_new('basewidget',row3,frame=1,column=1,/base_align_left)
void=obj_new('labelwidget',export_base,value='Export list')
export_cube_name=obj_new('fieldwidget',export_base,title='Export to variable:',name=basename+'_export_name',/cr_events,/column)
export_cube_file=obj_new('buttonwidget',export_base,value='Export to text file',name=basename+'_export_file')
if (cube) then export_cubes_dir=obj_new('buttonwidget',row3,value='Export cube files',name=basename+'_export_cubes_dir')


;Populate the tree
if (cube) then begin
;Figure out the variable names
  cmd={pp_titanbrowse_cmd}
  nt=n_tags(cmd) & tn=tag_names(cmd)
  vars='' & j=0
  for i=0,nt-1 do if (size(cmd.(i),/type) ne 8) then vars=[vars,tn[i]]
  vars=vars[1:*] & lvars='cmd.'+strlowcase(vars)
  nt=n_tags(cmd.back_min) & tn=tag_names(cmd.back_min)
  minback='cmd.back_min.'+strlowcase(tn)
  maxback='cmd.back_max.'+strlowcase(tn)
;Put the names into the tree
  for i=0,n_elements(vars)-1 do void=obj_new('treewidget',tree,value=vars[i],name=basename+'_vartree_cube',uvalue=lvars[i])
  backmin=obj_new('treewidget',tree,value='Backplanes_min',name=basename+'_vartree_cube_back',/folder)
  backmax=obj_new('treewidget',tree,value='Backplanes_max',name=basename+'_vartree_cube_back',/folder)
  for i=0,nt-1 do begin
    void=obj_new('treewidget',backmin,value=tn[i],name=basename+'_vartree_cube',uvalue=minback[i])
    void=obj_new('treewidget',backmax,value=tn[i],name=basename+'_vartree_cube',uvalue=maxback[i])
  endfor
endif else begin
;Figure out the variable names
;Core bands
  vbands=' _c_'+strtrim(sindgen(std.bands),2)+'_ '
  nbands=strtrim(sindgen(std.bands),2)+' ('+string(*std.wavs,format='(F6.4)')+string(181B)+'m)'
;Backplanes
  vbacks=' _b_'+*std.bnames+'_ '
  nbacks=strupcase(*std.bnames) 
;Put the names into the tree
  core=obj_new('treewidget',tree,value='Core bands',name=basename+'_vartree_pixel_core',/folder)
  back=obj_new('treewidget',tree,value='Backplanes',name=basename+'_vartree_pixel_back',/folder)
  for i=0,n_elements(vbands)-1 do void=obj_new('treewidget',core,value=nbands[i],uvalue=vbands[i],name=basename+'_vartree_pixel')
  for i=0,n_elements(vbacks)-1 do void=obj_new('treewidget',back,value=nbacks[i],uvalue=vbacks[i],name=basename+'_vartree_pixel')
endelse
;Save things into self
self.cube=cube
self.pixel=pixel
self.vartree=tree
self.selfunction=func
self.list=list
self.list_title=list_title
self.db=db
self.frozenswitch=frozenswitch
return,ret
end

pro pp_titanbrowse_gui_selectorwidget::getproperty,frozen=frozen,_ref_extra=rex
compile_opt idl2,hidden,logical_predicate
if arg_present(frozen) then frozen=self.frozen
if (n_elements(rex) gt 0) then self->basewidget::getproperty,_strict_extra=rex 
end

pro pp_titanbrowse_gui_selectorwidget::setproperty,list=list,nsel=nsel,frozen=frozen,_ref_extra=rex
compile_opt idl2,hidden,logical_predicate
if (n_elements(nsel) gt 0) then begin
  self.list->setproperty,value=n_elements(list) gt 0 ? list : ''
  val='Selected '+(self.cube ? 'cube' : 'pixel')+'s: '+strtrim(string(nsel),2)
  self.list_title->setproperty,value=val
endif
if (n_elements(frozen) eq 1) then begin
  self.frozen=frozen
  self.frozenswitch->setproperty,value=frozen ? 'Unfreeze selection display' : 'Freeze selection display'
endif
if (n_elements(rex) gt 0) then self->basewidget::setproperty,_strict_extra=rex
end

pro pp_titanbrowse_gui_selectorwidget::eventhandler,event
;Intercepts events to update the text in the function text widget, then passes the event on 
compile_opt idl2,hidden,logical_predicate
if ((event.name eq 'cubeselector_vartree_cube')||(event.name eq 'pixelselector_vartree_pixel'))&&(event.clicks eq 2) then begin
  event.id->getproperty,uvalue=uv
  self.selfunction->getproperty,value=text
  ;self.selfunction->setproperty,value=text+' '+uv+' '
  self.selfunction->setproperty,value=text+uv
endif
ename=strlowcase(event.name)
self->getproperty,name=basename
case ename of
  basename+'_export_name' : if (obj_valid(self.list)) then begin
    name=idl_validname(*event.value)
    if (name ne '') then begin
      widget_control,/hourglass
      list=self.pixel ? self.db->getselectedpixels() : self.db->getselectedcubes()
      savetomain,list,name
    endif
  endif
  basename+'_export_file' : if (obj_valid(self.list)) then begin
    self.list->getproperty,value=val
    self->getproperty,id=id
    file=dialog_pickfile(default_extension='.txt',filter=['*.txt','*.TXT'],display_name='Export cube list to file',$
     /overwrite_prompt,file='titanbrowse_'+(self.cube ? 'cube' : 'pixel')+'list',dialog_parent=id,/write)
    if (file ne '') then begin
      openw,unit,file,/get_lun
      printf,unit,val,format='(A0)'
      free_lun,unit
    endif
  endif
  basename+'_export_cubes_dir' : if (obj_valid(self.list)) then begin
    self.list->getproperty,value=val
    self->getproperty,id=id
    dir=dialog_pickfile(display_name='Export cube files to directory',/directory,dialog_parent=id)
    if (dir ne '') then begin
      widget_control,/hourglass
      list=self.pixel ? self.db->getselectedpixels() : self.db->getselectedcubes()
      for i=0,n_elements(list)-1 do begin
        file=dir+path_sep()+list[i]->getproperty(/file)
        print,'Writing cube ',file
        list[i]->write,file
      endfor
    endif
  endif
  else :
endcase
;Pass the event ahead
self->getproperty,parents=par
par->eventhandler,event
end

pro pp_titanbrowse_gui_selectorwidget::messagehandler,title,sender=sender,data=data
compile_opt idl2,hidden,logical_predicate
if (title eq 'updatelist') then begin
  if (self.cube) then begin
    if (self.frozen) then begin
      data->getproperty,nselcubes=nselcubes
      self->setproperty,nsel=nselcubes    
    endif else begin
      widget_control,/hourglass
      data->getproperty,cubelist=cubelist,nselcubes=nselcubes
      self->setproperty,list=cubelist,nsel=nselcubes
    endelse
  endif else begin
    if (self.frozen) then begin
      data->getproperty,nselpixels=nselpixels
      self->setproperty,nsel=nselpixels
    endif else begin
      widget_control,/hourglass
      data->getproperty,pixellist=pixellist,nselpixels=nselpixels
      self->setproperty,list=pixellist,nsel=nselpixels
    endelse
  endelse
endif
if (title eq 'functionreplace') then self.selfunction->setproperty,value=data
end

pro pp_titanbrowse_gui::getproperty,db=db,_ref_extra=rex,version=version
compile_opt idl2,hidden,logical_predicate
if (arg_present(db)) then db=self.db
if (arg_present(version)) then version=self.version
if (n_elements(rex) gt 0) then self->toplevelbase::getproperty,_strict_extra=rex
end

pro pp_titanbrowse_gui__define
compile_opt idl2,logical_predicate
void={pp_titanbrowse_gui,inherits toplevelbase,db:obj_new(),selected_cube:lonarr(2),DEFXSZ:0,DEFYSZ:0,$
 cubeinfo:ptr_new(),cubeinfo_text:obj_new(),version:'',histowindow:obj_new(),lastpixexpr:'',lastcubeexpr:''}
void={pp_titanbrowse_gui_tabwidget,inherits tabwidget} ;Simple extension for tabwidget to provide a messagehandler
void={pp_titanbrowse_gui_treewidget,inherits treewidget} ;Simple extension for treewidget to provide a messagehandler
void={pp_titanbrowse_gui_textwidget,inherits textwidget,minyborder:0} ;Simple extension for textidget to provide a messagehandler
void={pp_titanbrowse_gui_selectorwidget,inherits basewidget,$ ;Compound widget for cube or pixel selection
 vartree:obj_new(), selfunction:obj_new(), list:obj_new(),cube:0B,pixel:0B,list_title:obj_new(),db:obj_new(),frozen:0B,frozenswitch:obj_new(),$
 inherits IDL_Object}
end
