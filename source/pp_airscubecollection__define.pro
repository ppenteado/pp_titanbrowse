; docformat = 'rst'
;+
;
; :Uses: pp_editablecube__define, pp_getcubeheadervalue, pp_extractfields, pp_setcubeheadervalue, pp_readcube__define
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Nov/2009
;-

;+
; :Description:
;    Initializes a pp_cubecollection object, either from a previously built file,
;    or builds the file from cube files.
;
; :Params:
;    savefile : in, required
;      The name of the file that either will contain the object's data, or the data will be
;      read from, depending on the build keyword.
;
; :Keywords:
;    build : in, optional, default=0
;      If set, the cubes named in cubefiles are read from disk and the container file is created.
;      Otherwise, it is assumed the container file was previously created, and the object gets
;      its data from that file.
;    cubefiles : in, out, optional
;      If build is set, a string array with the names of the cube files to read. If absent and build is
;      set, all cubes found in the current directory (ending with '_eg.cub', '_ir_eg.cub', or '_vis_eg.cub',
;      depending on the vis and ir keywords) are used, and their names are returned in this keyword. 
;    vis : in, optional, default=0
;      If set, cubes matching '*_vis_eg.cub' are used when building the container file, instead of '*_eg.cub'.
;    ir : in, optional, default=0
;      If set, cubes matching '*_ir_eg.cub' are used when building the container file, instead of '*_eg.cub'.
;
; :Examples:
;    See the example on pp_editablecube__define.
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Nov/2009
;-
function pp_airscubecollection::init,savefile,build=build,cubefiles=cubefiles,vis=vis,ir=ir,compress=compress
compile_opt idl2

ret=0
;Defaults
compress=n_elements(compress) eq 1 ? compress : 1B
build=n_elements(build) eq 1 ? build : 0
if (n_elements(savefile) eq 0) then begin
  print,'pp_cubecollection: collection file not provided'
  return,0
endif else self.savefile=savefile

idstring='pp_cubecollection_container' ;id to test if savefile was created by this object

catch,error_status
if (error_status ne 0) then begin
  catch,/cancel
  print,'pp_cubecollection::init : Could not read file "',self.savefile,'"'
  self->cleanup
endif else begin
;Build savefile
  if build then begin
    ncubes=n_elements(cubefiles)
    suff=keyword_set(vis) ? '_vis_eg.cub' : (keyword_set(ir) ? '_ir_eg.cub' : '_eg.cub') 
    cubefiles=ncubes gt 0 ? cubefiles : file_search('*'+suff,count=ncubes)
    print,ncubes,' cubes found'
    if (ncubes eq 0) then message,'No cubes found to build collection from'
    print,'Building collection file, this may take a long time and use a lot of memory if there are many cubes'
    ocubes=objarr(ncubes)
    for i=0,ncubes-1 do begin
      print,'Reading ',cubefiles[i],' (',strcompress(string(i+1,'/',ncubes,')'),/rem)
      ocubes[i]=obj_new('pp_airscube',file=cubefiles[i])
    endfor
;Build index of heap variables, needed to retrieve individual elements of ocube from savefile
    heapinds=long(strsplit(strjoin(string(ocubes,/print)),'<ObjHeapVar',/regex,/extract))
;Make savefile
    print,'Writing savefile'
    save,file=savefile,idstring,ncubes,cubefiles,heapinds,ocubes,compress=compress
;Get rid of the large ocubes array
    obj_destroy,ocubes
  endif
;Initialize the object to read the savefile
  self.osav=obj_new('idl_savefile',savefile,/relaxed_structure_assignment)
;Get metadata from savefile, if initializing from it
  if (~build) then begin
    oidstring=idstring
    self.osav->restore,'idstring'
    if (idstring[0] ne oidstring) then message,'Not a pp_cubecollection savefile'
    self.osav->restore,['ncubes','cubefiles','heapinds']
;Make a dummy editablecube object just to make sure its methods get compiled
    a=obj_new('pp_airscube',file='')
    obj_destroy,a
  endif
;Save fields into self
  self.ncubes=ncubes
  self.cubefiles=ptr_new(cubefiles,/no_copy)
  self.heapinds=ptr_new(heapinds,/no_copy)
  obj_destroy,self.osav
  ret=1
endelse
return,ret
end


;+
; :Description:
;    Object that contains a collection of cubes, that can be retrieved as pp_editablecube objects
;    by their names or indexes. The cubes are stored in a savefile instead of memory, so initializing
;    from an existing file is a quick and light operation. The cube data is only read when that particular
;    cube is to be retrieved, and only those selected to be retrieved are read from the file. Thus a savefile
;    made by a pp_cubecollecion object is a portable, convenient and efficient way to store and carry several cubes.
;
; :Examples:
;    Initially, a container file must be built from cube files. To take all files ending with
;    '_ir_eg.cub' from the current directory (which can be a long operation and use a lot of
;    memory if there are many cubes)::
;    
;       a=obj_new('pp_cubecollection','testcollection.sav',/build,/ir)
;       
;    If a collection savefile already exists, no matter how large it is, loading it is a quick
;    and light operation, because only meta-data is read::
;     
;      a=obj_new('pp_cubecollection','testcollection.sav')
;       
;    To get the names of the cubes present in the collection::
;     
;      print,a->filenames(ncubes=ncubes)
;      ;CM_1467426144_5_ir_eg.cub CM_1467426479_1_ir_eg.cub CM_1467426798_1_ir_eg.cub CM_1467427145_1_ir_eg.cub
;      ;(...)
;      print,ncubes
;      57
;
;    To retrieve the first 3 cubes::
;     
;      b=a->getcube([0,1,2])
;       
;    To retrieve the cube of name CM_1467426479_1_ir_eg::
;     
;      c=a->getcube(name='CM_1467426479_1_ir_eg.cub')
;       
;     b[1] and c are the same object::
;     
;       ;print,b[1],c
;       
;    To make a copy of that object, to keep after the collection is destroyed::
;     
;      d=obj_new('pp_editablecube',c)
;      print,obj_valid(b),obj_valid(c),obj_valid(d)
;      ;1   1   1
;      ;1
;      ;1
;      obj_destroy,b
;      print,obj_valid(b),obj_valid(c),obj_valid(d)
;      ;0   0   0
;      ;0
;      ;1
;       
;    Get rid of the collection once it is done with. This does not affect the savefile,
;    which can be used to reobtain this collection in the future::
;     
;      obj_destroy,a
;      
;    See pp_editablecube__define for examples on how to use the cube objects.
;
; :Uses: pp_editablecube__define, pp_getcubeheadervalue, pp_extractfields, pp_setcubeheadervalue, pp_readcube__define
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Nov/2009
;-
pro pp_airscubecollection__define
compile_opt idl2
void={pp_airscubecollection, inherits pp_cubecollection}
end
