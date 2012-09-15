; docformat = 'rst'
;+
; :Description:
;    Changes the use of compression in pp_titanbrowse's data files
;    (both for pp_titanbrowse_db and pp_titanbrowse_metadb). This rotuine is
;    necessary, instead of just opening and saving the files, to properly handle
;    the references to the heap variables stored.
;    
;    The files are provided compressed for faster downloads, and can be used
;    in that way. But if the user want to trade disk space for lower processor
;    use (and, potentially, faster operation, if the disk is fast enough), this
;    routine can be used to decompress the files.
;    
;    This rotuine can take several minutes to run, as all datafiles currently
;    add to tens of GB. 
;
; :Params:
;    savefile : in, required
;      The name of the savefile to compress/decompress.
;
; :Keywords:
;    compress : in, optional, default=1B
;      Determines whether the created files will be compressed.
;      
; :Examples:
;    To decompress all pp_titanbrowse's ir channel files in the current directory::
;    
;      fl=file_search('covims_????_ir*.sav') ;find the files
;      for i=0,n_elements(fl)-1 do pp_titanbrowse_datacompress,fl[i],compress=0
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Jul/2010
;-
pro pp_titanbrowse_datacompress,savefile,compress=compress
compile_opt idl2, logical_predicate

;Defaults
compress=n_elements(compress) eq 1 ? compress : 1B

restore,savefile
lastid=idstring[n_elements(idstring)-1]
case lastid of
  'pp_titanbrowse_db_container': begin ;If savefile is db file
    coreheapinds=long(strsplit(strjoin(string(pbands,/print)),'<PtrHeapVar',/regex,/extract))
    backheapinds=long(strsplit(strjoin(string(pbacks,/print)),'<PtrHeapVar',/regex,/extract))
    print,'Writing savefile "',strtrim(savefile,2),'"'
    save,file=savefile,compress=compress,idstring,npixels,pstart,coreheapinds,backheapinds,pbands,pbacks
  end
  'pp_titanbrowse_metadb_container': begin ;If savefile is metadb file
    heapinds=long(strsplit(strjoin(string(ocubes,/print)),'<ObjHeapVar',/regex,/extract))
    print,'Writing savefile "',strtrim(savefile,2),'"'
    save,file=savefile,idstring,ncubes,cubefiles,heapinds,ocubes,$
     modind,cmd,std,compress=compress
  end
  else: print,'File not recognized as a pp_titanbrowse savefile, doing nothing to it'
endcase
end
