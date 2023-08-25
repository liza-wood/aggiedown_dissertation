library(stringr)

prep_papers <- function(x){
  # download chapter from drive
  googledrive::drive_download(GDOC_URL,
                              path = paste0(GDOC_PATH, '01_drive_manuscript.docx'),
                              overwrite = T)
  
  # convert the word to text (could do markdown but it does weird things with the backticks)
  rmarkdown::pandoc_convert(paste0(GDOC_PATH, '01_drive_manuscript.docx'),
                            output =  paste0(GDOC_PATH, '01_drive_manuscript.txt'))
  
  # Here I can manipulate the document
  txt <- readtext::readtext(paste0(GDOC_PATH, '01_drive_manuscript.txt'))[,2]
  # Start at header
  start <- str_locate(txt, '#\\s\\w')
  txt <- stringr::str_sub(txt, start[1], nchar(txt))
  
  # Take references -- put into anystyle, but don't remove footnotes (after refs)
  ref_start <- str_locate(txt, '#\\sReferences')
  ref_end <- str_locate_all(txt, '\\[\\^1\\]')
  if(length(ref_end) == 0){
    ref_end = nchar(txt)
  } else {
    ref_end = ref_end[[1]][lengths(ref_end)/2]
  }
  refs <- str_sub(txt, ref_start[1], ref_end[1]-1)
  # Remove the reference label
  refs <- str_remove(refs, '# References\\\n\\\n')
  # Remove the zotero embeddings, but keep these as an index for later
  zot_links <- str_extract_all(refs, '\\(https\\://www\\.zotero.*(?=\\\n\\\n)')
  refs <- str_remove_all(refs, '\\(https\\://www\\.zotero.*(?=\\\n\\\n)')
  refs <- str_remove_all(refs, '\\[|\\]|\\*')
  # Remove linebreaks so help the parser
  refs <- str_replace_all(refs, '\\\n(?!\\\n)', ' ')
  # If bibtext, then remove \\ before @
  refs <- str_remove_all(refs, '\\\\(?=@)')
  # if bibtex, save as .bib
  write.table(refs, paste0(GDOC_PATH, '04_refs.bib'),
              row.name = F, col.names = F, quote = F)
  # If not bibtext, Save as text to be read by the parser
  write.table(refs, paste0(GDOC_PATH, '04_refs.txt'),
              row.name = F, col.names = F, quote = F)
  
  # parse takes a biblio -- this runs in terminal but not here
  # but running in terminal yields a bad extraction, but when I paste
  # the test into anystyle.io it is great. I don't know why.
  #system(paste('anystyle --overwrite -f bib parse',
  #             paste0(GDOC_PATH, '04_refs.txt'),
  #             paste0(GDOC_PATH, 'bib'))) 
  
  # Remove the references from the text but keep the footnotes
  txt <- paste(str_sub(txt, 1, ref_start[1]-1), 
               str_sub(txt, ref_end, nchar(txt)))
  

  # Replace inline refs with bibtext
  refs <- str_split(refs, '\\\n')
  refs <- unlist(refs)
  refs <- refs[refs != '']
  
  name <- tolower(str_remove(word(refs), ','))
  year <- str_extract(refs, '\\d{4}')
  inline <- paste0(name,year)
  
  zot_links = unlist(zot_links)
  if(length(inline) == length(zot_links)){
    print("Well done, the text matching can align inline citations with bibliography Zotero links")
  } else {
    print("Sorry, please check that your Zotero references are well-organized. Right now the inline citations and bibliography do not line up")
    break
  }
  
  for(i in 1:length(zot_links)){
    str_replace(txt, '\\[\\(.*\\')
  }
  
  
  # Keep backticks for code chunks
  txt <- stringr::str_replace_all(txt, '\\\\`\\\\`\\\\`', '```')
  # Allow chaptermark
  cm <- str_locate(txt, 'chaptermark')
  str_sub(txt, cm[1]-2, cm[2]) <- '\\chaptermark'
  
  # And then make sure code within chunks doesn't have markup
  chunks <- data.frame(str_locate_all(txt, '```'))
  for(i in seq(1,nrow(chunks), 2)){
    if(i == 1){
      rpl <- str_remove_all(str_sub(txt, chunks$end[i], chunks$start[i+1]), '\\\\')
      rpl <- str_replace_all(rpl, '(?<!\\})\\n(?!`)', ' ')
      str_sub(txt, chunks$end[i], chunks$start[i+1]) <- rpl
    } else {
      newchunks <- data.frame(str_locate_all(txt, '```'))
      rpl <- str_remove_all(str_sub(txt, newchunks$end[i], newchunks$start[i+1]), '\\\\')
      rpl <- str_replace_all(rpl, '(?<!\\})\\n(?!`)', ' ')
      str_sub(txt, newchunks$end[i], newchunks$start[i+1]) <- rpl
    }
  }
  
  # Remove {.underline}
  txt <- stringr::str_replace_all(txt, '\\{\\.underline\\}', '')
  # Get rid of backslash before double quotes
  str_locate(txt, '\\\\"')
  str_sub(txt, 1390, 1395)
  txt <- str_replace_all(txt, '\\\\"', '"')
  str_sub(txt, 1390, 1395)
  # Remove media files
  media <- stringr::str_locate(txt, '\\!\\[\\]\\(media\\/image\\d\\.(png|jpg)\\)')
  stringr::str_sub(txt, media[1]-50, media[2])
  txt <- stringr::str_replace_all(txt, '\\!\\[\\]\\(media\\/image\\d\\.(png|jpg)\\)', '')
  # Add backslash to brackets
  str_locate(txt, '\\\\\\[')
  str_sub(txt, 8023, 8028)
  txt <- str_replace_all(txt, '\\\\\\[', '\\[')
  str_locate(txt, '\\\\\\]')
  str_sub(txt, 8023, 8028)
  txt <- str_replace_all(txt, '\\\\\\]', '\\]')
  # Allow there to be commending out
  str_locate(txt, '\\\\\\<\\!\\\\-\\\\--')
  txt <- stringr::str_replace_all(txt, '\\\\\\<\\!\\\\-\\\\--', '<!---')
  stringr::str_sub(txt, 21630, 21645)
  str_locate(txt, '\\\\-\\\\--\\\\\\>')
  txt <- stringr::str_replace_all(txt, '\\\\-\\\\--\\\\\\>', '--->')
  write.table(txt, paste0(GDOC_PATH, '03_manuscript.Rmd'), 
              col.names = F, row.names = F, quote = F)
  
  
}



GDOC_URL <- 'https://docs.google.com/document/d/1wsDqDBgijoXFMuOlyHa3hSlUwcNLpUWnRna11r8dxzs/edit'
GDOC_PATH <- '~/Documents/Davis/R-Projects/osisn_spatial/ch2-manuscript/'

prep_papers()

#Then need to get manual bib files -- do this for each and paste together
bib2 <- readtext::readtext('bib/anystyle-ch2.bib')$text

write.table(bib2, 'bib/thesis.bib', row.names = F, col.names = F, quote = F)


