library(stringr)

prep_papers <- function(GDOC_URL, GDOC_PATH){
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
  # These aren't actually unique so nevermind
  #zot_links <- str_extract_all(refs, '\\(https\\://www\\.zotero.*(?=\\\n\\\n)')
  refs <- str_remove_all(refs, '\\(https\\://www\\.zotero.*(?=\\\n\\\n)')
  refs <- str_remove_all(refs, '\\[|\\]|\\*')
  # Remove linebreaks so help the parser
  refs <- str_replace_all(refs, '\\\n(?!\\\n)', ' ')
  
  # If bibtext formatting setting in Zotero
  if(str_detect(refs,'^\\\\@')){
    # If bibtext, then remove \\ before @
    refs <- str_remove_all(refs, '\\\\(?=@)')
    # if bibtex, save as .bib
    write.table(refs, paste0(GDOC_PATH, '04_refs.bib'),
                row.name = F, col.names = F, quote = F)
    
    # For bibtex Zotero
    bt <- bib2df::bib2df(paste0(GDOC_PATH, '04_refs.bib'))
    
    # Fill in spaces in ref text
    spaced_bibkeys <- str_which(bt$BIBTEXKEY, '\\\n|\\s')
    if(length(spaced_bibkeys) > 0){
      for(i in spaced_bibkeys){
        refs <- str_replace_all(refs, bt$BIBTEXKEY[i], 
                                str_replace_all(bt$BIBTEXKEY[i], '\\\n|\\s', '_'))
      }
    }

    # Correct et al
    etal_bibkeys <- str_which(bt$BIBTEXKEY, 'et(_|\\s|)al\\.\\\\')
    ## I don;t know that I need to do what I did above, right?
    refs <- str_replace_all(refs, 'et(_?\\s?)al\\.\\\\', 'et_al\\.')
    
    # Re-writing refs with correction
    write.table(refs, paste0(GDOC_PATH, '04_refs.bib'),
                row.name = F, col.names = F, quote = F)

    # Fill in spaces in bib file
    bt$BIBTEXKEY <- str_replace_all(bt$BIBTEXKEY, '\\\n|\\s', '_')
    # Correct et al
    bt$BIBTEXKEY <- str_replace_all(bt$BIBTEXKEY, 'et(_|\\s)al\\.\\\\', 'et_al\\.')
    
    # Remove the references from the text but keep the footnotes
    txt <- paste(str_remove(str_sub(txt, 1, ref_start[1]), "#?#$"), 
                 str_sub(txt, ref_end[1], nchar(txt)))
    
    # Correct where linebreaks were
    break_loc <- data.frame(str_locate_all(txt, '\\[\\w+(\\s|\\n)\\w+_\\d{4}\\]'))
    # Then replace them in the text
    if(nrow(break_loc) > 0){
      for(i in 1:nrow(break_loc)){
        str_sub(txt, break_loc[i,1], break_loc[i,2]) <- str_replace_all(str_sub(txt, break_loc[i,1], break_loc[i,2]), 
                                                                        '\\\n|\\s', '_')
    }
    }
    
    # Then remove where the zotero link it
    txt <- str_remove_all(txt, '\\(https\\://www\\.zotero\\.org/google\\-docs/\\?.{6}\\)')
    
    # identify inline based on key (everything should be matched) and assign @
    for(i in 1:length(bt$BIBTEXKEY)){
      txt <- str_replace_all(txt, bt$BIBTEXKEY[i], paste0('@', bt$BIBTEXKEY[i]))
    }
    # if multiple references then replace _ with ;
    txt <- str_replace_all(txt, '_@', '; @')
    
  } else {
    # If not bibtext, Save as text to be read by the parser
    write.table(refs, paste0(GDOC_PATH, '04_refs.txt'),
                row.name = F, col.names = F, quote = F)
    
    # parse takes a biblio -- this runs in terminal but not here
    # but running in terminal yields a bad extraction, but when I paste
    # the test into anystyle.io it is great. I don't know why.
    #system(paste('anystyle --overwrite -f bib parse',
    #             paste0(GDOC_PATH, '04_refs.txt'),
    #             paste0(GDOC_PATH, 'bib'))) 
    
    # Replace inline refs with bibtext
    #refs <- str_split(refs, '\\\n')
    #refs <- unlist(refs)
    #refs <- refs[refs != '']
    #
    #name <- tolower(str_remove(word(refs), ','))
    #year <- str_extract(refs, '\\d{4}')
    #inline <- paste0(name,year)
    
    # Remove the references from the text but keep the footnotes
    txt <- paste(str_remove(str_sub(txt, 1, ref_start[1]), "#?#$"), 
               str_sub(txt, ref_end[1], nchar(txt)))
    
    # Then remove where the zotero link is
    txt <- str_remove_all(txt, '\\(https\\://www\\.zotero\\.org/google\\-docs/\\?.{6}\\)')
    txt <- str_remove_all(txt, '\\[(?=\\()|(?<=\\))\\]')
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
  write.table(txt, paste0(GDOC_PATH, '02_manuscript.Rmd'), 
              col.names = F, row.names = F, quote = F)
  
  
}

ch1url <- 'https://docs.google.com/document/d/1T9K308unavSzrx7A4ZsXRTScazhfeAVHGT-rtawP1x4/edit'
ch1path <- '~/Documents/Davis/R-Projects/organicseed_adoption/ch1-manuscript/'

ch2url <- 'https://docs.google.com/document/d/1wsDqDBgijoXFMuOlyHa3hSlUwcNLpUWnRna11r8dxzs/edit'
ch2path <- '~/Documents/Davis/R-Projects/osisn_spatial/ch2-manuscript/'

ch3url <- 'https://docs.google.com/document/d/1OWRFWWUNgEmn2VDEcj6bS8uc_lL1VnteS5jK-_YyhII/edit'
ch3path <- '~/Documents/Davis/R-Projects/osisn_processes/ch3-manuscript/'


prep_papers(ch1url, ch1path)
prep_papers(ch2url, ch2path)
prep_papers(ch3url, ch3path)

# If the setting in Google docs is bibtext, I can make my bibfile automatically by combining each bib
auto_bib_file <- function(){
  bib1 <- readtext::readtext(paste0(ch1path,'04_refs.bib'))$text
  bib2 <- readtext::readtext(paste0(ch2path,'04_refs.bib'))$text
  bib3 <- readtext::readtext(paste0(ch3path,'04_refs.bib'))$text
  
  bib <- paste(bib1, bib2, bib3, collapse = "\n")
  
  write.table(bib, 'bib/thesis.bib', row.names = F, col.names = F, quote = F)
}
# If not, I compile the text references and put them into Antyle myself to create thesis.bib
manual_bib_file <- function(){
  bib1 <- readtext::readtext(paste0(ch1path,'04_refs.txt'))$text
  bib2 <- readtext::readtext(paste0(ch2path,'04_refs.txt'))$text
  bib3 <- readtext::readtext(paste0(ch3path,'04_refs.txt'))$text
  
  bib <- paste(bib1, bib2, bib3, collapse = "\n")
  
  write.table(bib, 'bib/thesis.txt', row.names = F, col.names = F, quote = F)
}

manual_bib_file()
test <- bib2df::bib2df('bib/thesis.bib')
