Each chapter comes from the following repositories:
* Chapter 1: www.github.com/liza-wood/organicseed_adoption
* Chapter 2: www.github.com/liza-wood/osisn_spatial
* Chapter 3: www.github.com/liza-wood/osisn_processes

User pre-reqs: 
Here's the situation: I wanted my dissertation output to be in LaTeX but (let's be real) I've been writing my dissertation in Google Docs. I kind of understand LaTeX, and I understand Rmd well-enough (e.g. I am past the learning curve of spending hours on getting my `kable` just right and I hardly ever get working directory errors anymore). I am a glutton for coding punishment, but I want this dissertation process to be easy-ish. **If this sounds like you, then this could be the workflow you're looking for!**

The set-up:
* Writing in Google Docs: each Chapter is a document  
* Citations are embedding into the Google Doc via the Zotero plug-in 
* Coding in R Projects: each chapter has an R project (ideally -- though in reality you just need to know the filepaths of all your figures and tables)

Prepping your Google Doc:
* Use headers appropriately
  * Your Title should be the only header (e.g. set it as Header). The translation will turn these into markdown headers (e.g. # or ##)
  * Your sections should be secondary headers (##)
* Replace tables and figures with code chunks reading in an embedding those objects
  * make sure there is ; between lines of code in chunks
* Zotero settings: You can write the dissertation with any citations format you want, but when you're nearing the end, you'll want to switch the Options to BibTex format (it is ugly but deal with it). 
**the actual Google Doc itself will end up looking a bit ugly, but this is what I found works for translated to BibTex**


