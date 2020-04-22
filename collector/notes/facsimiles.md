Facsimiles
==========

This is what I did:
- bucket `facsimiles.alter-rebbe.org` created;
- CNAME for it pointing to `c.storage.googleapis.com` added;
- member "allUsers" with role "Storag Object Viewer" added to the bucket;
- facsimiles (extracted from the GitHub repository) uploaded into the bucket;
- code changed to point to a configured facsimiles host;
- facsimiles removed from the repository and its history (using BFG Repocleaner);

Remains to be done:
- fix the code that looks for facsimiles in the repository
  (use Google API to get (and cache?) bucket structure?);
- add CDN?
  
## Handy Commands ##

To extract images from PDF files:
```
  $ ls -1 *.pdf | xargs -I {} pdfimages -j {} {}
  $ rename '.pdf-000' '' *
```

To cut facsimiles:
```
  $ convert xxx.tif -crop 2x1+120@ x-%d.tif
```

To compress facsimiles:
```
  $ mogrify -path out -quality 80% -format jpg *.tif
```

To sync with the bucket:
```
  $ gsutil -m rsync -r -c -d <path-to-local-copy-of-the-bucket> gs://facsimiles.alter-rebbe.org
```
