## Static / Dynamic ##

Code in the `collector` module pre-generates the static www.alter-rebbe.org site,
which is then further processed by Jekyll when published on GitHub Pages.

We are moving towards making the site dynamic, so that we can add:  
- search functionality;
- ability to modify the site.

To support self-contained dynamic site we need to replicate some of the Jekyll's functionality:
- compile SCSS into CSS: compass/jsass?;
- convert Markdown to HTML (blog, notes): flexmark-java or Laika (planet42.github.io/Laika).

## Project Setup ##

- activated free Google Cloud Platform trial for `dub@opentorah.org` account (6/14/2020); 
- created project `alter-rebbe` with id `alter-rebbe-2` (id `alter-rebbe` is taken by the previous incarnation;
  maybe when a month passes and it gets deleted I’ll migrate to the old id);
- organization `opentorah.org` was auto-created; 

```
  $ gcloud auth login # to log into the dub@opentorah.org account.
  $ gcloud projects create [PROJECT_ID]
  $ gcloud config set project alter-rebbe-2
```

## Facsimiles ##

- created bucket `facsimile.alter-rebbe.org`
  (facsimiles.alter-rebbe.org was then taken by the previous incarnation) with:
  - standard storage type;
  - multi-region;
  - uniform access control;
- made it public: in its `Permissions | Add members | New members allUsers`,
 `Select a role | Cloud Storage | Storage Object Viewer`;
- added CNAME record for `facsimile.alter-rebbe.org` pointing to `c.storage.googleapis.com`;
```
  $ gsutil -m rsync -d -r <local copy> gs://facsimile.alter-rebbe.org
```
Deleted `facsimiles.alter-rebbe.org` bucket in the old project
  in the dub@podval.org account - and the project itself.

Created `facsimiles.alter-rebbe.org` bucket in the new project; made it public.
```
  $ gsutil -m copy -r gs://facsimile.alter-rebbe.org gs://facsimiles.alter-rebbe.org.
```
And (to fix unwanted directory nesting):
```
  $ gsutil -m mv gs://facsimiles.alter-rebbe.org/facsimile.alter-rebbe.org/facsimiles
    gs://facsimiles.alter-rebbe.org/
```
Deleted `facsimile.alter-rebbe.org` bucket and removed CNAME record for it.

Facsimiles displayed on the site come from that bucket;
they can be retrieved by anyone who has the correct URL.

To validate that facsimiles referenced from the site are in one-to-one correspondence with
the files in the bucket, we probably need to use Google Cloud Storage client to retrieve
(and cache) the list of them.

We also might want to use Google's CDN for them (Firebase Hosting?).

(I used BFG Repocleaner to remove facsimiles and their Git history from this repository
once they moved out; current tool for things like that is `git-filter-repo`.)
   
### Image Processing Commands ###

To extract images from PDF files:
```
  $ ls -1 *.pdf | xargs -I {} pdfimages -j {} {}
  $ rename '.pdf-000' '' *
```

To cut facsimiles:
```
  $ convert xxx.jpeg -crop 2x1+120@ x-%d.jpeg
```

To compress facsimiles:
```
  $ mogrify -path out -quality 80% -format jpg *.tif
```

To sync with the bucket:
```
  $ gsutil -m rsync -r -d <path-to-local-copy-of-the-bucket> gs://facsimiles.alter-rebbe.org
```

## JIB ##

Everything is Dockerized nowadays. To make docker work locally,
[I had to](https://linuxconfig.org/how-to-install-docker-on-fedora-31)
revert back to cgroup v1:
```
  $ sudo dnf install -y grubby
  $ sudo grubby --update-kernel=ALL --args="systemd.unified_cgroup_hierarchy=0"
  $ sudo reboot
```

I store my Docker image (`gcr.io/alter-rebbe-2/collector`) in the GCP's Container Registry:
```
  $ gcloud auth configure-docker
  $ gcloud components install docker-credential-gcr
  $ gcloud services enable containerregistry.googleapis.com
```

I use [jib](https://github.com/GoogleContainerTools/jib) Gradle Plugin to
build and push my Docker image:
```
  $ ./gradlew :collector-service:jib
```
Image layers are in the `artifacts.alter-rebbe-2.appspot.com/containers/images` bucket that
was auto-created (with fine-grained access control).

## Cloud Run ##

I use [Cloud Run](https://cloud.google.com/run#key-features)
([Unofficial FAQ](https://github.com/ahmetb/cloud-run-faq)).

```
 $ gcloud services enable run.googleapis.com
 $ gcloud config set run/platform managed
 $ gcloud config set run/region us-east4
```

Memorystore/Redis turned out to be too expensive (Google charges for *provisioned* capacity),
so the fact that Cloud Run probably can't talk to it even now (4/2020) isn't important :(
I may end up using [Cloud Firestore](https://firebase.google.com/docs/firestore)
for caching generated (and maybe even source) files.

To deploy on the Cloud Run:
```
  $ gcloud run deploy collector --image gcr.io/alter-rebbe-2/collector --allow-unauthenticated
```

To set entry point variables and environment variables:
```
  $ gcloud run services update collector
    --command COMMAND
    --args ARG1,ARG-N                  To reset this field to its default, pass an empty string.
    --clear-env-vars                   Remove all environment variables.
    --set-env-vars=[KEY=VALUE,...]     All existing environment variables will be removed first.
    --remove-env-vars=[KEY,...]        List of environment variables to be removed.
    --update-env-vars=[KEY=VALUE,...]  List of key-value pairs to set as environment variables.
At most one of 'clear' and 'set' may be specified.
If both 'remove' and 'update' are specified, 'remove' will be applied first.
```

To add a domain mapping:
```
  $ gcloud beta run domain-mappings create
    --service collector
    --domain app.alter-rebbe.org
    --force-override # because of the old project
```

Certificate provisioning spinner starts spinning once DNS record (`app CNAME ghs.googlehosted.com.`)
is in place; mine was there from the previous incarnation. I was getting “unexpectedly closed the
connection” while it was spinning and for a few minutes after it stopped; http 302-redirects to https.

I do not see the need to set up [Cloud Build](https://cloud.google.com/cloud-build),
but if I do - it runs locally too!

At my scale and using only Google Cloud, [Terraform](https://www.hashicorp.com/products/terraform) is
not needed...

### Running in local Docker ###

To run the container locally, build it to local Docker:
```
  $ ./gradlew jibDockerBuild
```
or, if pushed to a repository, pull it from there:
```
  $ docker pull <image name>
```
and then:
```
  $ docker run <image name>
```


To set entry point variables and environment variables:
```
  $ docker run -e "NAME=VALUE" <image name> <arg1> <arg2> <arg3>
```

### Logging ###

Excellent [post](https://medium.com/google-cloud/java-logging-on-cloud-run-with-stackdriver-9786d6fdbe17)
by [Averi Kitsch](https://medium.com/@averikitsch)
and the official [documentation](https://cloud.google.com/run/docs/logging#run_manual_logging-java)
on logging in Google Cloud Run were useful.

Special fields in JSON structured payloads are described in the Management Tools logging
[documentation](https://cloud.google.com/logging/docs/agent/configuration#special-fields).

Field `logging.googleapis.com/trace` can be used to nest log entries for the same request
under the request's entry.

### Keeping the service warm ###

```
  $ gcloud services enable cloudscheduler.googleapis.com
  $ gcloud scheduler jobs create http collector-service-warmer
      --description "Keep the Collector Service warm"
      --schedule="every 1 hours"
      --http-method GET
      --uri="https://app.alter-rebbe.org"
```


Before the first job si created, service has to be initialized. I did it via Console
(in the same region - us-east4 - my service is running). This created an empty App Engine
application `alter-rebbe-2.appspot.com` and an empty bucket with the same name...

gcloud does not require time zone, but the Console does...
