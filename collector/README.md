## Static / Dynamic ##

Code in the `collector` module pre-generates the static store.alter-rebbe.org site and syncs it into the Google Cloud Storage bucket.

We are moving towards making the site dynamic, so that we can add:  
- search functionality;
- ability to modify the site.

## Project Setup ##

- activated free Google Cloud Platform trial for `dub@opentorah.org` account (6/14/2020); 
- logged into it and created project `alter-rebbe` with id `alter-rebbe-2`
  (id `alter-rebbe` was taken by the previous incarnation,
  and can't be reused even now, long after it was deleted :();
- organization `opentorah.org` was auto-created; 

To set `dub@opentorah.org` as a default account and `alter-rebbe-2` as a default project:
```
  $ gcloud auth login dub@opentorah.org
  $ gcloud config set account dub@opentorah.org
  $ gcloud config set project alter-rebbe-2
```

To set Application Default Credentials that Cloud Code in the IDE needs:
```
  $ gcloud auth login --update-adc
```

## Facsimiles ##

- created bucket `facsimiles.alter-rebbe.org` with:
  - standard storage type;
  - multi-region;
  - uniform access control;
- made it public: in its `Permissions | Add members | New members allUsers`,
 `Select a role | Cloud Storage | Storage Object Viewer`;
- added CNAME record for `facsimiles.alter-rebbe.org` pointing to `c.storage.googleapis.com`;

Added `404.html` and set it as the error page in the website configuration of the bucket
(default error page is in XML).

Facsimiles displayed on the site come from that bucket;
they can be retrieved by anyone who has the correct URL.

To validate that facsimiles referenced from the site are in one-to-one correspondence with
the files in the bucket, we probably need to use Google Cloud Storage client to retrieve
(and cache) the list of them.

To sync local copy of the bucket into it with `gsutil`:
```
  $ gsutil -m rsync -d -r <local copy> gs://facsimile.alter-rebbe.org
```

Note: Since `facsimiles.alter-rebbe.org` was then taken by the bucket in the previous incarnation of the project
under dup@podval.org account, intermediate bucket was used to migrate the facsimiles into the new project's bucket.

Note: Chrome [tightened the nuts on the mixed content](https://blog.chromium.org/2019/10/no-more-mixed-messages-about-https.html),
so links to individual photograph in the facsimile page have to use HTTPS now.
For the SSL certificate's common name to be correct, those links have to point to the photographs indirectly
via `https://storage.googleapis.com/`. So, for example,
`http://facsimiles.alter-rebbe.org/facsimiles/derzhavin6/390.jpg` becomes
`https://storage.googleapis.com/facsimiles.alter-rebbe.org/facsimiles/derzhavin6/390.jpg`.
Alternatively, I can configure an CDN for the facsimiles - but I do not see the need at this point.

Note: I used BFG Repocleaner to remove facsimiles and their Git history from this repository
once they moved out; current tool for things like that is `git-filter-repo`.
   
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

## Store ##

On 2020-12-06 started moving towards storing the store data in a Google Storage Bucket:
- created `store.alter-rebbe.org` bucket;
- made it public;
- pointed CNAME record at it;


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
  $ ./gradlew :collector:jib
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

In December 2020 I wrote Cloud Run Gradle plugin and now use it to deploy the service.
Service configuration is in the `service.yaml` file.

Historically:
To deploy on the Cloud Run for the first time (beta is required for the `--min-instances` option):
```
  $ gcloud beta run deploy collector \
    --image gcr.io/alter-rebbe-2/collector \ 
    --allow-unauthenticated \
    --platform managed \
    --region us-east4 \
    --min-instances 1
```

To re-deploy inheriting parameters from the previous revision (image name is not inherited for some reason):
```
  $ gcloud run deploy collector --image gcr.io/alter-rebbe-2/collector 
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

I pointed `www.alter-rebbe.org` at the dynamic app, and configured it to proxy for
`store.alter-rebbe.org` on 2020-07-19.

I do not see the need to set up [Cloud Build](https://cloud.google.com/cloud-build),
but if I do - it runs locally too!

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

## Service Account ##

Created service account `cloud-run-deploy` and its key:
```
  $ gcloud iam service-accounts create cloud-run-deploy
  $ gcloud iam service-accounts keys create ./key.json --iam-account cloud-run-deploy@alter-rebbe-2.iam.gserviceaccount.com
```

For local use, added the key to `~/.gradle/gradle.properties` as a `gcloudServiceAccountKey_alter_rebbe_2` property,
with a backslash after each line of the key except the last one, and with backslash-n replaced with backslash-backslash-n :)

Granted the service account the roles for the Container Registry and Cloud Run
(see https://cloud.google.com/run/docs/reference/iam/roles#gcloud):
```
  $ gcloud projects add-iam-policy-binding alter-rebbe-2 \
    --member "serviceAccount:cloud-run-deploy@alter-rebbe-2.iam.gserviceaccount.com" --role "roles/storage.admin"
  $ gcloud projects add-iam-policy-binding alter-rebbe-2 \
    --member "serviceAccount:cloud-run-deploy@alter-rebbe-2.iam.gserviceaccount.com" --role "roles/run.admin"
  $ gcloud iam service-accounts add-iam-policy-binding 161107830568-compute@developer.gserviceaccount.com \
    --member="serviceAccount:cloud-run-deploy@alter-rebbe-2.iam.gserviceaccount.com" --role="roles/iam.serviceAccountUser"
```

Granted the user account the role needed for service account impersonation:
```
  $ gcloud projects add-iam-policy-binding alter-rebbe-2 \
    --member "user:dub@opentorah.org" --role "roles/iam.serviceAccountTokenCreator"
```

Logged into the service account:
```
  $ gcloud auth activate-service-account --key-file=/home/dub/.gradle/gcloudServiceAccountKey-alter-rebbe-2.json
```

To set it as a default account:
```
  $ gcloud config set account cloud-run-deploy@alter-rebbe-2.iam.gserviceaccount.com
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

Before the first job is created, service has to be initialized. I did it via Console
(in the same region - us-east4 - my service is running). This created an empty App Engine
application `alter-rebbe-2.appspot.com` and an empty bucket with the same name...

gcloud does not require time zone, but the Console does...

In October 2020, `--min-instances` option to gcloud run deploy became available (in beta);
in November 2020, I switched to using it (I estimate under $10 a month for one kept-warm instance).
If this works out, I won't need the CRON job anymore.

Another advantage of configuring property name instead of the key itself is: in CI environment,
the key is normally supplied (via an environment variable) only to the steps that need it;
by retrieving the key only when it is needed, we avoid...
