Running on GCP
==============

I am using [Cloud Run](https://cloud.google.com/run#key-features).
- created service `collector` in us-eat4;
- allow unauthenticated;
- concurrency 80; 1 CPU; 256Mb; up to 3 instances; request timeout 30 sec;
- service account: `940416907592-compute@developer.gserviceaccount.com`;
- got URL `https://collector-qfkasghxtq-uk.a.run.app`;
- mapped `app.alter-rebbe.org` to it (app CNAME ghs.googlehosted.com. in domains.google.com);  

I am using Cloud Storage for the facsimiles.

Memorystore/Redis turned out to be too expensive (they charge for provisioned capacity),
so the fact that Cloud Run probably can't talk to it even now (4/2020) isn't important :(
Maybe I should use Cloud Storage or Cloud Firestore for caching generated files?
Disk in the Cloud Run instances is just memory...

QUESTION: What is the difference between Cloud Storage and Cloud Firestore?

I am using [jib](https://github.com/GoogleContainerTools/jib).
To run the container locally, I do `$ ./gradlew jibDockerBuild` (and then `$ docker run ...`);
to push to Cloud Run via Container Registry, I do `$ ./gradlew jib`.
My image is `gcr.io/alter-rebbe/collector`.

I do not see the need to set up [Cloud Build](https://cloud.google.com/cloud-build),
but if I do - it runs locally too.

To make docker work locally, [I had to](https://linuxconfig.org/how-to-install-docker-on-fedora-31):
  Revert back to cgroup v1.
  Please consider whether this step does not negatively affect your any of the other services running on your system:
```
  $ sudo dnf install -y grubby
  $ sudo grubby --update-kernel=ALL --args="systemd.unified_cgroup_hierarchy=0"
  $ sudo reboot
```

QUESTION: Artifact Registry - how is it better than Bintray?


Monitoring
----------    

Stackdriver
  - Logging: supports Logback and java.util.logging;
    I use slf4j - with what back-end (slf4j-jdk14.jar)?
    And how do I configure it to work with Cloud Logging?
    'com.google.cloud:google-cloud-logging:1.101.1'?
    Switch to log4s?
    Handle http4s access log...

  - Monitoring
  - Error reporting

Cloud Debugger

Cloud Profiler


Extra
-----

Cloud Pub/Sub

Firebase Hosting (CDN)
