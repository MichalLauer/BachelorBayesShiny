Live odkaz: [https://shiny-hypotheses-538722985971.europe-west6.run.app/](https://shiny-hypotheses-538722985971.europe-west6.run.app/)

# Docker

docker build -t myshinyapp .
docker run -p 3838:3838 myshinyapp

# GCP

> gcloud cli
> Docker

gcloud auth configure-docker LOCATION-docker.pkg.dev
docker tag myshinyapp LOCATION-docker.pkg.dev/PROJECT_ID/shiny-hypotheses/shiny-hypotheses:latest
docker push LOCATION-docker.pkg.dev/PROJECT_ID/shiny-hypotheses/shiny-hypotheses:latest
