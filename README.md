# AgreenaRothC2

## Python wrapper
To avoid compatibility issues with local environments, a container is provided:
1. build the image `docker build -t r-test .`
2. log into the container `docker run -it r-test bash`


## Install requirements
```
ARG GOOGLE_APPLICATION_CREDENTIALS=/run/secrets/gsa_key
RUN --mount=type=secret,id=gsa_key \
    pip install keyring keyrings.google-artifactregistry.auth && \
    pip install -r requirements/base.txt```


```
pip install rothc-calculator==0.0.2 --extra-index-url https://europe-west1-python.pkg.dev/hummingbird-technologies/python/simple/```