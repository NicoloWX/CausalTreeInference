FROM ucsdets/scipy-ml-notebook:2021.3-stable

USER root

RUN apt-get update && apt-get -y install cmake

USER jovyan

COPY libraries.R ./

RUN Rscript libraries.R

RUN mkdir /workspace
ADD workspace /workspace

WORKDIR /workspace

CMD ["/bin/bash"]

