FROM haskell:9.2.5
ARG USERNAME
RUN useradd -ms /bin/bash $USERNAME
USER $USERNAME
RUN stack update
COPY . /tmp
RUN cd /tmp && stack build --test --only-dependencies
