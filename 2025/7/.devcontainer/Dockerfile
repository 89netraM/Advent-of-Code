FROM haskell:9

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Create the user
RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME -s /bin/bash

# [Optional] Set the default user. Omit if you want to keep the default as root.
USER $USERNAME
SHELL ["/bin/bash", "-ec"]
ENTRYPOINT ["bash"]
