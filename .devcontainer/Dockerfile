FROM mcr.microsoft.com/devcontainers/rust:2.0-trixie

# Make zsh load workspace-controlled config (for all users)
# This avoids any postCreate/postStart commands.
RUN mkdir -p /etc/zsh && \
    printf '%s\n' \
    '' \
    '# Load repo zsh config if present' \
    'WS="${WORKSPACE_FOLDER:-}"' \
    '[ -z "$WS" ] && WS="$(pwd 2>/dev/null)"' \
    '[ -f "$WS/.devcontainer/zshrc.workspace" ] && source "$WS/.devcontainer/zshrc.workspace"' \
    'HISTFILE="$WS/.devcontainer/.zsh_history"' \
    >> /etc/zsh/zshenv

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive &&  apt-get -y install --no-install-recommends just &&  apt-get clean &&  rm -rf /var/lib/apt/lists/*

USER vscode
WORKDIR /home/vscode

RUN rustup component add llvm-tools-preview \
 && cargo install cargo-llvm-cov

WORKDIR /workspaces

ENTRYPOINT []
CMD ["sh","-lc","sleep infinity"]

