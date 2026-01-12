FROM mcr.microsoft.com/devcontainers/rust:2.0-trixie

RUN <<EOF
    set -e
    apt-get update && export DEBIAN_FRONTEND=noninteractive
    apt-get -y install --no-install-recommends just llvm clang ripgrep curl ca-certificates
    curl -fsSL https://deb.nodesource.com/setup_20.x | bash -
    apt-get install -y nodejs
    # apt-get clean
    # rm -rf /var/lib/apt/lists/*
EOF

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
    'PATH="$HOME/.npm-global/bin:$PATH"' \
    'autoload -Uz compinit && compinit' \
    'eval "$(codex completion zsh)"' \
    >> /etc/zsh/zshenv

# Run these commands as vscode to install user specific things...
USER vscode
WORKDIR /home/vscode

RUN rustup component add llvm-tools-preview && cargo install cargo-llvm-cov difftastic

RUN <<EOF
    set -e
    mkdir -p ~/.npm-global
    command -v npm
    npm config set prefix ~/.npm-global
    npm i -g @openai/codex
EOF

WORKDIR /workspaces

ENTRYPOINT []
CMD ["sh","-lc","sleep infinity"]

