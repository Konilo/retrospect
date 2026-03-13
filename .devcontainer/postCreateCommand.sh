#!/bin/bash

# https://stackoverflow.com/a/19622569
trap 'exit' ERR

echo 'Running postCreateCommand.sh'

# The named volume mounted at ~/.claude is created by Docker as root-owned.
# Fix ownership so the vscode user can write into it before installing Claude Code.
sudo chown -R vscode:vscode /home/vscode/.claude

# Clean the renv library so stale symlinks from previous builds don't linger.
# The /renv/cache named volume persists across rebuilds, so restore just re-links
# cached packages without re-downloading them.
echo 'Cleaning renv library'
rm -rf renv/library

# Install R package dependencies via renv
echo 'Running renv::restore()'
R -e "renv::restore(prompt = FALSE)"

# Install dev/exploratory packages (not tracked by renv)
echo 'Installing dev and exploratory R packages'
R -e "
renv::install('profvis', prompt = FALSE)
renv::install('languageserver', prompt = FALSE)
install.packages('vscDebugger', repos = 'https://manuelhentschel.r-universe.dev')
"

# Install Claude Code CLI
echo 'Installing Claude Code CLI'
curl -fsSL https://claude.ai/install.sh | bash
