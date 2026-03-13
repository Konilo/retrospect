#!/bin/bash

# https://stackoverflow.com/a/19622569
trap 'exit' ERR

echo 'Running postCreateCommand.sh'

# The named volume mounted at ~/.claude is created by Docker as root-owned.
# Fix ownership so the vscode user can write into it before installing Claude Code.
sudo chown -R vscode:vscode /home/vscode/.claude

# Install R package dependencies via renv
echo 'Running renv::restore()'
R -e "renv::restore()"

# Install Claude Code CLI
echo 'Installing Claude Code CLI'
curl -fsSL https://claude.ai/install.sh | bash
