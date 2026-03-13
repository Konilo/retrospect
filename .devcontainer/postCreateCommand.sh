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

# Install dev/exploratory packages (not tracked by renv)
echo 'Installing dev and exploratory R packages'
R -e "
renv::install('fPortfolio', prompt = FALSE)
renv::install('PortfolioAnalytics', prompt = FALSE)
renv::install('PerformanceAnalytics', prompt = FALSE)
renv::install('timeSeries', prompt = FALSE)
renv::install('ROI', prompt = FALSE)
renv::install('ROI.plugin.glpk', prompt = FALSE)
renv::install('ROI.plugin.quadprog', prompt = FALSE)
renv::install('CVXR', prompt = FALSE)
renv::install('profvis', prompt = FALSE)
renv::install('languageserver', prompt = FALSE)
install.packages('vscDebugger', repos = 'https://manuelhentschel.r-universe.dev')
"

# Install Claude Code CLI
echo 'Installing Claude Code CLI'
curl -fsSL https://claude.ai/install.sh | bash
