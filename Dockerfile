FROM rocker/r-ver:4.4.2 AS base

# The Rglpk R package, a dependency of the fPortfolio R package, requires
# libglpk40.
# The gsl R package, a dependency of the fPortfolio R package, requires
# libgsl27.
# The RsymphonyR package, a dependency of the the PortfolioAnalytics R package,
# requires coinor-symphony.
# git is needed to install vscDebugger.
RUN apt-get update && apt-get install -y \
    libglpk40 \
    libgsl27 \
    coinor-symphony \
    git

RUN R -e "install.packages('renv', version='1.1.1')"

WORKDIR /app

FROM base AS development
