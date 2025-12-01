# Dotfiles and Configs

This repository contains the dotfiles and configuration files of my daily development Mac machine. Follow the README.md enclosed in the respective directories for information on how to setup the respective software.

## Installation

Clone this repository with submodules:

```bash
git clone --recurse-submodules git@github.com:DamianB-BitFlipper/dotfiles-and-configs.git ~/.config
```

If you've already cloned without submodules, initialize them with:

```bash
git submodule update --init --recursive
```

## Authentication

Some of the configurations rely on credentials. These should be stored in a `~/.authinfo` file with the format:

```text
machine <resource-uri> login <resource-login> password <resource-password>
```

The respective READMEs should state what the required authentication resources are.
