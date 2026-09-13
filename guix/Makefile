# Resolve THIS Makefile before anything else can change MAKEFILE_LIST
SELF         := $(realpath $(lastword $(MAKEFILE_LIST)))
HERE         := $(patsubst %/,%,$(dir $(SELF)))

# The private jazacash channel needs GITHUB_TOKEN. Prefer the environment
# (direnv); otherwise source .envrc with a real shell, since Make would
# mangle values containing $
ENVRC        := $(HERE)/../.envrc
ifndef GITHUB_TOKEN
GITHUB_TOKEN := $(shell [ -f $(ENVRC) ] && . $(ENVRC) >/dev/null 2>&1 && printf '%s' "$$GITHUB_TOKEN")
endif
export GITHUB_TOKEN

# File and Directory definitions
CHANNELS     := $(HERE)/channels.scm
SYSTEM_MOD   := $(HERE)/system/modules
SYSTEM       := $(HERE)/system/config.scm
HOME_MOD     := $(HERE)/home/modules
HOME_CFG     := $(HERE)/home/config.scm
REPL_INIT    := $(HERE)/repl-init.scm
DEPLOY_CONF  := $(HERE)/deploy.scm

# Standard Guix flags
GUIX_OPTS    := --cores=$(shell nproc) --max-jobs=4
TIME_MACHINE := guix time-machine --unsafe-channel-evaluation -C $(CHANNELS) --
# Extra tools (entr, graphviz) for the few targets that need them
TOOLS        := guix shell -m $(HERE)/guix.scm --

# Default action when running just 'make'
.DEFAULT_GOAL := help

# --- TARGETS ---

.PHONY: help pull system-build system-reconfigure system-container \
        home-build home-container home-reconfigure deploy up \
        gc gc-safe size size-home why why-full watch lint graph repl shell \
        offload-check gc-links news debug health blast-radius rollback \
        service-log status history check backup restore bootstrap \
        install-hooks nix-install nix-update nix-gc

help: ## Display this help screen
	@echo "Guix Management Makefile"
	@echo "========================"
	@awk 'BEGIN {FS = ":.*##"; printf "\033[36m%-20s\033[0m %s\n", "Target", "Description"} /^[a-zA-Z_-]+:.*?##/ { printf "\033[36m%-20s\033[0m %s\n", $$1, $$2 }' $(SELF)

# -- Core Operations --

pull: ## Pull Guix using the channels in channels.scm
	guix pull --unsafe-channel-evaluation -C $(CHANNELS)

system-build: ## Build the system derivation without installing
	cd $(HERE) && $(TIME_MACHINE) system build -L $(SYSTEM_MOD) $(SYSTEM)

system-reconfigure: ## Apply system configuration to the local machine
	cd $(HERE) && sudo GITHUB_TOKEN=$(GITHUB_TOKEN) $(TIME_MACHINE) system reconfigure -L $(SYSTEM_MOD) $(SYSTEM) $(GUIX_OPTS)

system-container: ## Spawn a guix system container for testing
	cd $(HERE) && $(TIME_MACHINE) system container -L $(SYSTEM_MOD) $(SYSTEM)

home-build: ## Build the home configuration without activating it
	cd $(HERE) && $(TIME_MACHINE) home build -L $(HOME_MOD) $(HOME_CFG) $(GUIX_OPTS)

home-container: ## Spawn a container running the home configuration
	cd $(HERE) && $(TIME_MACHINE) home container -L $(HOME_MOD) $(HOME_CFG) $(GUIX_OPTS)

home-reconfigure: ## Apply home configuration to the local user
	cd $(HERE) && $(TIME_MACHINE) home reconfigure -L $(HOME_MOD) $(HOME_CFG) $(GUIX_OPTS)

# The remote needs root SSH access and this machine's signing key authorized
# (guix archive --authorize). system/config.scm is otter-specific (hostname,
# root disk UUID), so point DEPLOY_OS at a config written for the target.
deploy: ## Deploy an OS config over SSH (usage: make deploy REMOTE_HOST=1.2.3.4 DEPLOY_OS=machine.scm)
	$(if $(REMOTE_HOST),,$(error Set REMOTE_HOST=host))
	$(if $(DEPLOY_OS),,$(error Set DEPLOY_OS=path/to/machine.scm))
	cd $(HERE) && DEPLOY_HOST=$(REMOTE_HOST) DEPLOY_OS=$(abspath $(DEPLOY_OS)) $(TIME_MACHINE) deploy -L $(SYSTEM_MOD) $(DEPLOY_CONF)

up: ## Reconfigure the system and notify on success or failure
	@if $(MAKE) -f $(SELF) system-reconfigure; then \
		notify-send "Guix System" "Reconfigure on $$(hostname) complete! 🦦" --icon=system-software-update; \
	else \
		notify-send -u critical "Guix System" "Reconfigure on $$(hostname) failed"; exit 1; \
	fi

# -- Maintenance & Inspection --

gc: ## Delete ALL old generations (System, Home, User) and run garbage collector
	@printf "Delete every old generation? Rollback will no longer be possible. [y/N] "; \
		read ans; [ "$$ans" = y ] || [ "$$ans" = Y ] || { echo "Aborted."; exit 1; }
	@echo "Cleaning up old generations..."
	guix package --delete-generations
	guix home delete-generations
	sudo guix system delete-generations
	guix gc

gc-safe: ## Delete generations older than 30 days and GC
	guix package --delete-generations=30d
	guix home delete-generations 30d
	sudo guix system delete-generations 30d
	guix gc

size: ## Show disk space occupied by the current system profile
	guix size $(shell readlink -f /run/current-system)

size-home: ## Show space occupied by the home profile
	guix size $(shell readlink ~/.guix-home/profile)

why: ## Find why a store item is being kept (usage: make why ITEM=/gnu/store/...)
	guix gc --referrers $(ITEM)

why-full: ## Trace the full dependency chain for a store item (usage: make why-full ITEM=/gnu/store/...)
	guix gc --referrers --recursive $(ITEM)

watch: ## Automatically lint and build when .scm files change
	@echo "Watching for changes in .scm files..."
	@find $(HERE) -name "*.scm" | $(TOOLS) entr -c $(MAKE) -f $(SELF) check

lint: ## Dry-run the system and home builds to catch config errors
	cd $(HERE) && $(TIME_MACHINE) system build -n -L $(SYSTEM_MOD) $(SYSTEM)
	cd $(HERE) && $(TIME_MACHINE) home build -n -L $(HOME_MOD) $(HOME_CFG)

graph: ## Generate a PDF visualization of service extensions
	cd $(HERE) && $(TIME_MACHINE) system extension-graph -L $(SYSTEM_MOD) $(SYSTEM) | $(TOOLS) dot -Tpdf > system-graph.pdf
	@echo "Graph saved to system-graph.pdf"

repl: ## Open a Guix REPL with local modules pre-loaded
	cd $(HERE) && guix shell -m guix.scm -- guix repl -L $(SYSTEM_MOD) -i $(REPL_INIT)

shell: ## Enter a shell with all tools needed for this Makefile
	cd $(HERE) && guix shell -m guix.scm

# -- Diagnostics --

offload-check: ## Verify connectivity to remote build machines
	guix offload test

gc-links: ## Remove temporary failed build links and unreachable roots
	guix gc --list-dead

news: ## Display news from all configured channels
	cd $(HERE) && $(TIME_MACHINE) extension-graph --version > /dev/null 2>&1 || true # Warm up the time machine
	guix pull -C $(CHANNELS) --news

debug: ## Print internal Makefile variables for troubleshooting
	@echo "--- Makefile Variables ---"
	@echo "HERE:       $(HERE)"
	@echo "CHANNELS:   $(CHANNELS)"
	@echo "SYSTEM:     $(SYSTEM)"
	@echo "--- Environment Check ---"
	@echo "PATH:       $(shell echo $$PATH | cut -d: -f1-3)..."

health: ## Check the status of system and user Shepherd services and system logs
	@echo "--- Checking Shepherd Services ---"
	@for herd in "sudo herd" herd; do \
		if ! out=$$($$herd status 2>&1); then echo "⚠️  Warning: could not query $$herd"; continue; fi; \
		stopped=$$(printf '%s\n' "$$out" | awk '/^Stopped:/ {s=1; next} /^[^ ]/ {s=0} s'); \
		if [ -n "$$stopped" ]; then echo "⚠️  Warning: stopped services ($$herd):"; echo "$$stopped"; \
		else echo "✅ All services are running ($$herd)."; fi; \
	done
	@echo ""
	@echo "--- Recent System Errors (Last 20 lines) ---"
	@sudo tail -n 20 /var/log/messages | grep -iE "error|fail|panic" || echo "✅ No recent errors found in /var/log/messages."

blast-radius: ## Show what depends on a specific service
	@echo "Generating dependency graph for services..."
	guix system shepherd-graph -L $(SYSTEM_MOD) $(SYSTEM) | $(TOOLS) dot -Tpdf > shepherd-deps.pdf
	@echo "Open shepherd-deps.pdf to see the 'Blast Radius' of your services."

rollback: ## Roll back to the previous system generation
	sudo guix system roll-back
	@echo "--- System Rolled Back ---"
	@$(MAKE) -f $(SELF) health

service-log: ## Tail logs for a specific shepherd service (usage: make service-log S=my-watcher)
	@echo "Tailing logs for service: $(S)..."
	@sudo grep "$(S)" /var/log/messages | tail -n 50

status: ## Show current system generation and channel commits
	@echo "--- Current System Status ---"
	@guix system describe
	@echo ""
	@echo "--- Channel Provenance ---"
	@guix describe

history: ## List all previous system generations
	guix system list-generations

check: debug lint ## Run pre-flight diagnostics (debug + lint)

# -- Backups --

BACKUP_DIR := $(HERE)/backups
TIMESTAMP  := $(shell date +%Y%m%d)
SECRET_FILE := $(BACKUP_DIR)/secrets-$(TIMESTAMP).tar.gz.gpg

backup: ## Encrypt and backup SSH/GPG keys to $(BACKUP_DIR)
	@mkdir -p $(BACKUP_DIR)
	@echo "Creating encrypted backup of secrets..."
	@tar -czf - -C $(HOME) .ssh .gnupg | \
		gpg --symmetric --cipher-algo AES256 -o $(SECRET_FILE)
	@echo "✅ Backup created: $(SECRET_FILE)"
	@echo "⚠️  Keep this file off your main drive (USB or Cloud)!"

restore: ## Decrypt and restore secrets from a backup file (usage: make restore FILE=path/to/gpg)
	@if [ -z "$(FILE)" ]; then echo "Error: Provide FILE=path/to/file.gpg"; exit 1; fi
	@echo "Restoring secrets from $(FILE)..."
	@gpg -d $(FILE) | tar -xzf - -C $(HOME)
	@chmod 700 $(HOME)/.ssh $(HOME)/.gnupg
	@echo "✅ Secrets restored. Check 'up health' to ensure SSH is working."

# -- Bootstrap --

bootstrap: ## Provision a new machine (usage: make bootstrap REMOTE_HOST=192.168.1.50 DEPLOY_OS=machine.scm)
	$(if $(REMOTE_HOST),,$(error Set REMOTE_HOST=host))
	$(if $(DEPLOY_OS),,$(error Set DEPLOY_OS=path/to/machine.scm))
	@echo "--- Initializing Remote Guix System ---"
	# 1. Copy the current dotfiles to the new machine
	rsync -avz --exclude '.git' $(HERE) $(USER)@$(REMOTE_HOST):~/Code/dotfiles/
	# 2. Build here (channels and token stay local) and push the system over SSH
	$(MAKE) -f $(SELF) deploy
	@echo "✅ System provisioned. Now run 'make restore' on the new machine."

install-hooks: ## Install git pre-commit hooks
	@echo "Installing git hooks..."
	@hooks=$$(git -C $(HERE) rev-parse --path-format=absolute --git-path hooks) && \
		mkdir -p "$$hooks" && ln -sf $(HERE)/scripts/validate.sh "$$hooks/pre-commit"
	@chmod +x $(HERE)/scripts/validate.sh
	@echo "✅ Hooks installed."

# -- Nix --

NIX_FLAKE := path:$(HERE)

nix-install: ## Install/update nix packages from flake.nix
	nix profile install $(NIX_FLAKE)#default --refresh

nix-update: ## Update nix flake inputs and reinstall
	nix flake update --flake $(NIX_FLAKE)
	nix profile upgrade guix

nix-gc: ## Garbage collect unused nix store paths
	nix store gc
