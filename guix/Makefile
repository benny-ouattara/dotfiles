# Resolve the absolute directory of THIS Makefile
HERE         := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

# Include environment variables from your .envrc
# Make automatically exports these to the shell commands below
-include $(HERE)/../.envrc

# File and Directory definitions
CHANNELS     := $(HERE)/channels.scm
SYSTEM_MOD   := $(HERE)/system/modules
SYSTEM       := $(HERE)/system/config.scm
HOME_MOD     := $(HERE)/home/modules
HOME_CFG     := $(HERE)/home/config.scm
REPL_INIT    := $(HERE)/repl-init.scm
DEPLOY_CONF  := $(HERE)/deploy.scm

# Standard Guix flags
GUIX_OPTS    := --cores=20 --max-jobs=10
TIME_MACHINE := guix time-machine -C $(CHANNELS) --

# Default action when running just 'make'
.DEFAULT_GOAL := help

# --- TARGETS ---

.PHONY: help pull system-build system-reconfigure system-container \
        home-reconfigure repl lint gc size graph deploy offload-check debug check

help: ## Display this help screen
	@echo "Guix Management Makefile"
	@echo "========================"
	@awk 'BEGIN {FS = ":.*##"; printf "\033[36m%-20s\033[0m %s\n", "Target", "Description"} /^[a-zA-Z_-]+:.*?##/ { printf "\033[36m%-20s\033[0m %s\n", $$1, $$2 }' $(MAKEFILE_LIST)

# -- Core Operations --

pull: ## Update channels and pull the latest Guix
	cd $(HERE) && $(TIME_MACHINE) pull -C $(CHANNELS)

system-build: ## Build the system derivation without installing
	cd $(HERE) && $(TIME_MACHINE) system build -L $(SYSTEM_MOD) $(SYSTEM)

system-reconfigure: ## Apply system configuration to the local machine
	cd $(HERE) && sudo GITHUB_TOKEN=$(GITHUB_TOKEN) $(TIME_MACHINE) system reconfigure -L $(SYSTEM_MOD) $(SYSTEM) $(GUIX_OPTS)

system-container: ## Spawn a guix system container for testing
	cd $(HERE) && $(TIME_MACHINE) system container -L $(SYSTEM_MOD) $(SYSTEM)

home-build: ## Apply home configuration to the local user
	cd $(HERE) && $(TIME_MACHINE) home build -L $(HOME_MOD) $(HOME_CFG) $(GUIX_OPTS)

home-container: ## Apply home configuration to the local user
	cd $(HERE) && $(TIME_MACHINE) home container -L $(HOME_MOD) $(HOME_CFG) $(GUIX_OPTS)

home-reconfigure: ## Apply home configuration to the local user
	cd $(HERE) && $(TIME_MACHINE) home reconfigure -L $(HOME_MOD) $(HOME_CFG) $(GUIX_OPTS)

deploy: ## Push configuration to a remote machine via SSH
	cd $(HERE) && $(TIME_MACHINE) deploy -L $(SYSTEM_MOD) $(DEPLOY_CONF)

# -- Maintenance & Inspection --

gc: ## Delete old generations (System, Home, User) and run garbage collector
	@echo "Cleaning up old generations..."
	guix package --delete-generations
	guix home delete-generations
	sudo guix system delete-generations
	guix gc

gc-safe: ## Delete generations older than 30 days and GC
	guix package --delete-generations=30d
	guix home delete-generations=30d
	sudo guix system delete-generations=30d
	guix gc

size: ## Show disk space occupied by the current system profile
	guix size $(shell readlink -f /run/current-system)

size-home: ## Show space occupied by the home profile
	guix size $(shell readlink ~/.guix-home/profile)

why: ## Find why a store item is being kept (usage: make why PATH=/gnu/store/...)
	guix gc --referrers $(PATH)

why-full: ## Trace the full dependency chain for a store item
	guix gc --referrers --recursive $(PATH)

watch: ## Automatically lint and build when .scm files change
	@echo "Watching for changes in .scm files..."
	@find $(HERE) -name "*.scm" | entr -c $(MAKE) -f $(lastword $(MAKEFILE_LIST)) check

lint: ## Check system and home configs for common errors
	cd $(HERE) && $(TIME_MACHINE) lint -L $(SYSTEM_MOD) $(SYSTEM) $(HOME_CFG)

graph: ## Generate a PDF visualization of service extensions
	cd $(HERE) && $(TIME_MACHINE) system extension-graph -L $(SYSTEM_MOD) $(SYSTEM) | dot -Tpdf > system-graph.pdf
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

health: ## Check the status of all Shepherd services and system logs
	@echo "--- Checking Shepherd Services ---"
	@herd status | grep "stopped" && echo "⚠️  Warning: Some services are stopped!" || echo "✅ All services are running."
	@echo ""
	@echo "--- Recent System Errors (Last 20 lines) ---"
	@sudo tail -n 20 /var/log/messages | grep -iE "error|fail|panic" || echo "✅ No recent errors found in /var/log/messages."

blast-radius: ## Show what depends on a specific service
	@echo "Generating dependency graph for services..."
	guix system shepherd-graph -L $(SYSTEM_MOD) $(SYSTEM) | dot -Tpdf > shepherd-deps.pdf
	@echo "Open shepherd-deps.pdf to see the 'Blast Radius' of your services."

rollback: ## Roll back to the previous system generation
	sudo guix system roll-back
	@echo "--- System Rolled Back ---"
	@$(MAKE) -f $(lastword $(MAKEFILE_LIST)) health

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

REMOTE_HOST := "new-machine"

bootstrap: ## Provision a new machine (Usage: make bootstrap REMOTE_HOST=192.168.1.50)
	@echo "--- Initializing Remote Guix System ---"
	# 1. Copy the current dotfiles to the new machine
	rsync -avz --exclude '.git' $(HERE) $(USER)@$(REMOTE_HOST):~/Code/dotfiles/
	# 2. Run the remote pull and reconfigure
	ssh $(USER)@$(REMOTE_HOST) "cd ~/Code/dotfiles/guix && \
		guix pull -C channels.scm && \
		sudo guix system reconfigure otter-system.scm"
	@echo "✅ System provisioned. Now run 'make restore' on the new machine."

install-hooks: ## Install git pre-commit hooks
	@echo "Installing git hooks..."
	@ln -sf $(HERE)/scripts/validate.sh .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@echo "✅ Hooks installed."

# Add libnotify to your manifest to ensure notify-send is available
done: ## Send a desktop notification
	@notify-send "Guix System" "Deployment on $(NAME) complete! 🦦" --icon=system-software-update

up: system-reconfigure done ## Reconfigure and notify when finished
