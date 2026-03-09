# Paths and Binaries
FLAKE_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
LOG_FILE  := $(HOME)/.logs/nix_telemetry.csv

# Binaries
MAKE         ?= make
SHELL        := $(shell which bash)
DARWIN_REB   := $(shell which darwin-rebuild 2>/dev/null || echo "/run/current-system/sw/bin/darwin-rebuild")
HOME_MAN     := $(HOME)/.nix-profile/bin/home-manager


# Colors for the help menu
CYAN  := $(shell printf "\033[36m")
RESET := $(shell printf "\033[0m")

.DEFAULT_GOAL := help

help: ## Show this help menu
	@echo "Usage: up <target>"
	@echo ""
	@echo "Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  $(CYAN)%-20s$(RESET) %s\n", $$1, $$2}'

check-sudo:
	@sudo -v

## --- Core Deployment ---

all: darwin home ## Reconfigure both Darwin (System) and Home-Manager

darwin: ## Reconfigure nix-darwin (System level)
	@echo "$(CYAN)>>> Building Darwin System...$(RESET)"
	@sudo -v
	@sudo $(DARWIN_REB) switch --flake $(FLAKE_DIR) --impure
	@echo "Darwin generation: $$($(DARWIN_REB) --list-generations | tail -1)"

home: ## Reconfigure home-manager (User level)
	@echo "$(CYAN)>>> Building Home Environment...$(RESET)"
	@$(HOME_MAN) switch --flake $(FLAKE_DIR)
	@echo "Home generation: $$($(HOME_MAN) generations | head -1)"

## --- Flake Profiles ---

kite: check-sudo ## Switch to 'kite' flake profile (Impure)
	@echo "$(CYAN)>>> Applying Kite Flake (kite)...$(RESET)"
	@sudo darwin-rebuild switch --flake $(FLAKE_DIR)#kite --impure

onyx: check-sudo ## Switch to 'onyx' flake profile (Impure)
	@echo "$(CYAN)>>> Applying Onyx Flake (zo)...$(RESET)"
	@sudo darwin-rebuild switch --flake $(FLAKE_DIR)#zo --impure

%!: ## Suffix with ! to run a profile (e.g., make kite!)
	@echo "$(CYAN)>>> Activating profile: $*...$(RESET)"
	@sudo -v
	@sudo $(DARWIN_REB) switch --flake $(FLAKE_DIR)#$* --impure --show-trace

## --- Services & Window Management ---

reset: all restart-skhd restart-yabai ## Full system refresh + restart Yabai/SKHD

restart-skhd: ## Restart skhd
	@echo "$(CYAN)>>> Restarting skhd...$(RESET)"
	@launchctl kickstart -k gui/$(shell id -u)/org.nixos.skhd 2>/dev/null || true

restart-yabai: ## Restart yabai
	@echo "$(CYAN)>>> Restarting yabai...$(RESET)"
	@launchctl kickstart -k gui/$(shell id -u)/homebrew.mxcl.yabai 2>/dev/null || true

restart-%: ## Restart a service (e.g., make restart-yabai)
	@echo "$(CYAN)>>> High-kick $*(RESET)"
	@launchctl kickstart -k gui/$(shell id -u)/org.nixos.$* 2>/dev/null || \
	 launchctl kickstart -k gui/$(shell id -u)/homebrew.mxcl.$*

## --- Maintenance ---

build: ## Dry build current directory and cleanup results
	@echo "$(CYAN)>>> Running nix build...$(RESET)"
	@nix build -f .
	@rm -f result*

gc: ## Collect Nix garbage and delete old generations
	@echo "$(CYAN)>>> Collecting garbage...$(RESET)"
	@nix-collect-garbage -d

clean-safe: ## Delete system generations older than 60 days
	@echo "$(CYAN)>>> Deleting old generations (60d+)...$(RESET)"
	@sudo nix-env --delete-generations +60d -p /nix/var/nix/profiles/system
	@nix-collect-garbage

# Quick way to see what's taking up space in the Nix store
size: ## Show the size of the current system closure
	@nix path-info -Sh /run/current-system

# View the diff between the current and previous generation
diff: ## Show what changed in the last switch
	@nix store diff-closures /nix/var/nix/profiles/system-$(shell expr $(shell readlink /nix/var/nix/profiles/system | cut -d- -f2) - 1)-link /run/current-system

health: ## Check Nix daemon, Rosetta, and Store health
	@echo "$(CYAN)>>> Checking Nix Daemon...$(RESET)"
	@ps aux | grep -v grep | grep nix-daemon > /dev/null && echo "✅ Daemon is running" || echo "❌ Daemon is DOWN"

	@echo "$(CYAN)>>> Checking Rosetta 2 (x86_64 emulation)...$(RESET)"
	@arch -x86_64 /usr/bin/true 2>/dev/null && echo "✅ Rosetta is working" || echo "⚠️  Rosetta not active/installed"

	@echo "$(CYAN)>>> Verifying Nix Store (Read-only check)...$(RESET)"
	@nix-store --verify --check-contents
	@echo "✅ Store integrity verified."

repair: ## Repair Nix store (Requires sudo)
	@echo "$(CYAN)>>> Repairing Nix Store...$(RESET)"
	@sudo nix-store --verify --check-contents --repair

rosetta: ## Force install Rosetta 2
	@echo "$(CYAN)>>> Installing Rosetta 2...$(RESET)"
	@sudo softwareupdate --install-rosetta --agree-to-license

list: check-sudo ## List system generations with dates
	@sudo nix-env --list-generations -p /nix/var/nix/profiles/system

check-git: ## Ensure no uncommitted changes in $(FLAKE_DIR)
	@cd $(FLAKE_DIR) && git diff --quiet || (echo "❌ Uncommitted changes in $(FLAKE_DIR)! Commit them first." && exit 1)

status: check-sudo ## Show Nix store size and generation count
	@echo "$(CYAN)>>> Nix Store Status$(RESET)"
	@sudo du -sh /nix 2>/dev/null || echo "Nix Store: Access Denied (use sudo)"
	@echo -n "Generations: "
	@sudo nix-env --list-generations -p /nix/var/nix/profiles/system | wc -l
	@echo ""
	@echo "$(CYAN)>>> Recent System History$(RESET)"
	@tail -n 3 $(LOG_FILE) | column -t -s ','
	@echo ""
	@echo "$(CYAN)>>> Disk Pressure$(RESET)"
	@sudo df -h /nix | tail -1 | awk '{print "Usage: " $$5 " (Free: " $$4 ")"}'

why: ## Explain why a path is in the store (Usage: make why PKG=...)
	@nix-store --query --referrers-closure $(PKG)

install-hooks: ## Install git pre-commit hooks
	@echo "Installing git hooks..."
	@ln -sf $(HERE)/scripts/validate.sh .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@echo "✅ Hooks installed."

dry-run: ## Show what would change in the next switch
	@echo "$(CYAN)>>> Building new derivation (without activating)...$(RESET)"
	$(eval NEW_PATH := $(shell nix build $(FLAKE_DIR)#darwinConfigurations.kite.system --no-link --print-out-paths --impure))
	$(eval OLD_PATH := $(shell readlink /run/current-system))
	@echo "$(CYAN)>>> Comparing current system to $(NEW_PATH)...$(RESET)"
	@nix shell nixpkgs#nix-diff -c nix-diff $(OLD_PATH) $(NEW_PATH)

diff-last: ## Compare current generation with the previous one
	@nix store diff-closures /nix/var/nix/profiles/system-$(shell expr $(shell readlink /nix/var/nix/profiles/system | cut -d- -f2) - 1)-link /run/current-system

telemetry: ## Record current system size and package count
	@mkdir -p $(shell dirname $(LOG_FILE))
	@if [ ! -f $(LOG_FILE) ]; then echo "date,gen,paths,size_gb" > $(LOG_FILE); fi
	@echo -n "$$(date +%Y-%m-%d)," >> $(LOG_FILE)
	@echo -n "$$(readlink /nix/var/nix/profiles/system | cut -d- -f2)," >> $(LOG_FILE)
	@echo -n "$$(nix-store --query --requisites /run/current-system | wc -l | tr -d ' ')," >> $(LOG_FILE)
	@echo "$$(du -sg /nix/store | awk '{print $$1}')" >> $(LOG_FILE)
	@echo "$(CYAN)>>> Telemetry recorded to $(LOG_FILE)$(RESET)"

find-config: ## Advanced search using ripgrep (Usage: make find-config QUERY=...)
	@echo "$(CYAN)>>> Searching for '$(QUERY)' using ripgrep...$(RESET)"
	@rg -L "$(QUERY)" /etc /run/current-system/etc

index-update: ## Update the nix-index database
	@echo "$(CYAN)>>> Updating nix-index...$(RESET)"
	@nix-index

rollback: ## Rollback to the previous system generation
	@echo "$(CYAN)>>> Rolling back to previous generation...$(RESET)"
	@sudo $(DARWIN_REB) rollback
	@echo "$(CYAN)>>> Restarting services...$(RESET)"
	@$(MAKE) reset

update: ## Update flake.lock inputs
	@echo "$(CYAN)>>> Updating Flake inputs...$(RESET)"
	@nix flake update $(FLAKE_DIR)

upgrade: update kite ## Update pins and apply kite profile

## --- Remote Builds ---

# Self-detecting the binary (standard in Make)
# MAKE := make

# Configuration for Remote Builder (Otter)
REMOTE_BUILDER := otter.local
BUILD_USERS_GROUP := nixbld

remote-build: ## Build the current flake on a remote Linux machine (Otter)
	@echo "$(CYAN)>>> Offloading build to $(REMOTE_BUILDER)...$(RESET)"
	@nix build .#darwinConfigurations.kite.system \
		--eval-store auto \
		--store ssh-ng://$(REMOTE_BUILDER) \
		--builders-use-substitutes \
		--show-trace

## --- Diagnostics & Benchmarking ---

bench-keys: ## Verify keyboard repeat rates match Nix config
	@echo "$(CYAN)>>> Running Keyboard Benchmark...$(RESET)"
	@nix shell nixpkgs#coreutils --command bash -c "$(key-benchmark)"

health-wm: ## Check if yabai has proper macOS permissions
	@echo "$(CYAN)>>> Checking Window Manager Health...$(RESET)"
	@$(wm-health-check)

trace: ## Trace why a package is in your system (Usage: make trace PKG=...)
	@echo "$(CYAN)>>> Finding paths that depend on $(PKG)...$(RESET)"
	@nix-store --query --referrers-closure $$(which $(PKG) || echo $(PKG)) | xargs nix-store --query --referrers

tree: ## Visual tree of system dependencies (requires nix-tree)
	@nix shell nixpkgs#nix-tree -c nix-tree /run/current-system

commit: ## Stage and commit all changes
	@git add .
	@read -p "Commit message: " msg; \
	git commit -m "$$msg"

ship: commit ## Commit and then deploy
	@$(MAKE) kite!
