# base nix binaries
NIX	       = nix
NIX_BUILD  = nix-build
NIX_ENV	   = nix-env
NIX_STORE  = nix-store
NIX_GC	   = nix-collect-garbage

# nix macos enhancements binaries
DARWIN_PATH = /run/current-system
HOME_MANAGER_PATH = $(HOME)/.nix-profile/bin
DARWIN_REBUILD = $(DARWIN_PATH)/sw/bin/darwin-rebuild
HOME_MANAGER   = $(HOME_MANAGER_PATH)/home-manager

define message
	@echo
	@echo '┌────────────────────────────────────────────────────────────────────────────┐'
	@echo -n '│ >>> $(1)'
	@printf "%$$((72 - $(shell echo '$(1)' | wc -c)))s│\n"
	@echo '└────────────────────────────────────────────────────────────────────────────┘'
endef

help:
	@echo "all                     - nix reconfigure home and darwin"
	@echo "darwin                  - nix reconfigure darwin"
	@echo "home                    - nix reconfigure home"
	@echo "restart-skhd            - restart skhd"
	@echo "restart-yabai           - restart yabai"
	@echo "reset                   - nix reconfigure home/darwin and restart yabai, skhd"
	@echo "build                   - nix build file"

all: switch

build:
	$(call message,nix build -f . )
	@$(NIX) build -f .
	@rm -f result*

darwin:
	$(call message,darwin-rebuild switch)
	@$(DARWIN_REBUILD) switch -Q
	@echo "Darwin generation: $$($(DARWIN_REBUILD) --list-generations | tail -1)"

personal:
	$(call message,nix run nix-darwin -- switch --flake  ~/Code/dotfiles/nix-darwin)
	@darwin-rebuild switch --flake ~/Code/dotfiles/nix-darwin#Bens-MacBook-Pro --impure

work:
	$(call message,nix run nix-darwin -- switch --flake  ~/Code/dotfiles/nix-darwin)
	@darwin-rebuild switch --flake ~/Code/dotfiles/nix-darwin#zangao --impure

home:
	$(call message,home-manager switch)
	@$(HOME_MANAGER) switch
	@echo "Home generation: $$($(HOME_MANAGER) generations | head -1)"

switch: darwin home

reset: switch restart-skhd restart-yabai

restart-skhd:
	launchctl unload -w ~/Library/LaunchAgents/org.nixos.skhd.plist
	sleep 5
	launchctl load -w ~/Library/LaunchAgents/org.nixos.skhd.plist

restart-yabai:
	launchctl unload -w ~/Library/LaunchAgents/homebrew.mxcl.yabai.plist
	sleep 5
	launchctl load -w ~/Library/LaunchAgents/homebrew.mxcl.yabai.plist

# clean:
# 	$(call message,nix collect garbage)
# 	@$(NIX_GC) -d
