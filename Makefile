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

all: switch

build:
	$(call message,nix build -f . )
	@$(NIX) build -f .
	@rm -f result*

darwin:
	$(call message,darwin-rebuild switch)
	@$(DARWIN_REBUILD) switch -Q
	@echo "Darwin generation: $$($(DARWIN_REBUILD) --list-generations | tail -1)"

home:
	$(call message,home-manager switch)
	@$(HOME_MANAGER) switch
	@echo "Home generation: $$($(HOME_MANAGER) generations | head -1)"

switch: darwin home

# clean:
# 	$(call message,nix collect garbage)
# 	@$(NIX_GC) -d
