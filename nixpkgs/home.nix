{ config, pkgs, ... }:

let
  home-directory = builtins.getEnv "HOME";
  tmp-directory = "/tmp";
  lib = pkgs.lib;
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  dotrcs = import ./dotrcs.nix {};
in rec {
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;

      permittedInsecurePackages = [
        "openssl-1.0.2u"
      ];
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.03";

  home.file = {
    ".curlrc".text = ''
          capath=${pkgs.cacert}/etc/ssl/certs/
          cacert=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
        '';

    ".vimrc".text = ''
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'chriskempson/base16-vim'
Plug 'arcticicestudio/nord-vim'
Plug 'rakr/vim-one'
call plug#end()

" Location for installing plugins
let mapleader=","

" Indent Guides - enable plugin
let g:indent_guides_enable_on_vim_startup = 1

" Integrate fzf via Homebrew
set rtp+=/usr/local/opt/fzf
" Integrate fzf via install on servers
set rtp+=~/.fzf

" Shortcuts for using fzf in vim
nnoremap <leader>f :Files<cr>
nnoremap <leader>g :GFiles<cr>
nnoremap <leader>t :Tags<cr>
" Focus on a particular window
noremap <leader>w :Windows<cr>
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>h :History<cr>
nnoremap <leader>r :Rg<cr>
" RG for word under cursor
nnoremap <silent> <Leader>ur :Rg <C-R><C-W><CR>
nnoremap <leader>v :Commands<cr>
" Shortcuts for opening up results in splits
let g:fzf_action = {
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit' }

" Surround-vim addition to delete a function
nmap <silent> dsf ds)db

" Vim-better-whitespace always automatically remove
let g:strip_whitespace_on_save=1
" Do not confirm
let g:strip_whitespace_confirm=0

" Hide buffers instead of closing them
set hidden
set history=5000
" Use many muchos levels of undo
set undolevels=1000

" Write even though you did not sudo to begin with: w!!
cmap w!! w !sudo tee % >/dev/null


colorscheme dracula
" colorscheme nord
" colorscheme one
" set background=dark
" set background=light

" Override background color to be as dark as the terminal
highlight Normal guibg=Black
" Highlight non-ascii characters them in an obvious color
syntax match nonascii "[^\x00-\x7F]"
highlight nonascii guibg=Green ctermbg=2

" Enable line numbering
set number
set relativenumber

" Turn click-me warnings about swapfiles into discreet little messages
set shortmess+=A

" Enable backup
set backup
set backupdir=~/.local/share/nvim/backup

" Undo even after leaving vim
set undofile

set hlsearch
" Removes highlighting from last search
nmap <silent> <leader>/ :nohlsearch<CR>

" Display long lines as just one line (you have to scroll horizontally)
set nowrap
" Tabsize
set tabstop=2
" Reindent operations (<< and >>)
set shiftwidth=2
set expandtab
" Display spaces and tabs and oddities (like non-breaking white-space)
set list
set smartindent
set autoindent
set textwidth=80

" Search as characters are entered
set incsearch
" Ignore case when searching
set ignorecase

" Ignore hated files generally at vim level (Some autocomplete engines picks up
" on and add their own ignored files to the mix)
set wildignore+=*.swp,*.bak,*.pyc,*.class,*/tmp/*

map <leader>n :NERDTreeToggle<CR>

" Automatically reload .vimrc file on save
augroup myvimrc
  au!
  au BufWritePost .vimrc so ~/.vimrc
augroup END

set nospell
autocmd filetype markdown,txt set spell spelllang=en_us

set clipboard=unnamedplus

" This unsets the 'last search pattern'. Register by hitting return
nnoremap <CR> :noh<CR><CR>

" Mostly for solving git merge conflicts
:set diffopt=vertical

imap jj <ESC>

autocmd filetype json,blade,eruby setlocal nofoldenable
" Set folder color to white
highlight Folded guifg=white

" Delete buffer and move to the next one without closing split
nmap <leader>d :bp\|bd #<cr>
nmap <leader>z :cd %:p:h<cr>\| :NERDTreeCWD<cr>

" Save my fingers when saving
nmap <space>w :w<cr>

" Copy current path into the paste buffer
nmap <space>fp :let @+=expand("%:p")<cr>

" Automatically reload file when underlying files change (e.g. git)
set autoread

" By default, swap out all instances in a line during substitutions
set gdefault

" Remove excess # when joining two lines of comments
set formatoptions+=j

" Keep same flags when repeating a substitution
nnoremap & :&&<CR>
xnoremap & :&&<CR>

" Swap the word the cursor is on with the next word (which can be on a
" newline, and punctuation is 'skipped'):
nmap <silent> gw "_yiw:s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR>

" Use <Leader>Esc to leave terminal mode
tnoremap <Leader><Esc> <C-\><C-n>
tnoremap <Leader>jj <C-\><C-n>

" Shortcuts for frequently accessed files
command! Vimrc e ~/.vimrc
command! Zshrc e ~/.zshrc
command! Gitconfig e ~/.gitconfig
        '';
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.bash = {
    enable = true;
    bashrcExtra = lib.mkBefore ''
        source /etc/bashrc

        if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi # required by nix to setup various paths
      '';
  };

  programs.git = {
    enable = true;
    #userName  = "Ben O.";
    # userEmail = "benny.ouattara@gmail.com";

    aliases = {
      amend      = "commit --amend -C HEAD";
      authors    = "!\"${pkgs.git}/bin/git log --pretty=format:%aN"
                   + " | ${pkgs.coreutils}/bin/sort"
                   + " | ${pkgs.coreutils}/bin/uniq -c"
                   + " | ${pkgs.coreutils}/bin/sort -rn\"";
      b          = "branch --color -v";
      ca         = "commit --amend";
      clone      = "clone --recursive";
      co         = "checkout";
      cp         = "cherry-pick";
      dc         = "diff --cached";
      dh         = "diff HEAD";
      ds         = "diff --staged";
      undo       = "reset --soft HEAD^";
      w          = "status -sb";
      wdiff      = "diff --color-words";
      l          = "log --graph --pretty=format:'%Cred%h%Creset"
                   + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
                   + " --abbrev-commit --date=relative --show-notes=*";
    };

    extraConfig = {
      github.user           = "benny-ouattara";

      color = {
        status      = "auto";
        diff        = "auto";
        branch      = "auto";
        interactive = "auto";
        ui          = "auto";
        sh          = "auto";
      };

      "url \"git@github.com:benny-ouattara/sp-hosts.el.git\"".insteadOf
      = "https://github.com/benny-ouattara/sp-hosts.el.git";

      "url \"git@github.com:\"".insteadOf
      = "https://github.com/";
    };

    ignores = [
      "*.elc"
      ".clean"
      ".direnv"
      "TAGS"
      "result"
      "result-*"
      "tags"
    ];
  };

  programs.zsh = rec {
    enable = true;
    oh-my-zsh = {
      enable = true;
      theme = "af-magic";
    };

    dotDir = ".config/zsh";
    enableCompletion = true;
    enableAutosuggestions = true;

    history = {
      size = 50000;
      save = 500000;
      path = "${dotDir}/history";
      ignoreDups = true;
      share = true;
    };

    sessionVariables = {
      ALTERNATE_EDITOR  = "${pkgs.vim}/bin/vi";
      LC_CTYPE          = "en_US.UTF-8";
      LESS              = "-FRSXM";
      PROMPT            = "%m %~ $ ";
      PROMPT_DIRTRIM    = "2";
      RPROMPT           = "";
      WORDCHARS         = "";
    };

    shellAliases = {
      ls     = "${pkgs.coreutils}/bin/ls --color=auto -alhrt";
      l      = "${pkgs.coreutils}/bin/ls --color=auto -alhrt";
      rX     = "${pkgs.coreutils}/bin/chmod -R ugo+rX";
      gr     = "${pkgs.git}/bin/git rm -r --cached";
      proc   = "${pkgs.darwin.ps}/bin/ps axwwww | ${pkgs.gnugrep}/bin/grep -i";
      nstat  = "${pkgs.darwin.network_cmds}/bin/netstat -nr -f inet"
               + " | ${pkgs.gnugrep}/bin/egrep -v \"(lo0|vmnet|169\\.254|255\\.255)\""
               + " | ${pkgs.coreutils}/bin/tail -n +5";
      sbcl   = "${pkgs.rlwrap}/bin/rlwrap sbcl";
    };

    profileExtra = ''
        . ${pkgs.z}/share/z.sh
      '';

    initExtra = lib.mkBefore ''
        ZSH_DISABLE_COMPFIX=true

        eval "$(starship init zsh)"

        eval "$(jenv init -)"
        export PATH=$PATH:$HOME/.local/bin:$HOME/.emacs.d/bin:/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin:${pkgs.custom-scripts}/bin:/opt/local/bin:/Users/benouattara/Qt/5.15.2/clang_64/bin

        if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi # required by nix to configure various paths

        export EDITOR=emacs
        export VISUAL=emacs
        export CPATH=/usr/local/include
      '';
  };

  programs.ssh = {
    enable = true;

    controlMaster  = "auto";
    controlPath    = "${tmp-directory}/ssh-%u-%r@%h:%p";
    controlPersist = "1800";

    forwardAgent = true;
    serverAliveInterval = 60;

    hashKnownHosts = true;

    extraConfig = ''
        user zangao
        Host *
          AddKeysToAgent yes
          StrictHostKeyChecking no
          IdentityFile ~/.ssh/id_rsa
      '';

    matchBlocks = {
      keychain = {
        host = "*";
        extraOptions = {
          "UseKeychain"    = "yes";
          "AddKeysToAgent" = "yes";
          "IgnoreUnknown"  = "UseKeychain";
        };
      };
    };
  };

  xdg = {
    enable = true;

    configHome = "${home-directory}/.config";
    dataHome   = "${home-directory}/.local/share";
    cacheHome  = "${home-directory}/.cache";

    configFile."fish/fish_variables".text = ''
      SETUVAR __fish_initialized:0
    '';

    configFile."mail/mbsyncrc".text = dotrcs.mbsync;
  };
}
