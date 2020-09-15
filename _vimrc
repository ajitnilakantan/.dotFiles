source $VIMRUNTIME/defaults.vim
" Store backup, undo, and swap files in temp directory
if has("win32")
  "Windows options here
  " :echom "Windows"
  set backupdir=c:\\.Backups\\\\
  set undodir=c:\\.Backups\\\\
  set directory=c:\\.Backups\\swapfiles\\\\
else
  if has("unix")
    let s:uname = system("uname")
    let s:unamev = system("uname -v")
    if s:unamev =~? ".*Microsoft.*"
        " Windows 10 / Linux
        set backupdir=/mnt/c/.Backups//
        set undodir=/mnt/c/.Backups//
        set directory=/mnt/c/.Backups/swapfiles//
        set directory=/mnt/c/.Backups//
        " :echom "Windows10 Linux"
    elseif s:uname == "Darwin\n"
        " Mac options here
        " :echom "MacOs"
        set mouse=v
        set backupdir=~/.Backups//,/tmp
        set undodir=~/.Backups//,/tmp
        set directory=~/.Backups/swapfiles//,/tmp
        set directory=~/.Backups//,/tmp
        " Automatically create .backup directory, writable by the group.
        if filewritable("~") && ! filewritable("~/.Backups")
          silent execute '!umask 002; mkdir ~/.Backups'
        endif
    endif
  endif
endif

" Default font
if has("gui_running")
    set guifont=Consolas:h16
endif

" Helper function
function! s:mycapture(excmd) abort
  try
    redir => out
    exe 'silent! '.a:excmd
  finally
    redir END
  endtry
  return out
endfunction

" :scriptnames
let s:mycurpath = fnamemodify(strpart(split(s:mycapture('scriptnames'), "\n")[-1], 5), ':p:h')
" :echom s:mycurpath
" let s:myfile = findfile('.vimrc.new', s:mycurpath)
" :echom s:myfile

" See: https://www.tutorialdocs.com/article/vim-configuration.html
" Basic 
set nocompatible
syntax on
set showmode
" set mouse=a
set mouse=nv
set mouse=v
set clipboard=unnamed
set encoding=utf-8
set t_Co=256
filetype indent on

" Indentation
set autoindent
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set backspace=indent,eol,start
:autocmd FileType text setlocal nocindent noautoindent nospell

" Shift-Tab to Outdent
" for command mode
nnoremap <S-Tab> <<
" for insert mode
inoremap <S-Tab> <C-d>

" Appearance
set laststatus=2
set ruler

set statusline=
set statusline+=%7*\[%n]                                  "buffernr
set statusline+=%1*\ %<%F\                                "File+path
set statusline+=%2*\ %y\                                  "FileType
set statusline+=%3*\ %{''.(&fenc!=''?&fenc:&enc).''}      "Encoding
set statusline+=%3*\ %{(&bomb?\",BOM\":\"\")}\            "Encoding2
set statusline+=%4*\ %{&ff}\                              "FileFormat (dos/unix..) 
set statusline+=%5*\ %{&spelllang}\%{HighlightSearch()}\  "Spellanguage & Highlight on?
set statusline+=%8*\ %=\ row:%l/%L\ (%03p%%)\             "Rownumber/total (%)
set statusline+=%9*\ col:%03c\                            "Colnr
set statusline+=%0*\ \ %m%r%w\ %P\ \                      "Modified? Readonly? Top/bot.

" Show character count
inoremap ;gg           G$g<C-G>''

function! HighlightSearch()
  if &hls
    return 'H'
  else
    return ''
  endif
endfunction

" Search
set showmatch
set matchtime=20
set hlsearch
set incsearch
set ignorecase
set smartcase
set cpoptions+=x
"This unsets the "last search pattern" register by hitting return
nnoremap <silent> <CR> :noh<CR><CR>
hi MatchParen cterm=bold ctermbg=none ctermfg=magenta

" Edit
set spell spelllang=en_us
set nospell
set undofile
set autochdir
set visualbell
set history=1000
set autoread
set wildmenu
set wildmode=longest:list,full
set whichwrap=b

" colorscheme
set termguicolors
colorscheme darkblue

let s:mypath = fnamemodify(resolve(expand('<sfile>:p')), ':h')
:execute 'source '.s:mypath.'/vim/autoload/plug.vim'
""" Run :PlugInstall 
call plug#begin(expand('~/.vim/plugged'))
Plug 'arcticicestudio/nord-vim'
call plug#end()
"colorscheme nord



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MULTIPURPOSE TAB KEY
" Indent if we're at the beginning of a line. Else, do completion.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <expr> <tab> InsertTabWrapper()
inoremap <s-tab> <c-n>

" Grep: https://thoughtbot.com/blog/faster-grepping-in-vim
if executable('rg')
  " Use rg over grep
  set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
endif
" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" Tabs: https://vim.fandom.com/wiki/Open_every_buffer_in_its_own_tabpage
nnoremap <C-Left> :tabprevious<CR>
nnoremap <C-Right> :tabnext<CR>

" Rainbow parentheses
""" Run :PlugInstall 
call plug#begin(expand('~/.vim/plugged'))
Plug 'luochen1990/rainbow'
call plug#end()
let g:rainbow_active = 1 "set to 0 if you want to enable it later via :RainbowToggle
